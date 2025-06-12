library(httr2)
library(lubridate)
library(dotenv)

#source(airtableutils.R)
#dotenv::load_dot_env('production.env')


ezeep_getactivetoken <- function(){
  atb_dev_base <- Sys.getenv('AIRTABLE_DEV_BASE')
  clientidsec_ezeep_b64 <- Sys.getenv('EZEEP_CLID_B64')
  
  tkdata <- airtable_getrecorddata_byid("recGnx94XcVEgnpVZ", "tokens", atb_dev_base)
  tkdata <- tkdata$fields
  token_expires <- lubridate::as_datetime(tkdata$token_expires)
  refresh_token <- tkdata$refresh_token
  access_token <- tkdata$access_token
  
  
  if(Sys.time()<token_expires){
    validat <- access_token
  }else{
    request("https://account.ezeep.com/oauth/access_token/") %>% 
      req_method("POST") %>% 
      req_headers('Content-type'='application/x-www-form-urlencoded') %>% 
      req_headers('Authorization'=paste0('Basic ',clientidsec_ezeep_b64)) %>% 
      req_body_form("grant_type" = 'refresh_token',
                    "scope" = 'printing',
                    "refresh_token" = refresh_token) %>% 
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
    if(!last_response()$status_code %in% c(199:299)){
      tryCatch(
        expr = {
          mensaje <- paste0("No se pudo actualizar el token por: ", last_response() %>% resp_body_string())
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
        },error =function(e){
          mensaje <- paste0("No se pudo actualizar el token y ocurrio un error", e)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
        }
      )
    }
    
    newtokendata <- last_response() %>% resp_body_json()
    newaccess_token <- newtokendata$access_token
    newrefreshtoken <- newtokendata$refresh_token
    newexpire <- last_response() %>% resp_date() + 3600 
    
    airtable_updatesinglerecord(fieldslist = list('access_token'=newaccess_token,
                                                  'refresh_token'=newrefreshtoken,
                                                  'token_expires'=newexpire), 
                                tablename = "tokens", base_id = atb_dev_base,
                                recordid = "recGnx94XcVEgnpVZ")
    validat <- newaccess_token
  }
  validat
}

ezeep_printbyurl <- function(urltoprint, ezeep_at, printername, copies=1,rango=NULL){
  switch(printername,
         'impresora_rosa'={
           #printer_id <- "1f0763ce-3112-4c5d-929c-800354d6ca83"
           printer_id <- "2c34289c-5304-4c43-a66b-cadea20fc2e2"
           paper_id <- 257
           paper_name <- "4x2"
           orientation <- 2
         },
         'impresora_azul'={
           #printer_id <- "e2e15ccf-7674-475b-9a86-9f1e5b1eb495"
           #printer_id <- "f7abe3c0-0623-43e3-bda9-694d69df71dc"
           #printer_id <- "d8e483c8-fc68-4fc5-a8ab-dc6699704797"
           printer_id <- "7843711d-3674-496b-a245-4bea92bdd3f0"
           paper_id <- 257
           paper_name <- "mediana"
           orientation <- 2
         },
         'impresora_paqueteria'={
           printer_id <- "6dab9519-b26a-46a3-8b4c-fa8781a68d63"
           paper_id <- 257
           paper_name <- "4x8"
           orientation <- 1
         },
         'impresora_brother'={
           #printer_id <- "6dab9519-b26a-46a3-8b4c-fa8781a68d63"
           #printer_id <- "1ea10e2d-9264-4392-90be-2a8a09bb3f3d"
           #printer_id <- "3e574e62-2e3c-4845-b6b8-f8da1582bf17"
           printer_id <- "ae8f8915-92da-4aa3-b097-c408ff74c3cf"
           paper_id <- 257
           paper_name <- "4x8"
           orientation <- 1
         }
         )
  
  jsonbody <- paste0('{
    "fileurl": "',urltoprint,'",
    "printerid": "',printer_id,'",
    "type": "pdf",
    "properties": {
        "color": false,
        "copies": ',copies,',
        "duplex": false,
        "duplexmode": 1,
        "orientation": ',orientation,',
        "paper": "',paper_name,'",
        "paperid": ',paper_id,',
        "resolution": "Auto"
    },
    "printanddelete": true
}')
  body <- fromJSON(jsonbody)
  if(!is.null(rango)){
    body$properties <- append(body$properties,list("pageRanges"=rango))
  }
  request("https://printapi.ezeep.com/sfapi/Print/") %>% 
    req_method("POST") %>% 
    # req_headers('Content-type'='application/x-www-form-urlencoded') %>% 
    req_headers('Content-type'='application/json') %>% 
    req_headers('Authorization'=paste0('Bearer ',ezeep_at)) %>% 
    req_body_json(body) %>% 
    req_perform() %>% 
    resp_body_json()
}
