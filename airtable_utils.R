library(httr)
library(httr2)
library(dplyr)
if (!require("emayili")) {install.packages("emayili")}
if (!require("tidyr")) {install.packages("tidyr")}
library(tidyr)
library(emayili)
#Asi se debe de llamar el archivo "api_logs_documentation.sqlite"

airtable_getrecordslist <- function(tablename, base_id, formula="", fields="",origen="",con=NULL){
  recordslist <- vector(mode="list", 0)
  requrl <- paste0("https://api.airtable.com/v0/",base_id,"/",
                             tablename, "?pageSize=100")
  
  if(fields[1]!=""){
    for(i in 1:length(fields)){
      requrl <- paste0(requrl,"&fields[]=",fields[i])
    }
  }
  
  if(formula!=""){
    requrl <- paste0(requrl,"&filterByFormula=",formula)
  }
  
  requrl <- URLencode(requrl)
  #falta poner un while para el retry
  try( atbreq <- request(requrl) %>% 
    req_method("GET") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform())

  #----tryCatch para el guardado de el response 
  if(!is.null(con)){
    guardar(origen, last_response(), last_request(), con, "airtable_getrecordslist", "api_logs")
    
  }
  #--------Guardar el status--------
  if(last_response()$status_code %in% c(199:299)){
  
    recordslist <- resp_body_json(atbreq)$records
  }else{
    #------trycatch para guardar el error------- 
    if(!is.null(con)){
      email_error(last_response()$status_code,"airtable_getrecordslist",origen,"~/api_logs_documentation.sqlite")
    }
    return(NULL)
  }
  atboffset <- resp_body_json(atbreq)$offset
  while(!is.null(atboffset)){
    requrl <- URLencode(paste0("https://api.airtable.com/v0/",base_id,"/",
                               tablename,"?offset=",atboffset))
    atbreq <- request(requrl) %>% 
      req_method("GET") %>% 
      req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
    #------Revisar 
    if(!is.null(con)){
      guardar(origen, last_response(), last_request(), con, "airtable_getrecordslist", "api_logs")
    }
    
    atboffset <- resp_body_json(atbreq)$offset
    Sys.sleep(1)
    if(last_response()$status_code %in% c(199:299)){
      recordslist <- append(recordslist, resp_body_json(atbreq)$records)
    }else{
      #------trycatch para guardar el error------- 
      if(!is.null(con)){
        email_error(last_response()$status_code,"airtable_getrecordslist",origen,"~/api_logs_documentation.sqlite")
        
      }
      if(length(recordslist)>0){
        return(recordslist)
      }
      else{
      return(NULL)
      }
    }
  }
  recordslist
}

airtable_getrecorddata_byid <- function(recordid, tablename, base_id,origen="",con=NULL){
  Sys.getenv("AIRTABLE_API_KEY")
  getrecordlink <- URLencode(paste0("https://api.airtable.com/v0/",base_id,"/",
                                    tablename,"/",recordid))
  #atbrequest <- GET(getrecordlink, 
   #                 add_headers(Authorization = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY")),
    #                            "Content-Type" = "application/json"))
  try(
    atbrequest <- request(getrecordlink) %>% 
      req_method("GET") %>% 
      req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
      req_headers("Content-Type" = "application/json") %>% 
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  )
  
  #-------tryCatch
  if(!is.null(con)){
    guardar(origen, last_response(), last_request(), con, "airtable_getrecorddata_byid", "api_logs")
  }
  if(last_response()$status_code %in% c(199:299)){
    #content(atbrequest)
    resp_body_json(atbrequest)
  }else{
    if(!is.null(con)){
      email_error(last_response()$status_code,"airtable_updatesinglerecord",origen,"~/api_logs_documentation.sqlite")
    }
    return(NULL)
  } 
}

airtable_createrecord <- function(fieldslist, tablename, base_id,origen="",con=NULL){
  requrl <- paste0("https://api.airtable.com/v0/",base_id,"/",tablename)
  try(
  atbreq <- request(requrl) %>% 
    req_method("POST") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_body_json(list(fields=fieldslist)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform())
  if(!is.null(con)){
    guardar(origen, last_response(), last_request(), con, "airtable_createrecord", "api_logs")
  }
  if(last_response()$status_code %in% c(199:299)){
    resp_body_json(atbreq)
  }else{
    if(!is.null(con)){
      email_error(last_response()$status_code,"airtable_createrecord",origen,"~/api_logs_documentation.sqlite")
    }
    return(NULL)
  } 
}

airtable_updatesinglerecord <- function(fieldslist, tablename, base_id, recordid,origen="",con=NULL){
  requrl <- paste0("https://api.airtable.com/v0/",base_id,"/",tablename,"/",recordid)
  try(
  atbreq <- request(requrl) %>% 
    req_method("PATCH") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_body_json(list(fields=fieldslist)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
  )
  if(!is.null(con)){
    guardar(origen, last_response(), last_request(), con, "airtable_updatesinglerecord", "api_logs")
  }
  if(last_response()$status_code %in% c(199:299)){
    resp_body_json(atbreq)
  }else{
    if(!is.null(con)){
      #guardar(origen,last_response(),last_request(),con,"airtable_updatesinglerecord","api_logs")
      email_error(last_response()$status_code,"airtable_updatesinglerecord",origen,"~/api_logs_documentation.sqlite")
    }
    return(NULL)
  } 
}

airtable_tibblewithfields <- function(recordlist){
  lapply(recordlist, pluck, 'fields') %>% bind_rows()
}

#-----Enviar por correo los errores------
email_error <- function(status,funcion,origen,archivo=""){
  

  email <- envelope() %>%
    from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
    to(Sys.getenv("EMAIL_ERROR_FAST_MAIL") ) %>%
    subject(paste0("Error: ",uuid::UUIDgenerate())) %>%
    text(paste0("¡Tuvimos un error ", status,"! Con en la funcion: ",funcion, ", y origen: ",origen, ". Adjunto archivo")) %>%
    attachment(path = archivo) # Ruta al archivo a adjuntar
  
  smtp <- server(
    host = "smtp.fastmail.com",
    port = 465,
    username = Sys.getenv("EMAIL_FAST_MAIL"),
    password = Sys.getenv("EMAIL_KEY"),
    use_ssl = TRUE
  )
  
  smtp(email)
  
}
email_error_general <- function(mensaje,archivo=NULL){
  email <- envelope() %>%
    from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
    to(Sys.getenv("EMAIL_ERROR_FAST_MAIL") ) %>%
    subject(paste0("Error : ",uuid::UUIDgenerate())) %>%
    text(paste0("¡Tuvimos un problema: ", mensaje)) 
    
    if(!is.null(archivo)){
      email <- email %>% attachment(path = archivo) # Ruta al archivo a adjuntar
    }
    
  smtp <- server(
    host = "smtp.fastmail.com",
    port = 465,
    username = Sys.getenv("EMAIL_FAST_MAIL"),
    password = Sys.getenv("EMAIL_KEY"),
    use_ssl = TRUE
  )
  
  smtp(email)
}

guardar <- function(origen="", resp, req, con, func, tabla){
  tryCatch(expr={
    #airtable_updatesinglerecord_tibble1 <- readRDS("airtable_updatesinglerecord_tibble1.RDS")
    #airtable_updatesinglerecord_tibble1 <- add_row(airtable_updatesinglerecord_tibble1,time=Sys.time(),rspns=list(last_response() %>% resp_body_json()),
    #                                          url=last_response()$url,status=last_response()$status_code,header =list(last_request()$headers),
    #                                          request=list(last_request()$body$data),origenes=origen)
    logs <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(resp_body_json(resp)),
                  url=resp$url, status=resp$status_code, header =toJSON(req$headers),
                  request=toJSON(req$body$data),funcion=func, origenes=origen)
    insertar_fila(con, tabla, logs)
  },
  error=function(er){
    print(er)
    tabla <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(resp_body_json(resp)),
                     url=resp$url,status=resp$status_code,header =toJSON(req$headers),
                    request=toJSON(req$body$data), funciones=func,origenes=origen)
    saveRDS(tabla,"api_logs.RDS",compress = FALSE)
    print("Ocurrio un error al en la conexion")
  })
}
#api_logs_documentation
