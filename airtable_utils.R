library(httr)
library(httr2)
library(dplyr)
if (!require("emayili")) {install.packages("emayili")}
if (!require("tidyr")) {install.packages("tidyr")}
library(tidyr)
library(emayili)

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
  tryCatch(expr={
    #airtable_getrecordslist_tibble1 <- readRDS("airtable_getrecordslist_tibble1.RDS")
    airtable_getrecordslist_tibble <-  tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                              status=last_response()$status_code,header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
                                              origenes=origen)
    insertar_fila(con,"airtable_getrecordslist",airtable_getrecordslist_tibble)
    #saveRDS(airtable_getrecordslist_tibble1,"airtable_getrecordslist_tibble1.RDS",compress = FALSE)
  },
  error=function(er){
    print(er)
    airtable_getrecordslist_tibble <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                             status=last_response()$status_code,header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
                                             origenes=origen)
    saveRDS(airtable_getrecordslist_tibble,"airtable_getrecordslist_tibble.RDS",compress = FALSE)
    print("Ocurrio un error en la conexion")
  }
  )
  }
  #--------Guardar el status--------
  if(last_response()$status_code %in% c(199:299)){
  
    recordslist <- resp_body_json(atbreq)$records
  }else{
    #------trycatch para guardar el error------- 
    if(!is.null(con)){
    tryCatch(expr={
      erroresp <- readRDS("erroresp.RDS")
      erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                         status=last_response()$status_code,header =toJSON(last_request()$headers),
                         request=toJSON(last_request()$body$data),funcion="airtable_getrecordslist",origenes=origen)
      insertar_fila_errores(con,erroresp)
      email_error(last_response()$status_code,"airtable_getrecordslist",origen,"~/airtable_functions.sqlite")
    },
    error=function(er){
      erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=list(last_response() %>% resp_body_json()),url=last_response()$url,
                         status=last_response()$status_code,header =list(last_request()$headers),
                         request=list(last_request()$body$data),funcion="airtable_getrecordslist",origenes=origen)
      saveRDS(erroresp1,"erroresp1.RDS",compress = FALSE)
      email_error(last_response()$status_code,"airtable_getrecordslist",origen,"~/erroresp.RDS")
      print("Hubo un error de conexion")
    }
    )}
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
    tryCatch(expr={
      #airtable_getrecordslist_tibble1 <- readRDS("airtable_getrecordslist_tibble1.RDS")
      #airtable_getrecordslist_tibble1 <- add_row(airtable_getrecordslist_tibble1,time=Sys.time(),rspns=list(last_response() %>% resp_body_json()),
      #                                          url=last_response()$url,status=last_response()$status_code,header =list(last_request()$headers),
      #                                          request=list(last_request()$body$data),origenes=origen)
      #saveRDS(airtable_getrecordslist_tibble1,"airtable_getrecordslist_tibble1.RDS",compress = FALSE)
      airtable_getrecordslist_tibble <-  tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                                status=last_response()$status_code,header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
                                                origenes=origen)
      insertar_fila(con,"airtable_getrecordslist",airtable_getrecordslist_tibble)
    },
    error=function(er){
      #airtable_getrecordslist_tibble1 <- tibble(time=Sys.time(),rspns=list(last_response() %>% resp_body_json()),url=last_response()$url,
      #                                         status=last_response()$status_code,header =list(last_request()$headers),
      #                                         request=list(last_request()$body$data),origenes=origen)
      #saveRDS(airtable_getrecordslist_tibble1,"airtable_getrecordslist_tibble1.RDS",compress = FALSE)
      airtable_getrecordslist_tibble <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                              status=last_response()$status_code,header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
                                              origenes=origen)
      saveRDS(airtable_getrecordslist_tibble,"airtable_getrecordslist_tibble.RDS",compress = FALSE)
      print("Ocurrio un error en la conexion")
    }
    )}
    recordslist <- append(recordslist, resp_body_json(atbreq)$records)
    atboffset <- resp_body_json(atbreq)$offset
    Sys.sleep(1)
    if(last_response()$status_code %in% c(199:299)){
      
      recordslist <- resp_body_json(atbreq)$records
    }else{
      #------trycatch para guardar el error------- 
      if(!is.null(con)){
      tryCatch(expr={
        erroresp <- readRDS("erroresp.RDS")
        erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                           status=last_response()$status_code,header =toJSON(last_request()$headers),
                           request=toJSON(last_request()$body$data),funcion="airtable_getrecordslist",origenes=origen)
        insertar_fila_errores(con,erroresp)
        email_error(last_response()$status_code,"airtable_getrecordslist",origen,"~/airtable_functions.sqlite")
      },
      error=function(er){
        erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=list(last_response() %>% resp_body_json()),url=last_response()$url,
                           status=last_response()$status_code,header =list(last_request()$headers),
                           request=list(last_request()$body$data),funcion="airtable_getrecordslist",origenes=origen)
        saveRDS(erroresp1,"erroresp1.RDS",compress = FALSE)
        email_error(last_response()$status_code,"airtable_getrecordslist",origen,"~/erroresp.RDS")
        print("Hubo un error de conexion")
      }
      )}
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
  tryCatch(
    expr={
      #airtable_getrecorddata_byid_tibble1 <- readRDS("airtable_getrecorddata_byid_tibble1.RDS")
      #airtable_getrecorddata_byid_tibble1 <- add_row(airtable_getrecorddata_byid_tibble1,time=Sys.time(),
      #                                               rspns=list(last_response() %>% resp_body_json()),
      #                                               url=last_response()$url,status=last_response()$status_code,
      #                                               header =list(last_request()$headers),request=list(last_request()$body$data),
      #                                               origenes=origen)
      #saveRDS(airtable_getrecorddata_byid_tibble1,"airtable_getrecorddata_byid_tibble1.RDS",compress = FALSE)
      airtable_getrecorddata_byid_tibble <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                                   status=last_response()$status_code, header =toJSON(last_request()$headers),
                                                   request=toJSON(last_request()$body$data),origenes=origen)
      insertar_fila(con,"airtable_getrecorddata_byid",airtable_getrecorddata_byid_tibble)
  
    },
    error=function(er){
      print(er)
            airtable_getrecorddata_byid_tibble <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                                         status=last_response()$status_code, header =toJSON(last_request()$headers),
                                                         request=toJSON(last_request()$body$data),origenes=origen)
            
             saveRDS(airtable_getrecorddata_byid_tibble,"airtable_getrecorddata_byid_tibble.RDS",compress = FALSE)
             print("Ocurrio un error en la conexion")
  })
  }
  if(last_response()$status_code %in% c(199:299)){
    #content(atbrequest)
    resp_body_json(atbrequest)
  }else{
    if(!is.null(con)){
      tryCatch(expr={
      #erroresp1 <- readRDS("erroresp1.RDS")
      erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                          status=last_response()$status_code,header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
                          funcion="airtable_getrecorddata_byid",origenes=origen)
      #saveRDS(erroresp1,"erroresp1.RDS",compress = FALSE)
      insertar_fila_errores(con,erroresp )
      email_error(last_response()$status_code,"airtable_getrecorddata_byid",origen,"~/airtable_functions.sqlite")
    },
      error=function(er){
      erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=list(last_response() %>% resp_body_json()),url=last_response()$url,
                         status=last_response()$status_code,header =list(last_request()$headers),request=list(last_request()$body$data),
                         funcion="airtable_getrecorddata_byid",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS",compress = FALSE)
      print("Hubo un error en la conexion")
      email_error(last_response()$status_code,"airtable_getrecorddata_byid",origen,"~/erroresp.RDS")
    }
      )
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
  tryCatch(expr={
    #airtable_createrecord_tibble1 <- readRDS("airtable_createrecord_tibble1.RDS")
    #airtable_createrecord_tibble1 <- add_row(airtable_createrecord_tibble,time=Sys.time(),rspns=list(last_response() %>% resp_body_json()),
    #                                        url=last_response()$url,status=last_response()$status_code,header =list(last_request()$headers),
    #                                        request=list(last_request()$body$data),origenes=origen)
    #saveRDS(airtable_createrecord_tibble1,"airtable_createrecord_tibble1.RDS",compress = FALSE)
    airtable_createrecord_tibble <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                            status=last_response()$status_code, header =toJSON(last_request()$headers),
                                            request=toJSON(last_request()$body$data),origenes=origen)
    insertar_fila(con,"airtable_createrecord",airtable_createrecord_tibble)
  },
  error=function(er){
    print(er)
    airtable_createrecord_tibble <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                                           status=last_response()$status_code, header =toJSON(last_request()$headers),
                                           request=toJSON(last_request()$body$data),origenes=origen)
    saveRDS(airtable_createrecord_tibble,"airtable_createrecord_tibble.RDS",compress = FALSE)
    print("Ocurrio un problema en la conexion")
  }
  )
  }
  if(last_response()$status_code %in% c(199:299)){
    resp_body_json(atbreq)
  }else{
    if(!is.null(con)){
    tryCatch(expr={
      #erroresp1 <- readRDS("erroresp1.RDS")
      erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                          status=last_response()$status_code,header =toJSON(last_request()$headers),
                          request=toJSON(last_request()$body$data),funcion="airtable_createrecord",origenes=origen)
      insertar_fila_errores(con,erroresp)
      #saveRDS(erroresp1,"erroresp1.RDS",compress = FALSE)
      email_error(last_response()$status_code,"airtable_createrecord",origen,"~/airtable_functions.sqlite")
    },
    error=function(er){
      erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                         status=last_response()$status_code,header =toJSON(last_request()$headers),
                         request=toJSON(last_request()$body$data),funcion="airtable_createrecord",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS",compress = FALSE)
      email_error(last_response()$status_code,"airtable_createrecord",origen,"~/erroresp.RDS")
      print("Hubo un error en la conexion")
    }
    )
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
    tryCatch(expr={
    #airtable_updatesinglerecord_tibble1 <- readRDS("airtable_updatesinglerecord_tibble1.RDS")
    #airtable_updatesinglerecord_tibble1 <- add_row(airtable_updatesinglerecord_tibble1,time=Sys.time(),rspns=list(last_response() %>% resp_body_json()),
    #                                          url=last_response()$url,status=last_response()$status_code,header =list(last_request()$headers),
    #                                          request=list(last_request()$body$data),origenes=origen)
    airtable_updatesinglerecord_tibble <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),
                                                 url=last_response()$url, status=last_response()$status_code,
                                                 header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
                                                 origenes=origen)
    insertar_fila(con,"airtable_updatesinglerecord",airtable_updatesinglerecord_tibble)
    #saveRDS(airtable_updatesinglerecord_tibble1,"airtable_updatesinglerecord_tibble1.RDS",compress = FALSE)
  },
    error=function(er){
    print(er)
    airtable_updatesinglerecord_tibble1 <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),
                                                  url=last_response()$url,status=last_response()$status_code,
                                                  header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
                                                  origenes=origen)
    saveRDS(airtable_updatesinglerecord_tibble1,"airtable_updatesinglerecord_tibble1.RDS",compress = FALSE)
    print("Ocurrio un error al en la conexion")
  }
  )
  }
  if(last_response()$status_code %in% c(199:299)){
    resp_body_json(atbreq)
  }else{
    if(!is.null(con)){
      tryCatch(expr={
      ##erroresp1 <- readRDS("erroresp1.RDS")
      #erroresp1 <- add_row(erroresp1,time=Sys.time(),rspns=list(last_response() %>% resp_body_json()),
      #                    url=last_response()$url,status=last_response()$status_code,
      #                    header =list(last_request()$headers),request=list(last_request()$body$data),
      #                    funcion="airtable_updatesinglerecord",origenes=origen)
      #saveRDS(erroresp,"erroresp1.RDS",compress = FALSE)
      erroresp <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                         status=last_response()$status_code,header =toJSON(last_request()$headers),
                         request=toJSON(last_request()$body$data),funcion="airtable_updatesinglerecord",origenes=origen)
      insertar_fila_errores(con,erroresp)
      email_error(last_response()$status_code,"airtable_updatesinglerecord",origen,"~/airtable_functions.sqlite")
    },
      error=function(er){
      print(er)
      erroresp1 <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
                         status=last_response()$status_code,header =toJSON(last_request()$headers),
                         request=toJSON(last_request()$body$data),funcion="airtable_updatesinglerecord",origenes=origen)
      saveRDS(erroresp1,"erroresp1.RDS",compress = FALSE)
      email_error(last_response()$status_code,"airtable_updatesinglerecord",origen,"~/erroresp.RDS")
      print("Hubo un problema en la conexion error")
    }
      )
    }
    return(NULL)
  } 
}

airtable_tibblewithfields <- function(recordlist){
  lapply(recordlist, pluck, 'fields') %>% bind_rows()
}

#-----Enviar por correo los errores------
email_error <- function(status,funcion,origen,archivo){
  

  email <- envelope() %>%
    from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
    to(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
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
