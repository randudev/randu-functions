library(httr)
library(httr2)
library(dplyr)
if (!require("emayili")) {install.packages("emayili")}
if (!require("tidyverse")) {install.packages("tidyverse")}
library(tidyverse)
library(emayili)

airtable_getrecordslist <- function(tablename, base_id, formula="", fields="",origen=""){
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
  tryCatch(expr={
    airtable_getrecordslist_tibble <- readRDS("airtable_getrecordslist_tibble.RDS")
    airtable_getrecordslist_tibble <- add_row(airtable_getrecordslist_tibble,time=Sys.time(),rspns=list(last_response()),
                                              url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                                              status=last_response()$status_code,origenes=origen)
    saveRDS(airtable_getrecordslist_tibble,"airtable_getrecordslist_tibble.RDS")
  },
  error=function(er){
    airtable_getrecordslist_tibble <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                                             resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                                             origenes=origen)
    saveRDS(airtable_getrecordslist_tibble,"airtable_getrecordslist_tibble.RDS")
  }
  )
  airtable_getrecordslist_tibble <- tibble(time=Sys.time(),rspns=list(last_response()))
  #--------Guardar el status--------
  if(last_response()$status_code %in% c(199:299)){
  
    recordslist <- resp_body_json(atbreq)$records
  }else{
    #------trycatch para guardar el error------- 
    tryCatch(expr={
      erroresp <- readRDS("erroresp.RDS")
      erroresp <- add_row(erroresp,time=Sys.time(),rspns=list(last_response()),
                          url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                          status=last_response()$status_code,funcion="airtable_getrecordslist",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_getrecordslist",origen)
    },
    error=function(er){
      erroresp <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                         resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                         funcion="airtable_getrecordslist",origenes=origen)
      
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_getrecordslist",origen)
    }
    )
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
    tryCatch(expr={
      airtable_getrecordslist_tibble <- readRDS("airtable_getrecordslist_tibble.RDS")
      airtable_getrecordslist_tibble <- add_row(airtable_getrecordslist_tibble,time=Sys.time(),rspns=list(last_response()),
                                                url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                                                status=last_response()$status_code,origenes=origen)
      saveRDS(airtable_getrecordslist_tibble,"airtable_getrecordslist_tibble.RDS")
    },
    error=function(er){
      airtable_getrecordslist_tibble <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                                               resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                                               origenes=origen)
      saveRDS(airtable_getrecordslist_tibble,"airtable_getrecordslist_tibble.RDS")
    }
    )
    recordslist <- append(recordslist, resp_body_json(atbreq)$records)
    atboffset <- resp_body_json(atbreq)$offset
    Sys.sleep(1)
    if(last_response()$status_code %in% c(199:299)){
      
      recordslist <- resp_body_json(atbreq)$records
    }else{
      #------trycatch para guardar el error------- 
      tryCatch(expr={
        erroresp <- readRDS("erroresp.RDS")
        erroresp <- add_row(erroresp,time=Sys.time(),rspns=list(last_response()),
                            url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                            status=last_response()$status_code,funcion="airtable_getrecordslist",origenes=origen)
        saveRDS(erroresp,"erroresp.RDS")
        email_error(last_response()$status_code)
      },
      error=function(er){
        erroresp <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                           resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                           funcion="airtable_getrecordslist",origenes=origen)
      
        saveRDS(erroresp,"erroresp.RDS")
        email_error(last_response()$status_code)
      }
      )
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

airtable_getrecorddata_byid <- function(recordid, tablename, base_id,origen=""){
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
  tryCatch(
    expr={
      airtable_getrecorddata_byid_tibble <- readRDS("airtable_getrecorddata_byid_tibble.RDS")
      airtable_getrecorddata_byid_tibble <- add_row(airtable_getrecorddata_byid_tibble,time=Sys.time(),rspns=list(last_response()),
                                            url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                                            status=last_response()$status_code,origenes=origen)

      saveRDS(airtable_getrecorddata_byid_tibble,"airtable_getrecorddata_byid_tibble.RDS")
  
    },error=function(er){
            airtable_getrecorddata_byid_tibble <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                                           resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                                           origenes=origen)
             saveRDS(airtable_getrecorddata_byid_tibble,"airtable_getrecorddata_byid_tibble.RDS")
  })
  
  if(last_response()$status_code %in% c(199:299)){
    #content(atbrequest)
    resp_body_json(atbrequest)
  }else{
    tryCatch(expr={
      erroresp <- readRDS("erroresp.RDS")
      erroresp <- add_row(erroresp,time=Sys.time(),rspns=list(last_response()),
                                                    url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                                                    status=last_response()$status_code,funcion="airtable_getrecorddata_byid",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_getrecorddata_byid",origen)
    },
    error=function(er){
      erroresp <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                                                    resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                                                    funcion="airtable_getrecorddata_byid",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_getrecorddata_byid",origen)
    }
    )
    return(NULL)
  } 
}

airtable_createrecord <- function(fieldslist, tablename, base_id,origen=""){
  requrl <- paste0("https://api.airtable.com/v0/",base_id,"/",tablename)
  try(
  atbreq <- request(requrl) %>% 
    req_method("POST") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_body_json(list(fields=fieldslist)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform())
  tryCatch(expr={
    airtable_createrecord_tibble <- readRDS("airtable_createrecord_tibble.RDS")
    airtable_createrecord_tibble <- add_row(airtable_createrecord_tibble,time=Sys.time(),rspns=list(last_response()),
                                              url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                                              status=last_response()$status_code,origenes=origen)
    saveRDS(airtable_createrecord_tibble,"airtable_createrecord_tibble.RDS")
  },
  error=function(er){
    airtable_createrecord_tibble <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                                             resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                                             origenes=origen)
    saveRDS(airtable_createrecord_tibble,"airtable_createrecord_tibble.RDS")
  }
  )
  if(last_response()$status_code %in% c(199:299)){
    resp_body_json(atbreq)
  }else{
    tryCatch(expr={
      erroresp <- readRDS("erroresp.RDS")
      erroresp <- add_row(erroresp,time=Sys.time(),rspns=list(last_response()),
                          url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                          status=last_response()$status_code,funcion="airtable_createrecord",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_createrecord",origen)
    },
    error=function(er){
      erroresp <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                         resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                         funcion="airtable_createrecord",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_createrecord",origen)
    }
    )
    
    return(NULL)
  } 
}

airtable_updatesinglerecord <- function(fieldslist, tablename, base_id, recordid,origen=""){
  requrl <- paste0("https://api.airtable.com/v0/",base_id,"/",tablename,"/",recordid)
  try(
  atbreq <- request(requrl) %>% 
    req_method("PATCH") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_body_json(list(fields=fieldslist)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
  )
  tryCatch(expr={
    airtable_updatesinglerecord_tibble <- readRDS("airtable_updatesinglerecord_tibble.RDS")
    airtable_updatesinglerecord_tibble <- add_row(airtable_updatesinglerecord_tibble,time=Sys.time(),rspns=list(last_response()),
                                              url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                                              status=last_response()$status_code,origenes=origen)
    
    saveRDS(airtable_updatesinglerecord_tibble,"airtable_updatesinglerecord_tibble.RDS")
  },
  error=function(er){
    airtable_updatesinglerecord_tibble <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                                             resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                                             origenes=origen)
    saveRDS(airtable_updatesinglerecord_tibble,"airtable_updatesinglerecord_tibble.RDS")
  }
  )
  if(last_response()$status_code %in% c(199:299)){
    resp_body_json(atbreq)
  }else{
    tryCatch(expr={
      erroresp <- readRDS("erroresp.RDS")
      erroresp <- add_row(erroresp,time=Sys.time(),rspns=list(last_response()),
                          url=last_response()$url,resp_body=list(last_response() %>% resp_body_json()),
                          status=last_response()$status_code,funcion="airtable_updatesinglerecord",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_updatesinglerecord",origen)
    },
    error=function(er){
      erroresp <- tibble(time=Sys.time(),rspns=list(last_response()),url=last_response()$url,
                         resp_body=list(last_response() %>% resp_body_json()),status=last_response()$status_code,
                         funcion="airtable_updatesinglerecord",origenes=origen)
      saveRDS(erroresp,"erroresp.RDS")
      email_error(last_response()$status_code,"airtable_updatesinglerecord",origen)
    }
    )
    return(NULL)
  } 
}

airtable_tibblewithfields <- function(recordlist){
  lapply(recordlist, pluck, 'fields') %>% bind_rows()
}

#-----Enviar por correo los errores------
email_error <- function(status,funcion,origen){
  

  email <- envelope() %>%
    from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
    to(Sys.getenv("EMAIL_ERROR_FAST_MAIL") ) %>%
    subject(paste0("Error: ",uuid::UUIDgenerate())) %>%
    text(paste0("¡Tuvimos un error ", status,"! Con en la funcion: ",funcion, ", y origen: ",origen, ". Adjunto archivo")) %>%
    attachment(path = "~/erroresp.RDS") # Ruta al archivo a adjuntar
  
  smtp <- server(
    host = "smtp.fastmail.com",
    port = 465,
    username = Sys.getenv("EMAIL_FAST_MAIL"),
    password = Sys.getenv("EMAIL_KEY"),
    use_ssl = TRUE
  )
  
  smtp(email)
  
}
