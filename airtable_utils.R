cargar_paquetes <- function(paquetes) {
  for (pkg in paquetes) {
    if (!pkg %in% loadedNamespaces()) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    }
  }
}

paquetes <- c("httr2","dplyr","jsonlite","purrr")
cargar_paquetes(paquetes)


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
  getrecordlink <- URLencode(paste0("https://api.airtable.com/v0/",base_id,"/",
                                    tablename,"/",recordid))
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
      email_error(last_response()$status_code,"airtable_getrecorddata_byid",origen,"~/api_logs_documentation.sqlite")
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

airtable_record_delete <- function(recordid, tablename, base_id,origen="",con=NULL){
  getrecordlink <- URLencode(paste0("https://api.airtable.com/v0/",base_id,"/",
                                    tablename,"/",recordid))
  try(
    atbrequest <- request(getrecordlink) %>% 
      req_method("DELETE") %>% 
      req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
      req_headers("Content-Type" = "application/json") %>% 
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  )
  
  #-------tryCatch
  if(!is.null(con)){
    guardar(origen, last_response(), last_request(), con, "airtable_record_delete", "api_logs")
  }
  if(last_response()$status_code %in% c(199:299)){
    #content(atbrequest)
    resp_body_json(atbrequest)
  }else{
    if(!is.null(con)){
      email_error(last_response()$status_code,"airtable_record_delete",origen,"~/api_logs_documentation.sqlite")
    }
    return(NULL)
  } 
}

airtable_tibblewithfields <- function(recordlist){
  lapply(recordlist, function(x) {
    campos <- x$fields
    campos <- lapply(campos, function(value) {
      if (length(value) == 0) return(NA)  
      if (length(value) > 1) return(list(value))
      return(value)
    })
    return(campos)}) %>% 
    bind_rows() %>% 
    mutate(across(where(is.list), ~ sapply(., function(x) {
      if (is.null(x)) return("")
      else return(jsonlite::toJSON(x, auto_unbox = TRUE))
    })))
}

airtable_subir_pdf <- function(record,ruta_pdf,columna,base_id,tipo){
  archivo_base64 <- base64enc::base64encode(ruta_pdf)
  fields <- list(
    contentType = paste0("application/",tipo),  # Tipo de contenido del archivo
    file = archivo_base64,            # El archivo codificado en base64
    filename = basename(ruta_pdf)  # Nombre del archivo
  )
  requrl <- paste0("https://content.airtable.com/v0/",base_id,"/",record,"/",columna,"/uploadAttachment")
  response <- request(requrl) %>%
    req_method("POST") %>% 
    req_headers(
      Authorization = paste0("Bearer ", Sys.getenv("AIRTABLE_API_KEY"))
    ) %>%
    req_body_json(fields) %>% 
    req_perform()
}

airtable_estructura <- function(base_id){
  url <- paste0("https://api.airtable.com/v0/meta/bases/",base_id,"/tables")
  res <- request(url) %>% 
    req_method("GET") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_headers("Content-Type" = "application/json") %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
}

delete_airtable_records <- function(api_key, base_id, table_name, record_ids) {
  base_url <- paste0("https://api.airtable.com/v0/", base_id, "/", URLencode(table_name))
  
  url <- base_url
  for (id in record_ids) {
    url <- paste0(url, ifelse(grepl("\\?", url), "&", "?"), "records[]=", id)
  }
  
  # Hacer la solicitud DELETE
  resp <- request(url) %>%
    req_method("DELETE") %>%
    req_headers(Authorization = paste("Bearer", api_key)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  # Verificar respuesta
  if(!last_response()$status_code %in% c(199:299)){
    mensaje <- paste0("No se pudieron eliminar estos records: \n", toJSON(record_ids),
                      "\n", toJSON(last_response() %>% resp_body_json()))
    enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
  }
   
  
  return(resp)
}

airtable_to_table <- function(lista) {
  registros <- lapply(lista, function(reg) {
    campos <- reg$fields
    campos$id <- reg$id
    campos <- lapply(campos, function(x) {
      if (is.list(x)) toJSON(x) else x
    })
    as.data.frame(campos, stringsAsFactors = FALSE)
  })
  
  # Rellena columnas faltantes con NA
  todas_columnas <- unique(unlist(lapply(registros, names)))
  registros_completos <- lapply(registros, function(df) {
    faltan <- setdiff(todas_columnas, names(df))
    for (col in faltan) df[[col]] <- NA
    df[todas_columnas]
  })
  
  do.call(rbind, registros_completos)
}