library(httr2)
library(jsonlite)
library(dotenv)

supabase_createrecord <- function(fieldslist, tablename, base_id="",origen="",con=NULL){
  tryCatch(
    expr = {
      url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tablaname,'?id=eq.',id)
      apikey <- Sys.getenv("SUPABASE_API_KEY")
      resp_sup <- request(url_supabase) %>%
        req_method("POST") %>%
        req_headers('apikey'=apikey) %>%
        req_headers('Content-type'='application/json') %>%
        req_headers('Prefer'='return=minimal') %>%
        req_body_json(fileds) %>%
        req_error(is_error = function(resp) FALSE) %>%
        req_perform()
    },
    error = function(e){
      print(e)
    }
  )
  
}

supabase_update <- function(id,fieldslist, tablename, base_id){
  
  url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tablename,'?id=eq.',id)
  
  apikey <- Sys.getenv("SUPABASE_API_KEY")
  res<-request(url_supabase) %>% 
    req_method("PATCH") %>% 
    req_headers("apikey"=apikey,
                "Content-Type" = "application/json") %>%
    req_headers('Prefer'= 'return=merge-duplicates') %>% 
    req_body_json(fieldslist) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()  
  if(!last_response()$status_code %in% c(199:299)){
    print(paste0("La fila ",id," no se modifico exitosamente"))
  }else{
    return(NULL)
  } 
}

supabase_getrecordslist <- function(tabla,base_id,filters="",fields="") {
  apikey <- Sys.getenv("SUPABASE_API_KEY")
  url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tabla)
  all_data <- list()
  limit <- 500        
  offset <- 0         
  
  while (TRUE) {
    rango <- paste0(offset,"-",limit)

    pagina_url <- url_supabase
    if (filters !="") {
      for (filtro in filters) {
        pagina_url <- paste0(pagina_url, "?", filtro)
      }
    }
  
    if (fields!="") {
      pagina_url <- paste0(pagina_url, "&select=", paste(fields, collapse = ","))
    }
    response <- request(pagina_url) %>%
      req_method("GET") %>% 
      req_headers('apikey'=apikey) %>%
      req_headers("Authorization" = paste0("Bearer ", apikey)) %>% 
      req_headers("Range"= rango) %>% 
      req_headers("Content-Type" = "application/json") %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
    
    if (response$status_code == 200) {
      data <- response %>% resp_body_json()
      
      if (length(data) > 0) {
        all_data <- append(all_data, data)
      }
      if (length(data) < 500) {
        break  
      }
      offset <- limit + 1
      limit <- limit + 500
    } else {
      print(paste0("Error: ", response$status_code))
      break
    }
    data <- list()
  }
  return(all_data)
}

dar_procesadas <- function(){
  base_id <- Sys.getenv("SUPABASE_URL_ID")
  
  url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/","notificaciones",'?procesada=eq.',FALSE,"&body=in.order_v2")
  
  apikey <- Sys.getenv("SUPABASE_API_KEY")
  res<-request(url_supabase) %>% 
    req_method("PATCH") %>% 
    req_headers("apikey"=apikey,
                "Content-Type" = "application/json") %>%
    req_headers('Prefer'= 'return=merge-duplicates') %>% 
    req_body_json(list("procesada"=TRUE )) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()  
  
}
