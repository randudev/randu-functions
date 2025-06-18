library(httr2)
library(jsonlite)
library(dotenv)
library(purrr)

supabase_createrecord <- function(fields, tablename="", base_id=""){
  tryCatch(
    expr = {
      if(base_id!=""){
        url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tablename)
      }else{
        url_supabase <- Sys.getenv("URL_SUPABASE_DEV")
      }
      
      #url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tablename)
      apikey <- Sys.getenv("AUTH_SUPABASE_DEV")
      resp_sup <- request(url_supabase) %>%
        req_method("POST") %>%
        req_headers('apikey'=apikey) %>%
        req_headers('Content-type'='application/json') %>%
        req_headers('Prefer'='return=minimal') %>%
        req_body_json(fields) %>%
        req_error(is_error = function(resp) FALSE) %>%
        req_perform()
    },
    error = function(e){
      print(e)
    }
  )
  
}

supabase_update <- function(id,fieldslist, tablename="", base_id=""){
  if(base_id!=""){
    url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tablename)
  }else{
    url_supabase <- Sys.getenv("URL_SUPABASE_DEV")
  }
  ids <- paste0("(", paste(id, collapse = ","),")")
  url_supabase <- paste0(url_supabase,'?id=in.',ids)
 
  apikey <- Sys.getenv("AUTH_SUPABASE_DEV")
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

supabase_getrecordslist <- function(tabla="",base_id="",filters="",fields="") {
  apikey <- Sys.getenv("AUTH_SUPABASE_DEV")
  if(base_id!=""){
    url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tabla)
  }else{
    url_supabase <- Sys.getenv("URL_SUPABASE_DEV")
  }
  #url_supabase <- paste0("https://",base_id,".supabase.co/rest/v1/",tabla)
  all_data <- list()
  limit <- 500        
  offset <- 0         
  
  while (TRUE) {
    rango <- paste0(offset,"-",limit)

    pagina_url <- url_supabase
    if (length(filters) > 1 || filters !="") {
      for (i in seq_along(filters)) {
        filtro <- filters[[i]]
        if(i == 1){
          pagina_url <- paste0(pagina_url, "?", filtro)
        }else{
          pagina_url <- paste0(pagina_url, "&", filtro)
        }
        
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
      req_url_query(order = "id.asc") %>%
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

#Esta funcion se usa para cuando quieres hacer cambio en mas de 2000 filas
supabase_updates <- function(id,fieldslist, tablename="", base_id=""){
  if(length(id)>2000){
   
    n_parts <- ceiling(length(id)/2000) + 1
    
    sub_ids <- split(id, gl(n_parts, ceiling(length(id) / n_parts), length(id)))
   
   for(ids in sub_ids){
     supabase_update(unlist(ids),fieldslist, tablename, base_id)
   }
  }else{
    supabase_update(id,fieldslist, tablename, base_id)
  }
}