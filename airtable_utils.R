library(httr)
library(httr2)
library(dplyr)

airtable_getrecordslist <- function(tablename, base_id, formula="", fields=""){
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
    req_perform())
  if(atbreq$status_code==200){
    recordslist <- resp_body_json(atbreq)$records
  }else{
    return(e)
  }
  atboffset <- resp_body_json(atbreq)$offset
  while(!is.null(atboffset)){
    requrl <- URLencode(paste0("https://api.airtable.com/v0/",base_id,"/",
                               tablename,"?offset=",atboffset))
    atbreq <- request(requrl) %>% 
      req_method("GET") %>% 
      req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
      req_perform()
    recordslist <- append(recordslist, resp_body_json(atbreq)$records)
    atboffset <- resp_body_json(atbreq)$offset
    Sys.sleep(1)
  }
  recordslist
}

airtable_getrecorddata_byid <- function(recordid, tablename, base_id){
  Sys.getenv("AIRTABLE_API_KEY")
  getrecordlink <- URLencode(paste0("https://api.airtable.com/v0/",base_id,"/",
                                    tablename,"/",recordid))
  atbrequest <- GET(getrecordlink, 
                    add_headers(Authorization = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY")),
                                "Content-Type" = "application/json"))
  
  if(atbrequest$status_code==200){
    content(atbrequest)
  }else{
    return(NULL)
  } 
}

airtable_createrecord <- function(fieldslist, tablename, base_id){
  requrl <- paste0("https://api.airtable.com/v0/",base_id,"/",tablename)
  atbreq <- request(requrl) %>% 
    req_method("POST") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_body_json(list(fields=fieldslist)) %>% 
    req_perform()
  if(atbreq$status_code==200){
    resp_body_json(atbreq)
  }else{
    # print(resp_body_json(atbreq))
    # print(resp_headers(atbreq))
    return(NULL)
  } 
}

airtable_updatesinglerecord <- function(fieldslist, tablename, base_id, recordid){
  requrl <- paste0("https://api.airtable.com/v0/",base_id,"/",tablename,"/",recordid)
  atbreq <- request(requrl) %>% 
    req_method("PATCH") %>% 
    req_headers('Authorization' = paste0("Bearer ",Sys.getenv("AIRTABLE_API_KEY"))) %>% 
    req_body_json(list(fields=fieldslist)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
  if(atbreq$status_code==200){
    resp_body_json(atbreq)
  }else{
    print("Hubo un error", resp_body_json())
    return(NULL)
  } 
}

airtable_tibblewithfields <- function(recordlist){
  lapply(recordlist, pluck, 'fields') %>% bind_rows()
}