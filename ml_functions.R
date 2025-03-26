library(lubridate)
library(httr2)

#source("~/airtable_utils.R")

get_active_token <- function(recordid=""){
  if(recordid==""){
    recordid <- "rec7opQaYsHnpRhgb" 
  }
  
  tkdata <- airtable_getrecorddata_byid(recordid, "tokens", Sys.getenv('AIRTABLE_DEV_BASE'))
  tkdata <- tkdata$fields
  token_expires <- lubridate::as_datetime(tkdata$token_expires)
  refresh_token <- tkdata$refresh_token
  access_token <- tkdata$access_token
  
  if(Sys.time()<token_expires){
    validat <- access_token
  }else{
    request("https://api.mercadolibre.com/oauth/token") %>% 
      req_method("POST") %>% 
      req_headers(accept= "application/json") %>% 
      req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
      req_body_json(list('grant_type'='refresh_token',
                         'client_id'=Sys.getenv('ML_APP_CLIENTID'),
                         'client_secret'=Sys.getenv('ML_APP_CLIENTSECRET'),
                         'refresh_token'=refresh_token)) %>% 
      req_perform()
    
    newtokendata <- last_response() %>% resp_body_json()
    newaccess_token <- newtokendata$access_token
    newrefreshtoken <- newtokendata$refresh_token
    newexpire <- last_response() %>% resp_date() + 21600 
    
    airtable_updatesinglerecord(fieldslist = list('access_token'=newaccess_token,
                                                  'refresh_token'=newrefreshtoken,
                                                  'token_expires'=newexpire), 
                                tablename = "tokens", base_id = Sys.getenv('AIRTABLE_DEV_BASE'),
                                recordid = "rec7opQaYsHnpRhgb")
    validat <- newaccess_token
  }
  validat
}

get_mlorder_byid <- function(orderid, mltoken){
  request(paste0("https://api.mercadolibre.com/orders/",orderid)) %>% 
    req_auth_bearer_token(mltoken) %>% 
    req_headers(accept= "application/json") %>% 
    req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
    req_perform() %>% 
    resp_body_json()
}

register_mlorder_in_airtable <- function(mlorder, ml_token,canal=NULL){
  lineitems_recordid <- register_lineitems_ml(mlorder, ml_token)
  #client_recordid <- register_client(shopifyorder)
  #shippingaddress_recordid <- register_address(shopifyorder)
  fieldslist <- list(
    'fecha'=mlorder$date_created,
    'canal_venta'='mercadolibrernd',
    'ventas_producto'=lineitems_recordid,
    'id_origen'=as.character(mlorder$id)
  )
  if(!is.null(canal)){
    fieldslist$canal_venta <- 'mercadolibreasm'
  }
  if(!is.null(mlorder$pack_id)){
    fieldslist <- append(fieldslist,list('ml_pack_id'=as.character(mlorder$pack_id)))
  }
  newov_content  <- airtable_createrecord(fieldslist, "ordenes_venta", Sys.getenv('AIRTABLE_CES_BASE'))
  if(!is.null(newov_content)){
    if(!is.null(mlorder$shipping$id)){
      register_shippingadd_ml_atb(mlorder, ml_token, newov_content$id)
    }
  }
}

#get_shipmentdetails
mlorder_checkfullfilment <- function(mlorder, ml_token){
  resp <- "order_without_shippingid"
  shipid <- mlorder$shipping$id
  if(!is.null(shipid)){
    request(paste0("https://api.mercadolibre.com/shipments/",shipid)) %>% 
      req_auth_bearer_token(ml_token) %>% 
      req_headers(accept= "application/json") %>% 
      req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
      req_perform()
    
    shipment_details <- last_response() %>% resp_body_json()
    resp <- shipment_details$logistic_type 
    if(is.null(resp)){
      resp <- "seller_fulfilled"
    }
  }
  resp
}

get_dir_mlorder <- function(mlorder,mltoken){
  shipmenturl <- paste0("https://api.mercadolibre.com/shipments/",
                        mlorder$shipping$id)
  
  mlshipment <- request(shipmenturl) %>%
    req_auth_bearer_token(mltoken) %>%
    req_headers(accept= "application/json") %>%
    req_headers("x-format-new"=TRUE) %>% 
    req_headers('content-type' = 'application/x-www-form-urlencoded') %>%
    req_perform() %>%
    resp_body_json()
  
  return(mlshipment)
}

register_shippingadd_ml_atb <- function(mlorder, mltoken, atb_recid_ovml){
  mlshipment <- get_dir_mlorder(mlorder,mltoken)
  fieldslist <- list(
    'nombre_destinatario'= mlshipment$destination$receiver_name,
    'telefono_destino' = mlshipment$destination$receiver_phone,
    'calle'= mlshipment$destination$shipping_address$address_line,
    'numero_exterior'= mlshipment$destination$shipping_address$street_number,
    'codigo_postal'= mlshipment$destination$shipping_address$zip_code,
    'colonia'= mlshipment$destination$shipping_address$neighborhood$name,
    'ciudad'= mlshipment$destination$shipping_address$city$name,
    'estado'= mlshipment$destination$shipping_address$state$name,
    'referencias'= mlshipment$destination$shipping_address$comment,
    'ordenes_venta'=list(atb_recid_ovml)
  )
  if(!is.null(mlshipment$logistic$type)){
    if(mlshipment$logistic$type != "fulfillment" ){
      airtable_createrecord(fieldslist,"direcciones",Sys.getenv('AIRTABLE_CES_BASE'))
    } 
  }else{
    if(!"splitted_order" %in% mlorder$tags){
      airtable_createrecord(fieldslist,"direcciones",Sys.getenv('AIRTABLE_CES_BASE'))
    }
  }
}

register_lineitems_ml <- function(mlorder, ml_token){
  vp_recordidlist <- vector(mode="list", 1)
  
  cantidad <- as.double(mlorder$order_items[[1]]$quantity)
  nombre_producto <- mlorder$order_items[[1]]$item$title
  precio <- as.double(mlorder$order_items[[1]]$unit_price)
  sku <- mlorder$order_items[[1]]$item$seller_sku
  comentarios <- ""
  fullfilment_type <- mlorder_checkfullfilment(mlorder, ml_token)
  if(fullfilment_type=="fulfillment"){
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'pendiente_envio'=0,
      'vp_revisada'=TRUE,
      'comentarios'=comentarios)
  }else{
    if("splitted_order" %in% mlorder$tags){
      fieldslist <- list(
        'cantidad'=cantidad,
        'helper_product_name'=nombre_producto,
        'precio_unitario'=0,
        'pendiente_envio'=0,
        'vp_revisada'=TRUE,
        'comentarios'=comentarios)
    }else{
      fieldslist <- list(
        'cantidad'=cantidad,
        'helper_product_name'=nombre_producto,
        'precio_unitario'=precio,
        'pendiente_envio'=cantidad,
        'comentarios'=comentarios)  
    }
  }
  
  if(!is.null(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
    product_recordid_list <- airtable_getrecordslist("productos",Sys.getenv('AIRTABLE_CES_BASE'), 
                                                     formula=paste0("sku=",sku))
    
    recordid_producto <- list(producto=list(product_recordid_list[[1]]$id))
    fieldslist <- append(fieldslist, recordid_producto)
  }
  newvp_content  <- airtable_createrecord(fieldslist, "ventas_producto", Sys.getenv('AIRTABLE_CES_BASE'))
  if(!is.null(newvp_content)){
    vp_recordidlist[[1]] <- newvp_content$id[[1]]
  }else{
    print(paste0("hubo un problema al registrar la el line_item 
                   (venta_producto #"))
  }
  vp_recordidlist
}

guia_agencia_pdf <- function(ml_token,id_shipping,ruta=NULL){
  url_envio <- paste0("https://api.mercadolibre.com/shipment_labels?shipment_ids=",id_shipping,"&response_type=pdf")
  response_envio <- request(url_envio) %>%
    req_auth_bearer_token(ml_token) %>%
    req_perform()
  if(!is.null(ruta)){
    writeBin(response_envio$body, ruta)
    return(ruta)
  }else{
    return(resp_body_raw(response_envio))
  }
}
