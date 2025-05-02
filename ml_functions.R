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
    newexpire <- last_response() %>% resp_date() + 21300 
    
    airtable_updatesinglerecord(fieldslist = list('access_token'=newaccess_token,
                                                  'refresh_token'=newrefreshtoken,
                                                  'token_expires'=newexpire), 
                                tablename = "tokens", base_id = Sys.getenv('AIRTABLE_DEV_BASE'),
                                recordid = recordid)
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
  if(!is.null(mlorder$shipping$id)){
    fieldslist <- append(fieldslist,list('ml_id_envio'=paste0(mlorder$shipping$id)))
  }
  if("splitted_order" %in% mlorder$tags){
    ml_shipping <- get_dir_mlorder(mlorder,ml_token)
    ov_padre <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("ml_id_envio='",ml_shipping$sibling$sibling_id,"'"))
    if(length(ov_padre)==1){
      fieldslist <- append(fieldslist,list("ov_padre"=list(ov_padre[[1]]$id)))
    }
    
  }
  if(!is.null(canal)){
    fieldslist$canal_venta <- 'mercadolibreasm'
  }
  if(!is.null(mlorder$pack_id)){
    fieldslist <- append(fieldslist,list('ml_pack_id'=as.character(mlorder$pack_id)))
  }
  newov_content  <- airtable_createrecord(fieldslist, "ordenes_venta", Sys.getenv('AIRTABLE_CES_BASE'))
  if(!is.null(newov_content)){
    if(!is.null(mlorder$shipping$id)){
      dir_id <- register_shippingadd_ml_atb(mlorder, ml_token, newov_content$id)
      register_client_ml(mlorder,ml_token,dir_id,newov_content$id)
    }
  }
  return(newov_content)
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
    if(mlshipment$logistic$type != "fulfillment"){
      recordid <- airtable_createrecord(fieldslist,"direcciones",Sys.getenv('AIRTABLE_CES_BASE'))
      return(recordid)
    } 
  }else{
    if(!"splitted_order" %in% mlorder$tags){
      recordid <- airtable_createrecord(fieldslist,"direcciones",Sys.getenv('AIRTABLE_CES_BASE'))
      return(recordid)
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
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'pendiente_envio'=cantidad,
      'comentarios'=comentarios)
    if("splitted_order" %in% mlorder$tags){
      ml_shipping <- get_dir_mlorder(mlorder,ml_token)
      ov_padre <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("ml_id_envio='",ml_shipping$sibling$sibling_id,"'"))
      if(length(ov_padre)==1){
        if(length(ov_padre[[1]]$fields$recibos)!=0){
          fieldslist <- list(
            'cantidad'=cantidad,
            'helper_product_name'=nombre_producto,
            'precio_unitario'=0,
            'pendiente_envio'=0,
            'vp_revisada'=TRUE,
            'comentarios'=comentarios)
        }
      }
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

guia_agencia_pdf <- function(ml_token,id_shipping,ruta=NULL,id_orden){
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

register_client_ml <- function(ml_order,ml_token,direccion,id_orden){
  client_id <- ml_order$buyer$id
  cliente <- airtable_getrecordslist("clientes",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_mercadolibre='",client_id,"'"))
  if(length(cliente)==0){
    fields <- list(
      'telefono_principal' = direccion$fields$telefono_destino,
      'nombre'=direccion$fields$nombre_destinatario,
      'apellido_paterno'=direccion$fields$apellido_destinatario,
      'ordenes_venta'=list(id_orden),
      'nickname_ml'=ml_order$buyer$nickname,
      'fuente_contacto'="ML",
      'id_mercadolibre'=paste0(ml_order$buyer$id),
      'direccion_principal'=list(direccion$id),
      'direcciones'=list(direccion$id)
    )
    airtable_createrecord(fields,"clientes",Sys.getenv("AIRTABLE_CES_BASE"))
  }else{
    fields <- list(
      'ordenes_venta'=append(cliente[[1]]$fields$ordenes_venta,id_orden),
      'direcciones'=append(cliente[[1]]$fields$direcciones,direccion$id),
      'direccion_principal'=list(direccion$id)
    )
    airtable_updatesinglerecord(fields,"clientes",Sys.getenv("AIRTABLE_CES_BASE"),cliente[[1]]$id)
    return(cliente[[1]]$id)
  }
  
}

ml_primer_mensaje <- function(ml_order,ml_token,mensaje){
  if(!is.null(ml_order$pack_id)){
    id <- ml_order$pack_id
  }else{
    id <- ml_order$id
  }
  resp_mensaje <- request(paste0("https://api.mercadolibre.com/messages/action_guide/packs/",id,"/option")) %>%
    req_method("POST") %>%
    req_headers('Authorization'=paste0("Bearer ",ml_token)) %>% 
    req_headers('Content-type'='application/json') %>%
    req_body_json(list('option_id'='SEND_INVOICE_LINK','text'=mensaje)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
}

responder_mensaje <- function(ml_order,ml_token,mensaje){
  if(!is.null(ml_order$pack_id)){
    id <- ml_order$pack_id
  }else{
    id <- ml_order$id
  }
  res_sup <- request(paste0("https://api.mercadolibre.com/messages/packs/",id,"/sellers/",ml_order$seller$id,"?tag=post_sale")) %>%
    req_method("POST") %>%
    req_headers('Authorization'=paste0("Bearer ",ml_token)) %>% 
    req_headers('Content-type'='application/json') %>%
    req_body_json(list('from'= list('user_id'=ml_order$seller$id),'to'=list('user_id'=ml_order$buyer$id),'text'=mensaje)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()}

ml_obtener_mensajes <- function(ml_order,ml_token){
  if(!is.null(ml_order$pack_id)){
    id <- ml_order$pack_id
  }else{
    id <- ml_order$id
  }
  res_sup <- request(paste0("https://api.mercadolibre.com/messages/packs/",id,"/sellers/",ml_order$seller$id,"?tag=post_sale")) %>%
    req_method("GET") %>%
    req_headers('Authorization'=paste0("Bearer ",ml_token)) %>% 
    req_headers('Content-type'='application/json') %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
}

ml_separar_orden <- function(ml_order,ml_token){
  shipment_id <- ml_order$shipping$id
  json <- paste0('{
    "reason": "OTHER_MOTIVE",
    "packs": [
        {
            "orders": [
                {
                    "id": "',ml_order$id,'",
                    "quantity": ', 1 ,'
                }
            ]
        },
        {
            "orders": [
                {
                    "id": "',ml_order$id,'",
                    "quantity": ',ml_order$order_items[[1]]$quantity - 1 ,'
                }
            ]
        }
    ]
}')
  
  
  
  resp <- request(paste0("https://api.mercadolibre.com/shipments/", shipment_id, "/split")) %>%
    req_method("POST") %>% 
    req_headers("Authorization" = paste0("Bearer ", ml_token),
                "x-format-new" = "true") %>% 
    req_body_json(fromJSON(json)) %>% 
    req_perform() %>% 
    resp_body_json()
} 

ml_preguntas <- function(question_id,ml_token){
  resp <- request(paste0("https://api.mercadolibre.com/questions/", question_id)) %>% 
    req_method("GET") %>% 
    req_headers(Authorization = paste("Bearer", ml_token)) %>% 
    req_url_query(api_version = 4) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  return(resp)
}

ml_obtener_item <- function(item_id, ml_token) {
  url <- paste0("https://api.mercadolibre.com/items/", item_id)
  resp <- request(url) %>% 
    req_method("GET") %>% 
    req_headers(Authorization = paste("Bearer", ml_token)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  # Convertimos la respuesta a JSON
  return(resp)
}

ml_obtener_user <- function(user_id, ml_token) {
  url <- paste0("https://api.mercadolibre.com/users/", user_id)
  resp <- request(url) %>% 
    req_method("GET") %>% 
    req_headers(Authorization = paste("Bearer", ml_token)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  # Convertimos la respuesta a JSON
  return(resp)
}

ml_responder_pregunta <- function(id,mensaje,ml_token){
  body <- list(
    "question_id" = id,
    "text" = mensaje
  )
  resp <- request("https://api.mercadolibre.com/answers") %>% 
    req_headers(
      "Authorization" = paste0("Bearer ", ml_token),
      "Content-Type" = "application/json") %>% 
    req_body_json(body) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
}
