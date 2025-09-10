library(lubridate)
library(httr2)
cargar_paquetes("purrr")
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
      req_error(is_error = function(resp) FALSE) %>%
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
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
}

mlorder_bypackid <- function(orderid, mltoken) {

    # Intentar como order_id
  url_order <- paste0("https://api.mercadolibre.com/orders/", orderid)
  
  resp <- tryCatch({
    request(url_order) %>% 
      req_auth_bearer_token(mltoken) %>% 
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  }, error = function(e) NULL)
  
  if (!is.null(resp) && last_response()$status_code %in%  c(199:299)) {
    return(resp_body_json(resp))
  }
  
  # Si falló, intentar como pack_id
  url_pack <- paste0("https://api.mercadolibre.com/packs/", orderid)
  
  resp_pack <- request(url_pack) %>% 
    req_auth_bearer_token(mltoken) %>% 
    req_error(is_error = function(resp) FALSE) %>%
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
    ml_shipping <- get_dir_mlorder(mlorder,ml_token)
  }
  records <- list()
  for(i in seq_along(mlorder$payments)){
    movimiento_dinero <- airtable_getrecordslist("movimientos_dinero",Sys.getenv("AIRTABLE_CES_BASE"),
                                                 paste0("id_origen='",mlorder$payments[[i]]$id,"'"))
    if(length(movimiento_dinero)!=0){
      records[[length(records)+1]] <- movimiento_dinero[[1]]$id
    }
  }
  if(length(records)!=0){
    fieldslist <- append(fieldslist,list("movimientos_dinero"=records))
  }
  
  
  if("splitted_order" %in% mlorder$tags){
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
      codigospostalesQRO <- readRDS("~/codigospostalesQRO.RDS")
      codigospostalesCDMX <- readRDS("~/codigospostalesCDMX.RDS")
      if(!is.null(ml_shipping$logistic$type)){
        if(ml_shipping$logistic$type != "xd_drop_off" && ml_shipping$logistic$type != "fulfillment"){
          if(ml_shipping$destination$shipping_address$zip_code %in% codigospostalesQRO){
            mensaje_envio <- paste0(ml_order$id ," pack_id: ",ml_order$pack_id,"\nLa orden de venta se registro como envio local \n",
                                    "CP: ",ml_shipping$destination$shipping_address$zip_code, " Calle: ",shopifyorder$shipping_address$address1,
                                    " Colonia: ",shopifyorder$shipping_address$address2," Ciudad: ",shopifyorder$shipping_address$city,
                                    "\n¿Deseas cambiar el tipo de envio a paqueteria?")
            enviar_mensaje_slack(Sys.getenv("SLACK_ENVIOS_LOCALES_URL"),mensaje_envio)
            airtable_updatesinglerecord(list("entrega_qro"=TRUE),"ordenes_venta",Sys.getenv('AIRTABLE_CES_BASE'),newov_content$id)
          }else{
            if(ml_shipping$destination$shipping_address$zip_code %in% codigospostalesCDMX){
              mensaje_envio <- paste0(ml_order$id ," pack_id: ",ml_order$pack_id,"\nLa orden de venta se registro como envio local \n",
                                      "CP: ",ml_shipping$destination$shipping_address$zip_code, " Calle: ",ml_shipping$destination$shipping_address$street_name,
                                      " Colonia: ",ml_shipping$destination$shipping_address$neighborhood$name," Ciudad: ",ml_shipping$destination$shipping_address$city$name,
                                      "\n¿Deseas cambiar el tipo de envio a paqueteria?")
              enviar_mensaje_slack(Sys.getenv("SLACK_PRUEBA_URL"),mensaje_envio)
              airtable_updatesinglerecord(list("entrega_cdmx"=TRUE),"ordenes_venta",Sys.getenv('AIRTABLE_CES_BASE'),newov_content$id)
            }
          }
        }
      }else{
        if(ml_shipping$destination$shipping_address$zip_code %in% codigospostalesQRO){
          mensaje_envio <- paste0(ml_order$id ," pack_id: ",ml_order$pack_id,"\nLa orden de venta se registro como envio local \n",
                                  "CP: ",ml_shipping$destination$shipping_address$zip_code, " Calle: ",ml_shipping$destination$shipping_address$street_name,
                                  " Colonia: ",ml_shipping$destination$shipping_address$neighborhood$name," Ciudad: ",ml_shipping$destination$shipping_address$city$name,
                                  "\n¿Deseas cambiar el tipo de envio a paqueteria?")
          enviar_mensaje_slack(Sys.getenv("SLACK_ENVIOS_LOCALES_URL"),mensaje_envio)
          airtable_updatesinglerecord(list("entrega_qro"=TRUE),"ordenes_venta",Sys.getenv('AIRTABLE_CES_BASE'),newov_content$id)
        }else{
          if(ml_shipping$destination$shipping_address$zip_code %in% codigospostalesCDMX){
            mensaje_envio <- paste0(ml_order$id ," pack_id: ",ml_order$pack_id,"\nLa orden de venta se registro como envio local \n",
                                    "CP: ",ml_shipping$destination$shipping_address$zip_code, " Calle: ",ml_shipping$destination$shipping_address$street_name,
                                    " Colonia: ",ml_shipping$destination$shipping_address$neighborhood$name," Ciudad: ",ml_shipping$destination$shipping_address$city$name,
                                    "\n¿Deseas cambiar el tipo de envio a paqueteria?")
            enviar_mensaje_slack(Sys.getenv("SLACK_PRUEBA_URL"),mensaje_envio)
            airtable_updatesinglerecord(list("entrega_cdmx"=TRUE),"ordenes_venta",Sys.getenv('AIRTABLE_CES_BASE'),newov_content$id)
          }
        }
      }
      
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
  if(fullfilment_type=="fulfillment" || mlorder$seller$id==Sys.getenv("SELLERID_ML_ASM")){
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

transaccion_mlfull <- function(ml_order,orden_venta){
  fields <- list()
  if(ml_order$seller$id == Sys.getenv("SELLERID_ML_ASM")){
    ubicacion <- "ml_full_asm"
    return(NULL)
  }else{
    ubicacion <- "ml_full_rnd"
  }
  venta_producto <- airtable_getrecorddata_byid(orden_venta$fields$ventas_producto[[1]],"ventas_producto",
                                                Sys.getenv("AIRTABLE_CES_BASE"))
  fields <- list(
    'tipo'='baja',
    "producto"=venta_producto$fields$producto,
    "ventas_producto"=orden_venta$fields$ventas_producto,
    "comentarios"= paste0(orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
    "cantidad"=venta_producto$fields$cantidad,
    "ubicacion"= ubicacion
  )
  airtable_createrecord(fields, "transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
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

ml_obtener_variante <- function(user_product,ml_token){
  varianturl <- paste0("https://api.mercadolibre.com/user-products/",user_product)
  mlvariant <- request(varianturl) %>%
    req_auth_bearer_token(ml_token) %>%
    req_headers(accept= "application/json") %>%
    req_headers("x-format-new"=TRUE) %>% 
    req_headers('content-type' = 'application/x-www-form-urlencoded') %>%
    req_perform() %>%
    resp_body_json()
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

ml_status_item <- function(id_item,ml_token,status){
  url <- paste0("https://api.mercadolibre.com/items/",id_item)
  resp <- request(url) %>% 
    req_method("PUT") %>% 
    req_auth_bearer_token(ml_token) %>% 
    req_headers("accept"= "application/json") %>% 
    req_headers('content-type' = 'application/json') %>% 
    req_body_json(list("status"=status)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
} 

ml_eliminar_item <- function(id_item,ml_token){
  url <- paste0("https://api.mercadolibre.com/items/",id_item)
  resp <- request(url) %>% 
    req_method("PUT") %>% 
    req_auth_bearer_token(ml_token) %>% 
    req_headers("accept"= "application/json") %>% 
    req_headers('content-type' = 'application/json') %>% 
    req_body_json(list("deleted"= "true")) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()%>% 
    resp_body_json()
}

ml_crear_publicacion <- function(titulo,precio,ml_token){
  # json <- paste0('{
  # "title":"',titulo,'",
  # "category_id":"MLM437179",
  # "price":',precio,',
  # "currency_id":"MXN",
  # "available_quantity":1,  
  # "buying_mode":"buy_it_now",
  # "condition":"new",
  # "listing_type_id":"gold_special",
  # "status":"active",
  # "sale_terms":[
  #    {
  #       "id":"WARRANTY_TYPE",
  #       "value_name":"Garantía del vendedor"
  #    },
  #    {
  #       "id":"WARRANTY_TIME",
  #       "value_name":"60 días"
  #    }
  # ],
  # "pictures":[
  #    {
  #       "source":"https://processmediacesrir.s3.us-west-2.amazonaws.com/media-productos/D_831387-MLM50622789114_072022-O.jpg"
  #    }
  # ],
  # "attributes":[
  #    {
  #       "id":"BRAND",
  #       "value_name":"Randu"
  #    },
  #    {
  #       "id":"MANUFACTURER",
  #       "name":"Fabricante",
  #       "value_name":"Randu"
  #    },{
  #       "id":"SELLER_SKU",
  #       "name":"SKU",
  #       "value_name":"10700"
  #    },{
  #       "id":"MODEL",
  #       "name":"Modelo",
  #       "value_name":"Personalizado"
  #    },{
  #       "id":"HEIGHT",
  #       "name":"Altura",
  #       "value_name":"1 cm"
  #    },{
  #       "id":"LENGTH",
  #       "name":"Largo",
  #       "value_name":"1 cm"
  #    },{
  #       "id":"WIDTH",
  #       "name":"Ancho",
  #       "value_name":"1 cm"
  #    }
  # ]
  # }')
  titulo <- substr(titulo, 1, 60)
  data <- list(
    title = titulo,
    category_id = "MLM437179",
    price = precio,
    currency_id = "MXN",
    available_quantity = 1,
    buying_mode = "buy_it_now",
    condition = "new",
    listing_type_id = "gold_special",
    status = "active",
    sale_terms = list(
      list(id = "WARRANTY_TYPE", value_name = "Garantía del vendedor"),
      list(id = "WARRANTY_TIME", value_name = "60 días")
    ),
    pictures = list(
      list(source = "https://processmediacesrir.s3.us-west-2.amazonaws.com/media-productos/D_831387-MLM50622789114_072022-O.jpg")
    ),
    attributes = list(
      list(id = "BRAND", value_name = "Randu"),
      list(id = "MANUFACTURER", name = "Fabricante", value_name = "Randu"),
      list(id = "SELLER_SKU", name = "SKU", value_name = "10700"),
      list(id = "MODEL", name = "Modelo", value_name = "Personalizado"),
      list(id = "HEIGHT", name = "Altura", value_name = "1 cm"),
      list(id = "LENGTH", name = "Largo", value_name = "1 cm"),
      list(id = "WIDTH", name = "Ancho", value_name = "1 cm"),
      list(id = "INCLUDES_ASSEMBLY_MANUAL",value_name = "Sí"  # o "No", según aplique
      )
    )
  )
  url <- paste0("https://api.mercadolibre.com/items/")
  resp <- request(url) %>% 
    req_method("POST") %>% 
    req_auth_bearer_token(ml_token) %>% 
    req_headers(accept= "application/json") %>% 
    req_body_json(data) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  return(resp)
}

ml_crear_descripcion <- function(id_item,descripcion,ml_token){
  url <- paste0("https://api.mercadolibre.com/items/",id_item,"/description")
  resp <- request(url) %>% 
    req_method("POST") %>% 
    req_auth_bearer_token(ml_token) %>% 
    req_headers(accept= "application/json") %>% 
    req_body_json(list("plain_text"=paste0("Producto personalizado: ", descripcion))) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
}

ml_cerrar_item <- function(ml_token){
  # Recolectar todas las publicaciones
  items <- c()
  offset <- 0
  limit <- 50
  while(T) {
    fecha_desde <- format(Sys.Date() - 2, "%Y-%m-%dT00:00:00.000Z") 
    fecha_desde <- Sys.Date()-3
    res <- request(paste0("https://api.mercadolibre.com/users/", Sys.getenv("SELLERID_ML_RANDU"), "/items/search")) %>%
      req_auth_bearer_token(ml_token) %>%
      req_url_query(items.date_created.from = fecha_desde, offset = offset, limit = limit) %>%
      req_perform() %>% 
      resp_body_json()
    items <- c(items, res$results)
    if(!last_response()$status_code %in% c(199:299)){
      print(resp_body_json())
      break
    }
   # break
    offset <- offset + limit
    if (length(res$results) < limit) {
      return(res)
      break
    }
    
  print(offset)
  
  }
}

ml_subir_publicaciones_air <- function(item,ml_token){
  subtitle <- ""
  sku <- 0
  if(item$shipping$mode == "me2"){
    if(!is.null(item$shipping$logistic_type)){
      if(item$shipping$logistic_type == "fulfillment"){
        envio <- "Fulfillment by marketplace"
      }else{
        envio <- "Agencia"
      }
    }else{
      envio <- "Agencia"
    }
  }else{
    envio <- "Fulfillment by Randu"
  }
  if(item$status == "paused"){
    status <- "Pausada"
  }else{
    if(item$status == "active"){
      status <- "Activo"
    }else{
      if(item$status == "closed"){
        status <- "Cerrado"
      }else{
        status <- "Inactivo"
      }
    }
  }
  # for(k in 1:length(id_img_var[[j]]$selectedoptions)){
  #   if(k==1){
  #     subtitle <- paste0(id_img_var[[j]]$selectedoptions[[k]]$name,": ",id_img_var[[j]]$selectedoptions[[k]]$value)
  #   }
  #   
  # }
  # 
  if(length(item$variations)!=0){
    for(variante in item$variations){
      subtitle <- ""
      user_product <- ml_obtener_variante(variante$user_product_id,ml_token)
      if(variante$available_quantity == 0){
        status <- "Pausada"
      }
      for(i in seq_along(user_product$attributes)){
        subtitle <- paste0(subtitle,user_product$attributes[[i]]$name,": ",user_product$attributes[[i]]$values[[1]]$name,"/")
        
        if(str_detect(tolower(user_product$attributes[[i]]$name),"sku")){
          sku <- user_product$attributes[[i]]$values[[1]]$name
        }
      }
      producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",sku))
      fields <- list(
        "canal"= "mercadolibre randu",
        "id_canal" = item$id,
        "titulo" = item$title,
        "product_id"= variante$user_product_id,
        "subtitulo"=subtitle,
        "status"=status,
        "tipo_envio"=envio,
        "stock_publicacion"=variante$available_quantity
      )
      if(length(producto)!=0){
        fields <- append(fields,list("producto"=list(producto[[1]]$id)))
      }
      if(!is.null(variante$inventory_id)){
        url_etiqueta <- paste0("https://processmediacesrir.s3.us-west-2.amazonaws.com/publicaciones/etiqueta_marketplace/",variante$inventory_id,".pdf")
        fields <- append(fields,list("id_inventario_marketplace"=variante$inventory_id ,"etiqueta_marketplace"=list(list("url"=url_etiqueta))))
      }
      airtable_createrecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"))
      if(!last_response()$status_code %in% c(199:299)){
        mensaje <- paste0("No se pudo subir una nueva publicacion con variantes: ",item$title,"\nlast_response:",
                          toJSON(last_response() %>% resp_body_json()),"\nlast_request:",
                          toJSON(last_request()$body))
        print(mensaje)
        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
      }
    }
  }else{
    subtitle <- ""
    for(i in seq_along(item$attributes)){
      subtitle <- paste0(subtitle,item$attributes[[i]]$name,": ",item$attributes[[i]]$value_name,"/")
      
      if(str_detect(tolower(item$attributes[[i]]$name),"sku")){
        sku <- item$attributes[[i]]$value_name
      }
    }
    producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",sku))
    fields <- list(
      "canal"= "mercadolibre randu",
      "id_canal" = item$id,
      "titulo" = item$title,
      "subtitulo"=subtitle,
      "status"=status,
      "tipo_envio"=envio,
      "stock_publicacion"=item$available_quantity
    )
    if(length(producto)!=0){
      fields <- append(fields,list("producto"=list(producto[[1]]$id)))
    }
    if(!is.null(item$inventory_id)){
      #url_etiqueta <- paste0("https://processmediacesrir.s3.us-west-2.amazonaws.com/publicaciones/etiqueta_marketplace/",item$inventory_id,".pdf")
      fields <- append(fields,list("id_inventario_marketplace"=item$inventory_id ))#,"etiqueta_marketplace"=list(list("url"=url_etiqueta))))
    }
    airtable_createrecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"))
    if(!last_response()$status_code %in% c(199:299)){
      mensaje <- paste0("No se pudo subir una nueva publicacion: ",item$title,"\nlast_response:",
                        toJSON(last_response() %>% resp_body_json()),"\nlast_request:",
                        toJSON(last_request()$body))
      print(mensaje)
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
    }
  }
}

ml_actualizar_publicaciones_air <- function(item,publicacion,ml_token){
  subtitle <- ""
  sku <- 0
  if(item$shipping$mode == "me2"){
    if(!is.null(item$shipping$logistic_type)){
      if(item$shipping$logistic_type == "fulfillment"){
        envio <- "Fulfillment by marketplace"
      }else{
        envio <- "Agencia"
      }
    }else{
      envio <- "Agencia"
    }
  }else{
    envio <- "Fulfillment by Randu"
  }
  
  if(item$status == "paused"){
    status <- "Pausada"
  }else{
    if(item$status == "active"){
      status <- "Activo"
    }else{
      if(item$status == "closed"){
        status <- "Cerrado"
      }else{
        status <- "Inactivo"
      }
    }
  }
  if(length(item$variations)==0){
    publicacion <- publicacion[[1]]
    for(i in seq_along(item$attributes)){
      subtitle <- paste0(subtitle,item$attributes[[i]]$name,": ",item$attributes[[i]]$value_name,"/")
      
      if(str_detect(tolower(item$attributes[[i]]$name),"sku")){
        sku <- item$attributes[[i]]$value_name
      }
    }
    
    producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",sku))
    
    fields <- list(
      "titulo" = item$title,
      "subtitulo"=subtitle,
      "status"=status,
      "tipo_envio"=envio,
      "stock_publicacion"=item$available_quantity
    )
    if(length(producto)!=0){
      fields <- append(fields,list("producto"=list(producto[[1]]$id)))
    }
    if(!is.null(item$inventory_id)){
      #url_etiqueta <- paste0("https://processmediacesrir.s3.us-west-2.amazonaws.com/publicaciones/etiqueta_marketplace/",item$inventory_id,".pdf")
      fields <- append(fields,list("id_inventario_marketplace"=item$inventory_id ))
    }
    #airtable_createrecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"))
    airtable_updatesinglerecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),publicacion$id)
    if(!last_response()$status_code %in% c(199:299)){
      print("Al actualizar")
      print(item$title)
    }
  }else{
    for(variante in item$variations){
      subtitle <- ""
      publicacion_variante <- buscar_primera_por_valor(publicacion,variante$user_product_id)
      user_product <- ml_obtener_variante(variante$user_product_id,ml_token)
      if(variante$available_quantity == 0){
        status <- "Pausada"
      }
      for(i in seq_along(user_product$attributes)){
        subtitle <- paste0(subtitle,user_product$attributes[[i]]$name,": ",user_product$attributes[[i]]$values[[1]]$name,"/")
        
        if(str_detect(tolower(user_product$attributes[[i]]$name),"sku")){
          sku <- user_product$attributes[[i]]$values[[1]]$name
        }
      }
      producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",sku))
      fields <- list(
        "titulo" = item$title,
        "product_id"= variante$user_product_id,
        "subtitulo"=subtitle,
        "tipo_envio"=envio,
        "status"=status,
        "stock_publicacion"=variante$available_quantity
      )
      if(length(producto)!=0){
        fields <- append(fields,list("producto"=list(producto[[1]]$id)))
      }
      if(!is.null(variante$inventory_id)){
        url_etiqueta <- paste0("https://processmediacesrir.s3.us-west-2.amazonaws.com/publicaciones/etiqueta_marketplace/",variante$inventory_id,".pdf")
        fields <- append(fields,list("id_inventario_marketplace"=variante$inventory_id ,"etiqueta_marketplace"=list(list("url"=url_etiqueta))))
      }
      if(!is.null(publicacion_variante)){
        airtable_updatesinglerecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),publicacion_variante$record_id)
        if(!last_response()$status_code %in% c(199:299)){
          mensaje <- paste0("No se pudo actualizar una publicacion: ",item$title,"\nlast_response:",
                            toJSON(last_response() %>% resp_body_json()),"\nlast_request:",
                            toJSON(last_request()$body))
          print(mensaje)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
        }
      }else{
        
        fields <- append(fields,list("canal"= "mercadolibre randu","id_canal" = item$id))
        airtable_createrecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"))
        if(!last_response()$status_code %in% c(199:299)){
          mensaje <- paste0("No se pudo subir una publicacion: ",item$title,"\nlast_response:",
                            toJSON(last_response() %>% resp_body_json()),"\nlast_request:",
                            toJSON(last_request()$body))
          print(mensaje)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
        }
      }
      
      # airtable_createrecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"))
      # if(!last_response()$status_code %in% c(199:299)){
      #   print(item$title)
      # }
    }
  }
}

ml_operaciones_fulfillment <- function(id_operacion,ml_token){
  url <- paste0("https://api.mercadolibre.com/stock/fulfillment/operations/", id_operacion)
  resp <- request(url) %>% 
    req_method("GET") %>% 
    req_headers(Authorization = paste("Bearer", ml_token)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  return(resp)
}

ml_stock_item <- function(id_item,ml_token,stock){
  url <- paste0("https://api.mercadolibre.com/items/",id_item)
  resp <- request(url) %>% 
    req_method("PUT") %>% 
    req_auth_bearer_token(ml_token) %>% 
    req_headers(accept= "application/json") %>% 
    req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
    req_body_json(list("available_quantity" = stock)) %>% 
    req_perform()
}

pausar_publicaciones_ml <- function(){
  productos  <- readRDS("publicaciones_a_pausar.RDS")
  ml_token <- get_active_token()
  if(productos[[1]]=="activo"){
    contar <- airtable_getrecordslist("solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),
                                      "AND({empacado}='',{prioridad}='8 - Antes de la 1',FIND('2000',{comentarios}),
                                      NOT(FIND('caja',{producto_solicitado})),{origen}!='empaque CNC')")
    #contar <- airtable_getrecordslist("solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),"AND(FIND('VP17600 - 10914 - recibidor 100x30 - 25mm negro',{venta_producto}))")
    if(length(contar)!=0){
      contar_productos <- sapply(contar, function(x){
        if(!str_detect(x$fields$producto_solicitado,"caja")){
          x$fields$cantidad
        }else{0}
      },simplify = T)
      contar_cajas <- sapply(contar, function(x){
        if(str_detect(x$fields$producto_solicitado,"caja")){
          x$fields$cantidad
        }else{0}
      },simplify = T)
      contar_productos <- quitar_null(contar_productos)
      if(sum(contar_productos)>10){
        
        ml_status_publicacion_agencia(ml_token,"paused")
        productos[[1]] <- "pausa"
        saveRDS(productos,"publicaciones_a_pausar.RDS")
        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),"Se pausaron las publicaciones")
      }
    }
  }
}

ml_agencia_sin_partes <- function(){
  publicaciones_agencia <- airtable_getrecordslist("publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),
                                                   "AND({canal}='mercadolibre randu',{tipo_envio}='Agencia',NOT({status}='Inactivo'))")
  productos <- sapply(publicaciones_agencia,function(x){
    producto <- airtable_getrecorddata_byid(x$fields$producto[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
    if(length(producto$fields$partes_producto)<=1 && !is.null(producto$fields$item_produccion)){
      return(x$fields$id_canal)
    }
  })
  if(file.exists("publicaciones_a_pausar.RDS")) {
    publicaciones_a_pausar <- readRDS("publicaciones_a_pausar.RDS")
    status <- publicaciones_a_pausar[[1]]
    productos <- quitar_null(productos)
    if(status == "pausa"){
      ml_token <- get_active_token()
      dar_de_baja <- setdiff(productos,publicaciones_a_pausar[-1])
      dar_de_alta <- setdiff(publicaciones_a_pausar[-1],productos)
      if(length(dar_de_baja) !=0){
        ml_status_conjunto_publicaciones(dar_de_baja,ml_token,"paused")
      }
      if(length(dar_de_alta) !=0){
        ml_status_conjunto_publicaciones(dar_de_alta,ml_token,"active")
      }
    }
    productos <- append(status,productos)
    productos <- unique(productos)
    saveRDS(productos,"publicaciones_a_pausar.RDS")
    return(productos)
  } else {
    status <- "activo"
    productos <- quitar_null(productos)
    productos <- append(status,productos)
    productos <- unique(productos)
    saveRDS(productos,"publicaciones_a_pausar.RDS")
    return(productos)
  }
  
}

ml_status_conjunto_publicaciones <- function(productos,ml_token,status){
  
  if(length(productos)>=1 ){
    productos <- productos[-1]
    for(i in 1:length(productos)){
      item_no_cambio <- ml_obtener_item(productos[[i]],ml_token)
      if(item_no_cambio$status == "under_review"){
        next
      }
      item <- ml_status_item(productos[[i]],ml_token,status)
      if(!last_response()$status_code %in% c(199:299) ){
        causa <- toJSON(last_response() %>% resp_body_json())
        
        if(length(item$cause)!=0){
          no_stock <- F
          for(i in seq_along(item$cause)){
            no_stock <- no_stock || str_detect(item$cause[[i]]$message,"stock") || str_detect(item$cause[[i]]$message,"not possible to activate")
          }
          if(no_stock){
            aux <- ml_stock_item(productos[[i]],ml_token,10)
            if(!last_response()$status_code %in% c(199:299) ){
              
              mensaje_dar_pausa <- paste0("El item: ",item_no_cambio$id," ",item_no_cambio$title,"\nNo se pudo poner en ",
                                          status," ni cambiar la cantidad\n",toJSON(last_response() %>% resp_body_json()))
              enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
            }
          }else{
            mensaje_dar_pausa <- paste0("El item: ",item_no_cambio$id," ",item_no_cambio$title,"\nNo se pudo poner en ",
                                        status,"\n",causa)
            enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
          }
        }else{
          mensaje_dar_pausa <- paste0("El item: ",item_no_cambio$id," ",item_no_cambio$title,"\nNo se pudo poner en ",
                                      status,"\n",causa)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
        }
      }else{
        if(item$status != status){
          mensaje_dar_pausa <- paste0("El item: ",item$id," ",item$title,"\n",
                                      "No se pudo poner en ",status)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
        }
        if(item$available_quantity<2){
          aux <- ml_stock_item(item$id,ml_token,10)
          if(!last_response()$status_code %in% c(199:299) ){
            mensaje_dar_pausa <- paste0("El item: ",item$id," ",item$title,"\nNo se pudo poner cambiar la cantidad\n",
                                        toJSON(last_response() %>% resp_body_json()))
            enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
          }
        }
      }
    }
  }
}

ml_status_publicacion_agencia <- function(ml_token,status){
  productos  <- readRDS("publicaciones_a_pausar.RDS")
  if(length(productos)>1 ){
    productos <- productos[-1]
    for(i in 1:length(productos)){
      Sys.sleep(1.5)
      item_no_cambio <- ml_obtener_item(productos[[i]],ml_token)
      if(item_no_cambio$status == "under_review"){
        next
      }
      item <- ml_status_item(productos[[i]],ml_token,status)
      if(!last_response()$status_code %in% c(199:299) ){
        causa <- toJSON(last_response() %>% resp_body_json())
        
        if(length(item$cause)!=0){
          no_stock <- F
          for(i in seq_along(item$cause)){
            no_stock <- no_stock || str_detect(item$cause[[i]]$message,"stock") || str_detect(item$cause[[i]]$message,"not possible to activate")
          }
          if(no_stock){
            aux <- ml_stock_item(productos[[i]],ml_token,10)
            if(!last_response()$status_code %in% c(199:299) ){
              
              mensaje_dar_pausa <- paste0("El item: ",item_no_cambio$id," ",item_no_cambio$title,"\nNo se pudo poner en ",
                                          status," ni cambiar la cantidad\n",toJSON(last_response() %>% resp_body_json()))
              enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
            }
          }else{
            mensaje_dar_pausa <- paste0("El item: ",item_no_cambio$id," ",item_no_cambio$title,"\nNo se pudo poner en ",
                                        status,"\n",causa)
            enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
          }
        }else{
          mensaje_dar_pausa <- paste0("El item: ",item_no_cambio$id," ",item_no_cambio$title,"\nNo se pudo poner en ",
                                      status,"\n",causa)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
        }
      }else{
        if(item$status != status){
          mensaje_dar_pausa <- paste0("El item: ",item$id," ",item$title,"\n",
                                      "No se pudo poner en ",status)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
        }
        if(item$available_quantity<2){
          aux <- ml_stock_item(item$id,ml_token,10)
          if(!last_response()$status_code %in% c(199:299) ){
            mensaje_dar_pausa <- paste0("El item: ",item$id," ",item$title,"\nNo se pudo poner cambiar la cantidad\n",
                                        toJSON(last_response() %>% resp_body_json()))
            enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_dar_pausa)
          }
        }
      }
    }
  }
}

ml_items_id <- function(user_id,ml_token){
  base_url <- paste0("https://api.mercadolibre.com/users/", user_id, "/items/search")
  offset <- 0
  limit <- 50
  all_ids <- c()
  
  while(T) {
    url <- paste0(base_url, "?offset=", offset, "&limit=", limit)
    
    resp <- request(url) %>%
      req_auth_bearer_token(ml_token) %>%
      req_perform() %>%
      resp_body_json()
    
    ids <- resp$results
    total <- resp$paging$total
    
    all_ids <- append(all_ids, ids)
    
    offset <- offset + limit
    if (offset >= total){
      break
    }
  }
  
  return(all_ids)
}

ml_enviar_guia <- function(ml_order,ml_token,guia){
  ml_shipping <- get_dir_mlorder(ml_order,ml_token)
  url <- paste0("https://api.mercadolibre.com/shipments/",ml_shipping$id)
  resp <- request(url) %>% 
    req_method("PUT") %>% 
    req_auth_bearer_token(ml_token) %>% 
    req_headers("accept"= "application/json") %>% 
    req_headers('content-type' = 'application/json') %>% 
    req_body_json(list( "tracking_number"= guia[[1]],"status"= "ready_to_ship",
                        "receiver_id"= ml_shipping$destination$receiver_id,
                        "carrier_info" = list(
                          "name"= guia[[2]]
                        )
    )) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()%>% 
    resp_body_json()
}

ml_obtener_mensaje <- function(id_mensaje,ml_token){
  url <- paste0("https://api.mercadolibre.com/messages/", id_mensaje)
  response <- request(url) %>% 
    req_headers(
      Authorization = paste("Bearer", ml_token)
    ) %>% 
    req_url_query(tag = "post_sale") %>%   # Agrega el parámetro ?tag=post_sale
    req_perform() %>% 
    resp_body_json()
}

ml_mensaje_slack <- function(mensaje_body){
  tryCatch(
    expr = {
      hilo_enviados <- list()
      id_mensaje <- mensaje_body$id_resource
      for(i in seq_along(id_mensaje)){
        
        cuerpo <- fromJSON(mensaje_body$body[[i]])
        if(!is.null(cuerpo$attempts)){
          if(cuerpo$attempts != 1){
            next
          }
        }
        
        if(cuerpo$user_id == Sys.getenv("SELLERID_ML_ASM")){
          recordid_token <- "recQLtjnMhd4ZCiJq"
          canal <- "mercadolibreasm"
          canal_slack <- ""
          next
        }else{
          recordid_token <- ""
          canal <- NULL
          canal_slack <- "C096XTJ6T25"
        }
        ml_token <- get_active_token(recordid_token)
        slack_user_token <- Sys.getenv("SLACK_USER_TOKEN")
        mensaje_ml <- ml_obtener_mensaje(id_mensaje[[i]],ml_token)
        
        if(mensaje_ml$messages[[1]]$from$user_id != Sys.getenv("SELLERID_ML_RANDU") && mensaje_ml$messages[[1]]$from$user_id != Sys.getenv("SELLERID_ML_ASM")){
          Sys.sleep(5)
          mensajes_slack_aux <- buscar_mensajes_slack(mensaje_ml$messages[[1]]$id,slack_user_token)
          if(length(mensajes_slack_aux)!=0){
            next 
          }
          
          mensajes_slack <- buscar_mensajes_slack(mensaje_ml$messages[[1]]$message_resources[[1]]$id,slack_user_token)
          ml_order <- mlorder_bypackid(mensaje_ml$messages[[1]]$message_resources[[1]]$id,ml_token)
          if(length(ml_order$orders)!=0){
            ml_order <- mlorder_bypackid(paste0(ml_order$orders[[1]]$id),ml_token)
          }
          mensaje_hilo <- paste0(mensaje_ml$messages[[1]]$message_resources[[1]]$id,
                                 ":\nID mensaje:",mensaje_ml$messages[[1]]$id,
                                 "\n",ml_order$order_items[[1]]$item$title,"\n",ml_order$buyer$nickname,
                                 " ",ml_order$buyer$first_name, " ", ml_order$buyer$last_name,"\n",
                                 "Envió un mensaje:\n*",mensaje_ml$messages[[1]]$text,"*")
          if(length(mensajes_slack)!=0){
            mensajes_slack_canal <- subset(mensajes_slack,canal == "mensajes_mercado_libre" & usuario == "procesos") 
            
            if(length(mensajes_slack_canal$canal)!=0){
              #puede fallar por el id del canal
              slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),mensajes_slack_canal$canal[[length(mensajes_slack_canal$ts)]],mensajes_slack_canal$ts[[length(mensajes_slack_canal$ts)]],mensaje_hilo)
            }else{
              ts_mensaje <- purrr::pluck(hilo_enviados,mensaje_ml$messages[[1]]$message_resources[[1]]$id)
              if(is.null(ts_mensaje)){
                msj_hilo <- enviar_a_slack(Sys.getenv("SLACK_BOT_TOKEN"),canal_slack,mensaje_hilo)
                if(msj_hilo$ok){
                  hilo_enviados[[length(hilo_enviados) + 1]] <- list("ts"=msj_hilo$ts,"canal"=msj_hilo$channel)
                  names(hilo_enviados)[[length(hilo_enviados)]] <- mensaje_ml$messages[[1]]$message_resources[[1]]$id
                }
              }else{
                slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),ts_mensaje$canal,ts_mensaje$ts,mensaje_hilo)
                
              }
              #enviar_mensaje_slack(Sys.getenv("SLACK_MENSAJES_ML_URL"),mensaje_hilo)
            }
          }else{
            ts_mensaje <- purrr::pluck(hilo_enviados,mensaje_ml$messages[[1]]$message_resources[[1]]$id)
            if(is.null(ts_mensaje)){
              msj_hilo <- enviar_a_slack(Sys.getenv("SLACK_BOT_TOKEN"),canal_slack,mensaje_hilo)
              if(msj_hilo$ok){
                hilo_enviados[[length(hilo_enviados) + 1]] <- list("ts"=msj_hilo$ts,"canal"=msj_hilo$channel)
                names(hilo_enviados)[[length(hilo_enviados)]] <- mensaje_ml$messages[[1]]$message_resources[[1]]$id
              }
            }else{
              slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),ts_mensaje$canal,ts_mensaje$ts,mensaje_hilo)
            }
          }
        }
      }
    },error=function(e){
      mensaje_error <- paste0("Ocurrio un error al enviar el mensaje de mercado libre a slack", e)
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
    }
  )
}

ml_full_register_op <- function(operaciones){
  tryCatch(
    expr={
      operaciones <- operaciones %>% distinct(id_resource, .keep_all = TRUE)
      operacion_id <- operaciones$id_resource
      for(i in seq_along(operacion_id)){
        fields <- list()
        cuerpo <- fromJSON(operaciones$body[[i]])
        if(cuerpo$user_id == Sys.getenv("SELLERID_ML_ASM")){
          recordid_token <- "recQLtjnMhd4ZCiJq"
          canal <- "mercadolibreasm"
          ubicacion <- "ml_full_asm"
        }else{
          recordid_token <- ""
          canal <- NULL
          ubicacion <- "ml_full_rnd"
        }
        ml_token <- get_active_token(recordid_token)
        operacion <- ml_operaciones_fulfillment(operacion_id[[i]],ml_token)
        
        if(last_response()$status_code %in% c(199:299)){
          # if(!operacion$type %in% c("SALE_CONFIRMATION", "ADJUSTMENT")){
          #   print(paste0(i, ": ",operacion$type))
          # }
          if(operacion$type %in% c("SALE_CANCELATION","INBOUND_RECEPTION","TRANSFER_DELIVERY","SALE_DELIVERY_CANCELATION")){
            publicacion <- airtable_getrecordslist("publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_inventario_marketplace='",operacion$inventory_id,"'"))
            if(length(publicacion)==0){
              print(paste0("No se pudo: ",operacion$inventory_id))
              next
            }
            id_item <- ""
            permalink <- ""
            nombre <- ""
            if(last_response()$status_code %in% c(199:299) && length(publicacion)!=0){
              id_item <- publicacion[[1]]$fields$id_canal
              nombre <- publicacion[[1]]$fields$titulo
              permalink <-   publicacion[[1]]$fields$link_ml$url      
            }
            refs <- operacion$external_references
            
            # Construir texto tipo "type - value" para cada uno
            refs_text <- sapply(refs, function(ref) {
              paste0(ref$type, " - ", ref$value)
            })
            comentarios <- paste0("Tipo: ",operacion$type,": ",paste(refs_text, collapse = ", ")," ID:",operacion$id )
            cantidad <- operacion[["detail"]][["available_quantity"]]
            if(is.null(cantidad)){
              cantidad <- 1
              comentarios <- paste0(comentarios," ",toJSON(operacion[["detail"]]))
            }
            
            fields <- list(
              "tipo"="alta",
              "cantidad"= cantidad,
              "comentarios"=comentarios,
              "ubicacion" = ubicacion,
              "producto" = publicacion[[1]]$fields$producto
              
            )
            
          }
          if(operacion$type %in% c("TRANSFER_RESERVATION","WITHDRAWAL_RESERVATION","SALE_DELIVERY_CANCELATION")){
            publicacion <- airtable_getrecordslist("publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_inventario_marketplace='",operacion$inventory_id,"'"))
            if(length(publicacion)==0){
              next
            }
            id_item <- ""
            permalink <- ""
            nombre <- ""
            if(last_response()$status_code %in% c(199:299) && length(publicacion)!=0){
              id_item <- publicacion[[1]]$fields$id_canal
              nombre <- publicacion[[1]]$fields$titulo
              permalink <-   publicacion[[1]]$fields$link_ml$url      
            }
            refs <- operacion$external_references
            
            # Construir texto tipo "type - value" para cada uno
            refs_text <- sapply(refs, function(ref) {
              paste0(ref$type, " - ", ref$value)
            })
            comentarios <- paste0("Tipo: ",operacion$type,": ",paste(refs_text, collapse = ", ")," ID:",operacion$id )
            cantidad <- operacion[["detail"]][["available_quantity"]]
            if(is.null(cantidad)){
              cantidad <- pluck(operacion, "detail", "not_available_quantity", .default = 0)
              comentarios <- paste0(comentarios," ",toJSON(operacion[["detail"]]))
            }
            
            fields <- list(
              "tipo"="baja",
              "cantidad"= safe_abs_numeric(cantidad),
              "comentarios"=comentarios,
              "ubicacion" = ubicacion,
              "producto" = publicacion[[1]]$fields$producto
            )
          }
          if(operacion$result$available_quantity==0 && operacion$result$total == 0){
            id_item <- ""
            permalink <- ""
            nombre <- ""
            publicacion <- airtable_getrecordslist("publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_inventario_marketplace='",operacion$inventory_id,"'"))
            if(last_response()$status_code %in% c(199:299) && length(publicacion)!=0){
              id_item <- publicacion[[1]]$fields$id_canal
              nombre <- publicacion[[1]]$fields$titulo
              permalink <-   publicacion[[1]]$fields$link_ml$url
            }
            mensaje_operacion <- paste0("El stock de la publicacion: ", id_item, "  ", nombre,"\n",permalink
                                        ,"\nID del inventario de fulfillment: ",operacion$inventory_id,"\n"
                                        ,"Se quedo sin stock en full por la operacion: ",operacion$type," ",operacion$detail$available_quantity)
            enviar_mensaje_slack(Sys.getenv("SLACK_STOCK_FULL_ML_URL"),mensaje_operacion)
          }
          if(length(fields)!=0){
            fields$comentarios <- paste0(fields$comentarios," FECHA:",Sys.Date())
            airtable_createrecord(fields,"transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
          }
          
        }else{
          mensaje <- paste0("No se pudo registrar transaccion de almacen de full por:\n ",
                            "ERROR: ",last_response()$status_code,
                            "ID Mercado libre: ",operacion_id[[i]], "ID Supabase: ",
                            operaciones$id[[i]],"\n",last_response() %>% resp_body_string(),
                            "\nRevise y compruebe que sucedio")
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
        }
      }
    },error=function(e){
      if(!is.null(last_response() %>% resp_body_string())){
        mensaje <- paste0("Ocurrio un error al registrar transaccion de almacen de full:\n ",
                          "ERROR: ",e,"  STATUS:", last_response()$status_code,
                          " ID Mercado libre: ",operacion_id[[i]], " ID Supabase: ",
                          operaciones$id[[i]],"\n",last_response() %>% resp_body_string(),
                          "\nRevise y compruebe que sucedio")
      }else{
        mensaje <- paste0("Ocurrio un error al registrar transaccion de almacen de full:\n ",
                          "ERROR: ",e)
      }
      
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
    }
  )
  
}

ml_full_saldo_inicial <- function(){
  publicaciones_full <- airtable_getrecordslist("publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),
                                                paste0("AND(canal='mercadolibre randu',tipo_envio='Fulfillment by marketplace')"))
  tabla_publicaciones <- airtable_to_table(publicaciones_full)
  ml_token <- get_active_token()
  skus_repetidos <- list()
  fields_todos <- list() 
  items <- sapply(unique(tabla_publicaciones$id_canal), function(x){
    item <- ml_obtener_item(x,ml_token)
  },simplify = F)
  for(i in 1:length(publicaciones_full)){
    if(length(publicaciones_full[[i]]$fields$product_id)!=0){
      item <- buscar_primera_por_valor(items,publicaciones_full[[i]]$fields$product_id)
      cantidad <- item$available_quantity
      sku <- publicaciones_full[[i]]$fields$producto
      if(is.null(sku)){
        print("Tu producto no tiene sku")
        next
      }
      if(sku[[1]] %in% skus_repetidos){
        print("tu producto ya fue registrado anteriormente")
        next
      }
      comentarios <- paste0("Primer registro: ",publicaciones_full[[i]]$fields$id_canal," User_product:",
                            publicaciones_full[[i]]$fields$product_id)
      skus_repetidos[[length(skus_repetidos) + 1]] <- sku[[1]]
      fields <- list("tipo"="saldo inicial",
                     "ubicacion"="ml_full_rnd",
                     "cantidad"=safe_abs_numeric(cantidad),
                     "producto"=sku,
                     "comentarios"=comentarios
                     )
      fields_todos[[length(fields_todos)+1]] <- fields
                    
    }else{
      #item <- buscar_primera_por_valor(items,publicaciones_full[[i]]$fields$id_canal)
      item <- pluck(items,publicaciones_full[[i]]$fields$id_canal)
      cantidad <- item$available_quantity
      sku <- publicaciones_full[[i]]$fields$producto
      if(is.null(sku)){
        print("Tu producto no tiene sku")
        next
      }
      if(sku[[1]] %in% skus_repetidos){
        print("tu producto ya fue registrado anteriormente")
        next
      }
      skus_repetidos[[length(skus_repetidos) + 1]] <- sku[[1]]
      comentarios <- paste0("Primer registro: ",publicaciones_full[[i]]$fields$id_canal)
      fields <- list("tipo"="saldo inicial",
                     "ubicacion"="ml_full_rnd",
                     "cantidad"=safe_abs_numeric(cantidad),
                     "producto"=sku,
                     "comentarios"=comentarios)
      fields_todos[[length(fields_todos)+1]] <- fields
    }
    if(safe_abs_numeric(cantidad)==0){
      next
    }
    airtable_createrecord(fields,"transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
    if(!last_response()$status_code %in% c(199:299)){
      mensaje_ml <- paste0("\nURL:",last_response()$url,"\nStatus:",last_response()$status_code,
                           "\nBody:\n",toJSON(last_response() %>% resp_body_json()),
                           "\nRequest:\n",toJSON(last_request()$body))
      enviar_mensaje_slack(Sys.getenv("SLACK_PRUEBA_URL"),mensaje_ml)
      print(i)
      break
    }
   
    #variante <- buscar_primera_por_valor(items,"MLM2203256173")
  }
  return(list("fields"=fields_todos,"skus"=skus_repetidos))
}