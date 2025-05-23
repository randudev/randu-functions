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
  json <- paste0('{
  "title":"',titulo,'",
  "category_id":"MLM437179",
  "price":',precio,',
  "currency_id":"MXN",
  "available_quantity":1,  
  "buying_mode":"buy_it_now",
  "condition":"new",
  "listing_type_id":"gold_special",
  "status":"active",
  "sale_terms":[
     {
        "id":"WARRANTY_TYPE",
        "value_name":"Garantía del vendedor"
     },
     {
        "id":"WARRANTY_TIME",
        "value_name":"60 días"
     }
  ],
  "pictures":[
     {
        "source":"https://processmediacesrir.s3.us-west-2.amazonaws.com/media-productos/D_831387-MLM50622789114_072022-O.jpg"
     }
  ],
  "attributes":[
     {
        "id":"BRAND",
        "value_name":"Randu"
     },
     {
        "id":"MANUFACTURER",
        "name":"Fabricante",
        "value_name":"Randu"
     },{
        "id":"SELLER_SKU",
        "name":"SKU",
        "value_name":"10700"
     },{
        "id":"MODEL",
        "name":"Modelo",
        "value_name":"Personalizado"
     },{
        "id":"HEIGHT",
        "name":"Altura",
        "value_name":"1 cm"
     },{
        "id":"LENGTH",
        "name":"Largo",
        "value_name":"1 cm"
     },{
        "id":"WIDTH",
        "name":"Ancho",
        "value_name":"1 cm"
     }
  ]
  }')
  url <- paste0("https://api.mercadolibre.com/items/")
  resp <- request(url) %>% 
    req_method("POST") %>% 
    req_auth_bearer_token(ml_token) %>% 
    req_headers(accept= "application/json") %>% 
    req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
    req_body_json(fromJSON(json)) %>% 
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
    req_headers('content-type' = 'application/x-www-form-urlencoded') %>% 
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

ml_subir_publicaciones_air <- function(item){
  subtitle <- ""
  # for(k in 1:length(id_img_var[[j]]$selectedoptions)){
  #   if(k==1){
  #     subtitle <- paste0(id_img_var[[j]]$selectedoptions[[k]]$name,": ",id_img_var[[j]]$selectedoptions[[k]]$value)
  #   }
  #   
  # }
  # 
  for(i in seq_along(item$attributes)){
    subtitle <- paste0(subtitle,item$attributes[[i]]$name,": ",item$attributes[[i]]$value_name,"/")
    
    if(str_detect(tolower(item$attributes[[i]]$name),"sku")){
      sku <- item$attributes[[i]]$value_name
    }
  }
  producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",sku))
  if(item$status!="paused"){
    status <- "Activo"
  }else{
    status <- "Inactivo" 
  }
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
    url_etiqueta <- paste0("https://processmediacesrir.s3.us-west-2.amazonaws.com/publicaciones/etiqueta_marketplace/",item$inventory_id,".pdf")
    fields <- append(fields,list("id_inventario_marketplace"=item$inventory_id ,"etiqueta_marketplace"=list(list("url"=url_etiqueta))))
  }
  airtable_createrecord(fields,"publicaciones",Sys.getenv("AIRTABLE_CES_BASE"))
  if(!last_response()$status_code %in% c(199:299)){
    print(item$title)
  }
}

ml_actualizar_publicaciones_air <- function(item,publicacion){
  
  subtitle <- ""
  sku <- 0
  for(i in seq_along(item$attributes)){
    subtitle <- paste0(subtitle,item$attributes[[i]]$name,": ",item$attributes[[i]]$value_name,"/")
    
    if(str_detect(tolower(item$attributes[[i]]$name),"sku")){
      sku <- item$attributes[[i]]$value_name
    }
  }
  
  producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE1"),paste0("sku=",sku))
  
  if(item$status!="paused"){
    status <- "Activo"
  }else{
    status <- "Inactivo" 
  }
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
  if(productos[[1]]=="activo"){
    contar <- airtable_getrecordslist("solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),
                                      "AND({empacado}='',{empaque_terminado}='',{prioridad}='8 - Antes de la 1',FIND('2000',{comentarios}))")
    #contar <- airtable_getrecordslist("solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),"AND(FIND('VP17600 - 10914 - recibidor 100x30 - 25mm negro',{venta_producto}))")
    if(length(contar)!=0){
      contar_productos <- sapply(contar, function(x){
        if(!str_detect(x$fields$producto_solicitado,"caja")){
          x$fields$producto_solicitado
        }
      },simplify = T)
      contar_cajas <- sapply(contar, function(x){
        if(str_detect(x$fields$producto_solicitado,"caja")){
          x$fields$producto_solicitado
        }
      },simplify = T)
      contar_productos <- quitar_null(contar_productos)
      if(length(contar_productos)>6){
        
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
                                                   "AND({canal}='mercadolibre randu',{tipo_envio}='Agencia',{status}='Activo')")
  productos <- sapply(publicaciones_agencia,function(x){
    producto <- airtable_getrecorddata_byid(x$fields$producto[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
    if(length(producto$fields$partes_producto)<=1 && !is.null(producto$fields$item_produccion)){
      return(x$fields$id_canal)
    }
  })
  productos <- quitar_null(productos)
  productos <- append("activo",productos)
  saveRDS(productos,"publicaciones_a_pausar.RDS")
  return(productos)
}

ml_status_publicacion_agencia <- function(ml_token,status){
  productos  <- readRDS("publicaciones_a_pausar.RDS")
  if(length(productos)!=0){
    for(i in productos[-1]){
      item <- ml_status_item(productos[[i]],ml_token,status)
      if(!last_response()$status_code %in% c(199:299) ){
        causa <- toJSON(last_response() %>% resp_body_json())
        item_no_cambio <- ml_obtener_item(productos[[i]],ml_token)
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