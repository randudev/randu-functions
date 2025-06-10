
facturapi_obtener_facturas <- function(auth_facturapi,fecha=NULL,filters=NULL) {
  # Inicializar la página de búsqueda
  page <- 1
  facturas_totales <- list()  # Lista para almacenar todas las facturas
  
  while(TRUE) {
    if(is.null(fecha)){
      url <- paste0("https://www.facturapi.io/v2/invoices", "?page=", page)  # 'page_size' es el número de resultados por página
    }
    else{
      url <- paste0("https://www.facturapi.io/v2/invoices","?date[gte]=",fecha, "&page=", page)  # 'page_size' es el número de resultados por página
    }
    if(!is.null(filters)){
      url <- paste0(url,"&",filters)
    }
    
    response <- request(url) %>%
      req_method("GET") %>% 
      req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
      req_perform() 
    
    result <-response  %>% resp_body_json() 
    
    if (length(result$data) == 0) {
      break  
    }
    
    # Agregar las facturas obtenidas a la lista
    facturas_totales <- append(facturas_totales, result$data)
    
    # Incrementar la página para obtener la siguiente
    page <- page + 1
  }
  
  return( facturas_totales)  # Devolver la lista con todas las facturas
}

facturapi_id_factura <- function(auth_facturapi,id){
  url <-  paste0("https://www.facturapi.io/v2/invoices/",id)
  response <- request(url) %>%
    req_method("GET") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_perform() %>% 
    resp_body_json()
}

facturapi_id_recibo <- function(auth_facturapi,id){
  url <-  paste0("https://www.facturapi.io/v2/receipts/",id)
  response <- request(url) %>%
    req_method("GET") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_perform() %>% 
    resp_body_json()
}

facturapi_descargar_factura <- function(id,auth_facturapi,formato="pdf"){
  res<-request(paste0("https://www.facturapi.io/v2/invoices/",id,"/",formato)) %>% 
    req_method("GET") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() 
}

facturapi_enviar_recibo <- function(id,auth_facturapi,email){
  res<-request(paste0("https://www.facturapi.io/v2/receipts/",id,"/","email")) %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi),
                'Content-Type'='application/json') %>%
    req_body_json(list("email"=email)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() 
}

facturapi_cancelar_recibo <- function(id,auth_facturapi){
  url <- paste0("https://www.facturapi.io/v2/receipts/",id)
  res<-request(url) %>% 
    req_method("DELETE") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() 
}

facturapi_crear_recibo <- function(orden,auth_facturapi,id_orden,canal_venta,tipo_de_pago="31"){
  items <- datos_recibo(canal_venta,orden,id_orden)
  
  items_json <- sapply(items, function(producto) {
    generar_json(producto$cantidad, producto$nombre, producto$producto_key, producto$precio, producto$sku,producto$descuento)
  })
  recibo <- paste0('{"payment_form": "',tipo_de_pago,'", "items": [', paste(items_json, collapse = ","), ']}')
  recibo_lista <- fromJSON(recibo)
  recibo_lista <- append(recibo_lista,list("external_id"=id_orden))
  
  res1<-request("https://www.facturapi.io/v2/receipts") %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi))  %>% 
    req_headers('Content-type'='application/json') %>%
    req_body_json(recibo_lista) %>%
    req_perform() %>%
    resp_body_json()
}

datos_recibo <- function(canal_venta,orden,id_orden){
  items_orden <- list()
  
  if(canal_venta=="ml"){
    for(i in 1:length(orden$order_item)){
      items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = orden$order_items[[1]]$item$title,
        "precio"= orden$order_items[[1]]$unit_price,
        "sku"=orden$order_items[[1]]$item$seller_sku,
        "cantidad" = orden$order_items[[1]]$quantity 
      )  
      
    }
  }
  if(canal_venta=="amz"){
    for(item in orden$payload$OrderItems){
      descuento <- NULL
      precio <- as.numeric(item$ItemPrice$Amount)
      if(!is.null(item$PromotionDiscount$Amount)){
        if(as.numeric(item$PromotionDiscount$Amount)!=0){
          descuento <- item$PromotionDiscount$Amount
        }
      }
      sku <- str_extract(item$SellerSKU,"\\d+")
      if(!is.null(item$ItemTax$Amount)){
        if(!is.na(item$ItemTax$Amount)){
          precio <- precio + as.numeric(item$ItemTax$Amount)
        }
      }
      items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = item$Title,
        "precio" = precio,
        "sku"=sku,
        "cantidad" = item$QuantityOrdered,
        "descuento"=descuento
      )  
    }
  }
  if(canal_venta == "shp"){
    for(i in 1:length(orden$line_items$id)){
      descuento <- orden$line_items$discount_allocations[[i]]$amount
      items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = orden$line_items$name[[i]],
        "precio"= as.numeric(orden$line_items$price[[i]]) - as.numeric(ifelse(is.null(descuento)||is.na(descuento), 0, descuento))/orden$line_items$quantity[[i]],
        "sku"=orden$line_items$sku[[i]],
        "cantidad" = orden$line_items$quantity[[i]],
        "descuento" = NULL
      )  
      
      
      if(length(orden$shipping_lines)!=0){
        if(i == length(orden$line_items$id) && as.numeric(orden$shipping_lines$price[[1]])!=0){
          items_orden[[length(items_orden) + 1]] <- list(
            "nombre" = orden$shipping_lines$title[[1]],
            "precio"= sum(as.numeric(orden$shipping_lines$price),na.rm=TRUE),
            "sku"=1000,
            "cantidad" = 1,
            "descuento" = NULL
          )  
        }
      }
    }
  }
  for(i in 1:length(items_orden)){
    if(!is.null(items_orden[[i]]$sku)){
      if(!is.na(items_orden[[i]]$sku) && str_detect(items_orden[[i]]$sku,"^\\d\\d\\d\\d\\d$") ){
        if(items_orden[[i]]$sku!=1000){
          producto<-airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),
                                            paste0("sku=",items_orden[[i]]$sku))
          if(length(producto)!=0){
            if(!is.null(producto)){
              producto_key <- producto[[1]]$fields$clave_sat 
            }else{
              producto_key <- NULL
            }
          }else{
            producto_key <- NULL
          }
        }else{
          producto_key <- 78101800
        }
        
      }else{
        producto_key <- NULL
      }
    }else{
      producto_key <- NULL
    }
    if(!is.null(producto_key)){
      items_orden[[i]]$producto_key <- producto_key
    }else{
      items_orden[[i]]$producto_key <- 56101703
    }
  }
  
  return(items_orden)
}

generar_json <- function(cantidad, descripcion, product_key, precio, sku,descuento=NULL) {
  if(is.null(sku)){
    sku <- 10700
  }else{
    if(is.na(sku)){
      sku <- 10700
    }
  }
  if(sku !=1000){
    if(is.null(descuento)){
      json <- paste0('{
                  "quantity": ', cantidad, ',
                  "product": {
                    "description": "', descripcion, '",
                    "product_key": "', product_key, '",
                    "price": ', precio, ',
                    "sku": "', sku, '"
                  }
                }')
    }else{
      json <- paste0('{
                  "quantity": ', cantidad, ',
                  "product": {
                    "description": "', descripcion, '",
                    "product_key": "', product_key, '",
                    "price": ', precio, ',
                    "sku": "', sku, '"
                  },
                  "discount": ',descuento,'
                }')
    }
  }else{
    json <- paste0('{
                  "quantity": ', cantidad, ',
                  "product": {
                    "description": "', descripcion, '",
                    "product_key": "', product_key, '",
                    "price": ', precio, '
                  }
                }')
  }
  
  return(json)
}

facturapi_obtener_recibos <- function(auth_facturapi,fecha=NULL,filters=NULL){
  # Inicializar la página de búsqueda
  page <- 1
  recibos_totales <- list()  # Lista para almacenar todas las facturas
  
  while(TRUE) {
    if(is.null(fecha)){
      url <- paste0("https://www.facturapi.io/v2/receipts", "?page=", page)  # 'page_size' es el número de resultados por página
    }
    else{
      url <- paste0("https://www.facturapi.io/v2/receipts","?date[gte]=",fecha, "&page=", page)  # 'page_size' es el número de resultados por página
    }
    if(!is.null(filters)){
      url <- paste0(url,"&",filters)
    }
    
    response <- request(url) %>%
      req_method("GET") %>% 
      req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
      req_perform() 
    
    result <-response  %>% resp_body_json() 
    
    if (length(result$data) == 0) {
      break  
    }
    
    # Agregar las facturas obtenidas a la lista
    recibos_totales <- append(recibos_totales, result$data)
    
    # Incrementar la página para obtener la siguiente
    page <- page + 1
  }
  return( recibos_totales)  # Devolver la lista con todas las facturas
}

facturapi_descargar_recibos <- function(id,auth_facturapi){
  res<-request(paste0("https://www.facturapi.io/v2/receipts/",id,"/pdf")) %>% 
    req_method("GET") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() 
}

facturapi_crear_factura_recibo <- function(auth_facturapi,recibo,mes,serie,date){
  res<-request(paste0("https://www.facturapi.io/v2/receipts/global-invoice")) %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_body_json(list(
      "from"= recibo$date,
      "to"=   recibo$created_at,
      "receipts"= list(
        recibo$id
      ),
      "date"=date,
      "months" = mes,
      "periodicity"= "day",
      "series"=serie
    )) %>% 
    req_error(is_error = function(res) FALSE) %>%
    req_perform() 
}

registrar_recibo <- function(recibo,orden_venta=NULL){
  if (!dir.exists("~/recibos")) {
    dir.create("~/recibos", recursive = TRUE)
  }
  helper <- ""
  productos <- list()
  for(j in 1:length(recibo$items)){
    helper <- paste0(helper,recibo$items[[j]]$product$description,"\n")
    if(!is.null(recibo$items[[j]]$product$sku)){
      sku <- str_extract(recibo$items[[j]]$product$sku,"\\d+")
      if(!is.na(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
        producto<-airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),
                                          paste0("sku=",sku))
        if(length(producto)!=0){
          productos <- append(productos,producto[[1]]$id) 
        }
      }
    }
  }
  
  fieldslist <- list(
    'id_recibo'=recibo$id,
    'monto'=recibo$total,
    'fecha_creacion'=recibo$created_at,
    'link_facturacion'=recibo$self_invoice_url,
    'helper_producto'=helper,
    'folio'=paste0(recibo$folio_number),
    'status_factura'='pendiente de facturar'
  )
  if(recibo$status=="open"){
    fieldslist <- append(fieldslist,list("status_recibo"="abierto"))
  }else{
    fieldslist <- append(fieldslist,list("status_recibo"="cerrado"))
  }
  if(length(productos)!=0){
    fieldslist <- append(fieldslist,list("producto"=unique(productos)))
  }
  if(length(orden_venta)!=0){
    fieldslist <- append(fieldslist,list("orden_venta"=list(orden_venta$id)))
  }
  resp <- airtable_createrecord(fieldslist,"recibos",Sys.getenv("AIRTABLE_CES_BASE"))
  while(TRUE){
    pdf <- facturapi_descargar_recibos(recibo$id,Sys.getenv("FACTURAPI_KEY"))
    if(pdf$status_code %in% c(199:299)){
      writeBin(pdf$body, paste0("~/recibos/",recibo$id,".pdf"))
      airtable_subir_pdf(resp$id,paste0("~/recibos/",recibo$id,".pdf"),"pdf_recibo",Sys.getenv("AIRTABLE_CES_BASE"),"pdf")
      break
    }
    if(i>=10){
      i <- 0
      break
    }
    Sys.sleep(1)
    i <- i + 1
  }
}

revisar_recibo <- function(recibos){
  ml_token <- get_active_token()
  amz_token <- amz_get_active_token()
  ml_token_asm <- get_active_token("recQLtjnMhd4ZCiJq")
  shp_token <- Sys.getenv("SHOPIFY-RANDUMX-TK")
  recibos_validos <- list()
  fecha_final <- as.Date(format(Sys.Date(), "%Y-%m-01"))-1
  #fecha_final <- as.Date(format(fecha_final,"%Y-%m-31"))
  fecha_inicio <- as.Date(format(fecha_final, "%Y-%m-01"))
  for(i in seq_along(recibos)){
    if(i%%5==0){
      print(i)
    }
    fecha_final <- as.Date(format(Sys.Date(), "%Y-%m-01"))-1
    if(is.null(recibos[[i]]$fields$canal_venta)){print(recibos[[i]]$fields)}
    
    if(recibos[[i]]$fields$canal_venta=="shprndmx"){
      fecha_final <- as.Date(format(fecha_final,"%Y-%m-28"))
    }
    if(recibos[[i]]$fields$fecha_creacion>fecha_inicio && recibos[[i]]$fields$fecha_creacion<fecha_final){
      if(recibos[[i]]$fields$canal_venta=="mercadolibrernd"){
        orden_venta <- airtable_getrecorddata_byid(recibos[[i]]$fields$orden_venta[[1]],"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
        ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
        if(ml_order$status!="cancelled"){
          if(ml_order$total_amount== recibos[[i]]$fields$monto){
            recibos_validos[[length(recibos_validos) + 1]] <- recibos[[i]]
          }
        }else{
          airtable_updatesinglerecord(list("cancelada"=TRUE),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),recibos[[i]]$fields$orden_venta[[1]])
        }
      }
      if(recibos[[i]]$fields$canal_venta == "amazonrnd"){
        orden_venta <- airtable_getrecorddata_byid(recibos[[i]]$fields$orden_venta[[1]],"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
        amz_order <- get_amzorder_byid(orden_venta$fields$id_origen,amz_token)
        if(amz_order$payload$OrderStatus != "Canceled"){
          if(as.numeric(amz_order$payload$OrderTotal$Amount)== as.numeric(recibos[[i]]$fields$monto)){
            recibos_validos[[length(recibos_validos) + 1]] <- recibos[[i]]
          }
        }else{
          airtable_updatesinglerecord(list("cancelada"=TRUE),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),recibos[[i]]$fields$orden_venta[[1]])
        }
      }
      if(recibos[[i]]$fields$canal_venta == "shprndmx"){
        orden_venta <- airtable_getrecorddata_byid(recibos[[i]]$fields$orden_venta[[1]],"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
        if(!str_detect(orden_venta$fields$id_ordenes_venta,"DST")){
          #orden_venta <- airtable_getrecorddata_byid(recibos[[i]]$fields$orden_venta,"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
          shp_order <- consulta_por_nombre(orden_venta$fields$id_origen,shp_token)
          if(is.null(shp_order$data$orders$edges[[1]]$node$cancelledAt)){
            if(as.numeric(shp_order$data$orders$edges[[1]]$node$totalPriceSet$shopMoney$amount) == as.numeric(recibos[[i]]$fields$monto)){
              recibos_validos[[length(recibos_validos) + 1]] <- recibos[[i]]
            }
          }else{
            airtable_updatesinglerecord(list("cancelada"=TRUE),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),recibos[[i]]$fields$orden_venta[[1]])
          }
        }
      }
      if(recibos[[i]]$fields$canal_venta == "mercadolibreasm"){
        orden_venta <- airtable_getrecorddata_byid(recibos[[i]]$fields$orden_venta[[1]],"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
        ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token_asm)
        if(ml_order$status!="cancelled"){
          if(ml_order$total_amount== recibos[[i]]$fields$monto){
            recibos_validos[[length(recibos_validos) + 1]] <- recibos[[i]]
          }
        }else{
          airtable_updatesinglerecord(list("cancelada"=TRUE),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),recibos[[i]]$fields$orden_venta[[1]])
        }
      }
    }
  }
  return(recibos_validos)
}

cambiar_recibo <- function(id_origen,forma_pago){
  if(str_detect(id_origen,"SHP")){
    id_origen <- trimws(id_origen)
    noti <- supabase_getrecordslist("notificaciones",,paste0('body=ilike.*',id_origen,'*')) %>% bind_rows()
    if(length(noti)==0){
      return(0)
    }
    orden_shopify <- subset(noti,str_detect(headers,"orders/create"))
    shopifyorden <- fromJSON(orden_shopify$body[[i]])
    ov_shopi <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_origen='",id_origen,"'"))[[1]]
    recibo <- facturapi_crear_recibo(shopifyorden,Sys.getenv("FACTURAPI_KEY"),shopifyorden$name,"shp",forma_pago)
    registrar_recibo(recibo,ov_shopi)
    airtable_updatesinglerecord(list("id_recibo"=recibo$id),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),ov_shopi$id)
    
  }else{
    if(str_detect(id_origen,"-")){
      amz_token<- amz_get_active_token()
      amz_order <- get_amzorder_byid(id_origen,amz_token)
      ov_amz <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_origen='",id_origen,"'"))[[1]]
      amz_order_items <- get_amzorderitem_byid(id_origen,amz_token)
      recibo <- facturapi_crear_recibo(amz_order_items,Sys.getenv("FACTURAPI_KEY"),id_origen,"amz",forma_pago)
      registrar_recibo(recibo,ov_amz)
      airtable_updatesinglerecord(list("id_recibo"=recibo$id),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),ov_amz$id)
    }else{
      orden_venta <-  airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_origen='",id_origen,"'"))
      if(length(orden_venta)==0){
        orden_venta airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("ml_pack_id='",id_origen,"'"))
      }
      if(length(orden_venta) != 0){
        orden_venta <- orden_venta[[1]]
      }
      
      noti <- supabase_getrecordslist("notificaciones",,paste0('body=ilike.*',orden_venta,'*')) %>% bind_rows()
      cuerpo <- fromJSON(noti$body[[1]])
      if(cuerpo$user_id == Sys.getenv("SELLERID_ML_ASM")){
        recordid_token <- "recQLtjnMhd4ZCiJq"
        canal <- "mercadolibreasm"
      }else{
        recordid_token <- ""
        canal <- NULL
      }
      ml_token <- get_active_token(recordid_token)
      ml_order <- get_mlorder_byid(id_origen)
      
      recibo <- facturapi_crear_recibo(ml_order,Sys.getenv("FACTURAPI_KEY"),paste0(ml_order$id),"ml",forma_pago)
      registrar_recibo(recibo,orden_venta)
      airtable_updatesinglerecord(list("id_recibo"=recibo$id),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),orden_venta$id)
    }
  }
  return(recibo)
}