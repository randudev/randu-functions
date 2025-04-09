library(httr2)
library(jsonlite)

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

facturapi_crear_recibo <- function(orden,auth_facturapi,id_orden,canal_venta){
  items <- datos_recibo(canal_venta,orden,id_orden)
  
  items_json <- sapply(items, function(producto) {
    generar_json(producto$cantidad, producto$nombre, producto$producto_key, producto$precio, producto$sku,producto$descuento)
  })
  recibo <- paste0('{"payment_form": "31", "items": [', paste(items_json, collapse = ","), ']}')
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
        if(!is.null(item$PromotionDiscount$Amount)){
          if(as.numeric(item$PromotionDiscount$Amount)!=0){
            descuento <- item$PromotionDiscount$Amount
          }
        }
        sku <- str_extract(item$SellerSKU,"\\d+")
        items_orden[[length(items_orden) + 1]] <- list(
          "nombre" = item$Title,
          "precio"= item$ItemPrice$Amount,
          "sku"=sku,
          "cantidad" = item$QuantityOrdered,
          "descuento"=descuento
        )  
      }
    }
    if(canal_venta == "shp"){
      for(i in 1:length(orden$line_items$id)){
        items_orden[[length(items_orden) + 1]] <- list(
          "nombre" = orden$line_items$name[[i]],
          "precio"= orden$line_items$price[[i]],
          "sku"=orden$line_items$sku[[i]],
          "cantidad" = orden$line_items$quantity[[i]],
          "descuento" = orden$line_items$discount_allocations[[i]]$amount
        )  
      }
    }
  for(i in 1:length(items_orden)){
    if(!is.null(items_orden[[i]]$sku)){
      if(!is.na(items_orden[[i]]$sku) && str_detect(items_orden[[i]]$sku,"^\\d\\d\\d\\d\\d$") ){
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

facturapi_crear_factura_recibo <- function(auth_facturapi){
  res<-request(paste0("https://www.facturapi.io/v2/receipts/global-invoice")) %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_body_json(list(
      "from"= "2025-04-02T01:00:00.000Z",
      "to"=   "2025-04-02T23:00:00.000Z",
      "receipts"= list(
        recibo$id
      ),
      "periodicity"= "month"
      
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
    fieldslist <- append(fieldslist,list("producto"=productos))
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
