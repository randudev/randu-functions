
facturapi_obtener_facturas <- function(auth_facturapi,fecha=NULL,fecha_final=NULL,filters=NULL) {
  # Inicializar la página de búsqueda
  page <- 1
  facturas_totales <- list()  # Lista para almacenar todas las facturas
  
  while(TRUE) {
    if(is.null(fecha)){
      url <- paste0("https://www.facturapi.io/v2/invoices", "?page=", page)  # 'page_size' es el número de resultados por página
    }
    else{
      if(is.null(fecha_final)){
        fecha_final <- Sys.Date()
      }
      url <- paste0("https://www.facturapi.io/v2/invoices","?date[gt]=",fecha, "&date[lte]=",
                    as.character(fecha_final),"&page=", page)  
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

facturapi_crear_recibo <- function(orden,auth_facturapi,id_orden,canal_venta,tipo_de_pago="31",omitir=""){
  items <- datos_recibo(canal_venta,orden,id_orden,omitir)
  if(tipo_de_pago == "31"){
    tipo_de_pago <- tipo_pago_real(orden,canal_venta)
  }
  items_json <- sapply(items, function(producto) {
    generar_json(producto$cantidad, producto$nombre, producto$producto_key, producto$precio, producto$sku,producto$descuento)
  })
  recibo <- paste0('{"payment_form": "',tipo_de_pago,'", "items": [', paste(items_json, collapse = ","), ']}')
  recibo_lista <- fromJSON(recibo)
  if(length(id_orden)>1){
    id_orden <- id_orden[[1]]
  }
  recibo_lista <- append(recibo_lista,list("external_id"=id_orden))
  
  res1<-request("https://www.facturapi.io/v2/receipts") %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi))  %>% 
    req_headers('Content-type'='application/json') %>%
    req_body_json(recibo_lista) %>%
    req_perform() %>%
    resp_body_json()
}

facturapi_cambiar_factura <- function(orden,auth_facturapi,id_orden,canal_venta,tipo_de_pago="31",omitir="",
                                      datos_direccion=NULL,mes='',fecha=NULL,factura_anterior=list(),series="Randu"){
  items <- datos_recibo(canal_venta,orden,id_orden,omitir)
  if(tipo_de_pago == "31"){
    tipo_de_pago <- tipo_pago_real(orden,canal_venta)
  }
  items_json <- sapply(items, function(producto) {
    generar_json(producto$cantidad,  gsub('"', "\\'", producto$nombre), producto$producto_key, producto$precio, producto$sku,producto$descuento)
  })
  recibo <- paste0('{"payment_form": "',tipo_de_pago,'", "items": [', paste(items_json, collapse = ","), ']}')
  recibo_lista <- fromJSON(recibo)
  if(length(id_orden)>1){
    id_orden <- id_orden[[1]]
  }
  recibo_lista <- append(recibo_lista,list("external_id"=id_orden))
  recibo_lista$series <- series
  if(length(datos_direccion)==0){
    datos_direccion <- list(
      "legal_name"="PUBLICO EN GENERAL",
      "tax_id"="XAXX010101000",
      "tax_system"="616",
      "address"=list(zip='76113')
      
    )
    global <- list(
      "periodicity"="day",
      "months"="03",
      "year"="2026"
      )
    recibo_lista$global <- global
    recibo_lista$customer <- datos_direccion
    related_documents <- list("relationship"="04",
                              "documents"=factura_anterior)
    recibo_lista$related_documents <- list(related_documents)
  }
  return(recibo_lista)
}

facturapi_cancelar_factura <- function(invoice_id, auth_facturapi, motive, substitution = NULL) {
  
  url <- paste0("https://www.facturapi.io/v2/invoices/", invoice_id)
  
  req <- request(url) %>% 
    req_method("DELETE") %>% 
    req_headers(Authorization = paste0("Bearer ", auth_facturapi)) %>%
    req_url_query(motive = motive)
  
  # Agregar substitution solo si existe
  if (!is.null(substitution)) {
    req <- req %>%  req_url_query(substitution = substitution)
  }
  
  response <- req %>%  req_perform()
  
  return(resp_body_json(response))
}

facturapi_crear_factura <- function(body,auth_facturapi){
  res1<-request("https://www.facturapi.io/v2/invoices") %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi))  %>% 
    req_headers('Content-type'='application/json') %>%
    req_body_json(body) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>%
    resp_body_json()
}

datos_recibo <- function(canal_venta,orden,id_orden,omitir=""){
  items_orden <- list()
  
  if(canal_venta=="ml"){
    for(i in 1:length(orden$order_item)){
      if(orden$order_items[[i]]$item$title %in% omitir){
        next
      }
      items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = orden$order_items[[i]]$item$title,
        "precio"= orden$order_items[[i]]$unit_price,
        "sku"=orden$order_items[[i]]$item$seller_sku,
        "cantidad" = orden$order_items[[i]]$quantity 
      )  
      
    }
  }
  if(canal_venta=="amz"){
    for(item in orden$payload$OrderItems){
      descuento <- NULL
      if(item$Title %in% omitir){
        next
      }
      if(!is.null(item$QuantityOrdered)){
        cantidad <- item$QuantityOrdered
      }else{
        cantidad <- 1
      }
      if(item$QuantityOrdered==0){
        next
      }
      precio <- as.numeric(item$ItemPrice$Amount)
      if(!is.null(item$PromotionDiscount$Amount)){
        if(as.numeric(item$PromotionDiscount$Amount)!=0){
          descuento <- as.numeric(item$PromotionDiscount$Amount)/cantidad
        }
      }
      sku <- str_extract(item$SellerSKU,"\\d+")
      if(!is.null(item$ItemTax$Amount)){
        if(!is.na(item$ItemTax$Amount)){
          precio <- (precio + as.numeric(item$ItemTax$Amount))/cantidad
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
  if(canal_venta == "amz_v2"){
    for(item in orden$order$orderItems){
      descuento <- NULL
      if(item$product$title %in% omitir){
        next
      }
      if(!is.null(item$quantityOrdered)){
        cantidad <- item$quantityOrdered
      }else{
        cantidad <- 1
      }
      precio <- as.numeric(item$product$price$unitPrice$amount)/cantidad
      # if(!is.null(item$product$condition$conditionType)){
      #   if(as.numeric(item$PromotionDiscount$Amount)!=0){
      #     descuento <- as.numeric(item$PromotionDiscount$Amount)/cantidad
      #   }
      # }
      
      sku <- str_extract(item$product$sellerSku,"\\d+")
      # if(!is.null(item$ItemTax$Amount)){
      #   if(!is.na(item$ItemTax$Amount)){
      #     precio <- precio + as.numeric(item$ItemTax$Amount)/cantidad
      #   }
      # }
      items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = item$product$title,
        "precio" = precio,
        "sku"=sku,
        "cantidad" = cantidad,
        "descuento"=descuento
      )  
    }
  }
  if(canal_venta == "shp"){
    for(i in 1:length(orden$line_items$id)){
      descuento <- orden$line_items$discount_allocations[[i]]$amount
      discount <- as.numeric(ifelse(is.null(descuento)||is.na(descuento), 0, descuento))/orden$line_items$quantity[[i]]
      precio <- as.numeric(orden$line_items$price[[i]]) - discount 
      if(precio>0){
        descuento_real <- NULL
      }else{
        descuento_real <- 1.16 *orden$line_items$quantity[[i]]
      }
      items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = orden$line_items$name[[i]],
        "precio"= ifelse(precio>0,precio,1.16),
        "sku"=orden$line_items$sku[[i]],
        "cantidad" = orden$line_items$quantity[[i]],
        "descuento" = descuento_real
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
  if(canal_venta == "directa"){
    ventas_producto <- airtable_getrecordslist("ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),paste0("FIND('",id_orden,"',{ordenes_venta})"))
    for(i in 1:length(ventas_producto)){
      producto <- airtable_getrecorddata_byid(ventas_producto[[i]]$fields$producto[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
      precio <- ventas_producto[[i]]$fields$precio_unitario_con_descuento
      if(is.null(precio) || precio <= 0){
        precio <- 1.16
        descuento_real <- 1.16 * ventas_producto[[i]]$fields$cantidad
      }else{
        descuento_real <- NULL
      }
      items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = ventas_producto[[i]]$fields$helper_productname,
        "precio"= precio,
        "sku"=paste0(producto$fields$sku),
        "cantidad" = ventas_producto[[i]]$fields$cantidad,
        "producto_key"=producto$fields$clave_sat,
        "descuento" = descuento_real
      )  
    }
  }
  if(canal_venta == "directaunir"){
    if(length(id_orden)>1){
      for(j in seq_along(id_orden)){
        ventas_producto <- airtable_getrecordslist("ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),paste0("FIND('",id_orden[[j]],"',{ordenes_venta})"))
        for(i in 1:length(ventas_producto)){
          producto <- airtable_getrecorddata_byid(ventas_producto[[i]]$fields$producto[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
          precio <- ventas_producto[[i]]$fields$precio_unitario_con_descuento
          if(is.null(precio) || precio <= 0){
            precio <- 1.16
            descuento_real <- 1.16 * ventas_producto[[i]]$fields$cantidad
          }else{
            descuento_real <- NULL
          }
          
          items_orden[[length(items_orden) + 1]] <- list(
            "nombre" = ventas_producto[[i]]$fields$helper_productname,
            "precio"= precio,
            "sku"=paste0(producto$fields$sku),
            "cantidad" = ventas_producto[[i]]$fields$cantidad,
            "producto_key"=producto$fields$clave_sat,
            "descuento" = descuento_real
          )  
        }
      }
    }
    
  }
  if(canal_venta == "shp1"){
    orden <- orden$data$orders$edges[[1]]$node
    if(!is.null(orden)){
      for(i in 1:length(orden$lineItems$edges)){
        if(orden$lineItems$edges[[i]]$node$currentQuantity==0){
          next
        }
        if(orden$lineItems$edges[[i]]$node$name %in% omitir){
          next
        }
        if(length(orden$lineItems$edges[[i]]$node$discountAllocations)!=0){
          descuento <- orden$lineItems$edges[[i]]$node$discountAllocations[[1]]$allocatedAmountSet$shopMoney$amount
          discount <- as.numeric(ifelse(is.null(descuento)||is.na(descuento), 0, descuento))/orden$lineItems$edges[[i]]$node$quantity
        }else{
          discount <- 0
        }
        
        precio <- orden$lineItems$edges[[i]]$node$originalUnitPriceSet$shopMoney$amount
        precio <- as.numeric(ifelse(is.null(precio)||is.na(precio), 0, precio)) - discount 
        
        if(precio>0){
          descuento_real <- NULL
        }else{
          descuento_real <- 1.16 *orden$lineItems$edges[[i]]$node$quantity
        }
        items_orden[[length(items_orden) + 1]] <- list(
          "nombre" = orden$lineItems$edges[[i]]$node$name,
          "precio"= ifelse(precio>0,precio,1.16),
          "sku"=orden$lineItems$edges[[i]]$node$sku,
          "cantidad" =orden$lineItems$edges[[i]]$node$currentQuantity,
          "descuento" = descuento_real
        )  
       
        
        if(length(orden$shippingLines$edges)!=0){
          if(i == length(orden$shippingLines$edges) && as.numeric(orden$shippingLines$edges[[1]]$node$discountedPriceSet$shopMoney$amount)!=0){
            items_orden[[length(items_orden) + 1]] <- list(
              "nombre" = orden$shippingLines$edges[[1]]$node$title,
              "precio"= sum(as.numeric(orden$shippingLines$edges[[1]]$node$discountedPriceSet$shopMoney$amount),na.rm=TRUE),
              "sku"=1000,
              "cantidad" = 1,
              "descuento" = NULL
            )  
            
          }
        }
        
      }
    }
    
  }
  
  if(canal_venta != "directa" && canal_venta !="directaunir"){
    
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
    
  }
  items_orden <- lapply(items_orden, function(x) {
    x$nombre <- paste(id_orden, x$nombre)
    x
  })
  return(items_orden)
}

generar_json <- function(cantidad, descripcion, product_key, precio, sku,descuento=NULL) {
  if(is.null(sku)){
    sku <- 10700
  }else{
    if(is.na(sku)){
      sku <- 10700
    }else{
      if(sku==""){
        sku <- 10700
      }
    }
  }
  if(is.null(product_key)){
    product_key <- 56101703
  }else{
    if(is.na(product_key)){
      product_key <- 56101703
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
    url <- URLencode(url)
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
      #"to"=   recibo$created_at,
      "to"=   recibo$date,
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

registrar_factura <-function(factura,orden_venta,orden_supabase=list()) {
  if (length(factura) != 0 ) {
    pagada <- FALSE
    if(factura$amount_due==0){
      pagada <- TRUE
    }
    subtotal <- 0
    tax <- 0
    
    if(!is.null(factura$items)){
      
      for(item in factura$items){
        if(length(item$product$taxes)!=0){
          if(!is.null(item$product$taxes[[1]]$base)){
            subtotal <-subtotal + item$product$taxes[[1]]$base*item$quantity
            for(k in 1:length(item$product$taxes)){
              if(!item$product$taxes[[k]]$withholding){
                tax <- tax + item$product$taxes[[k]]$base * item$product$taxes[[k]]$rate*item$quantity
              }else{
                tax <- tax - item$product$taxes[[k]]$base * item$product$taxes[[k]]$rate*item$quantity
              }
            }
          }
          else{
            
            aux_rate <- sum(unlist(lapply(item$product$taxes,function(x) if(!x$withholding){x$rate}else{-1*x$rate})))
            if(aux_rate !=0)
            {
              subtotal <- subtotal + item$product$taxes[[1]]$rate/aux_rate
              tax <- tax + (item$product$taxes[[1]]$rate/aux_rate-item$product$taxes[[1]]$rate)
            }
            else{
              subtotal <- subtotal + item$product$taxes[[1]]$rate
            }
            
          }
        }
      }
    }
    if(length(subtotal)==0){
      subtotal <- 0
    }
    if(length(tax)==0){
      tax <- 0
    }
    if(length(factura$global)!=0){
      if(!is.null(factura$global$months)){
        mes <- as.numeric(factura$global$months)
      }else{
        mes <- as.numeric(format(as.Date(factura$date),"%m"))
      }
    }else{
      mes <- as.numeric(format(as.Date(factura$date),"%m"))
    }
    
    fields_factura <- list(
      'issued_at'=factura$date,
      'id_satws'=factura$id,
      'issuer_rfc'=factura$issuer_info$tax_id,
      'total'=as.numeric(factura$total),
      'uuid'=tolower(factura$uuid),
      'receiver_rfc'=factura$customer$tax_id,
      'receiver_name'=factura$customer$legal_name,
      'issuer_name'= factura$issuer_info$legal_name,
      'cancellation_status'=factura$cancellation_status,
      'status'=factura$status,
      'version'=paste0(factura$cfdi_version),
      'type'=factura$type,
      'usage'=factura$use,
      'payment_type'=factura$payment_method,
      'payment_method'=factura$payment_form,
      'currency'=factura$currency,
      'discount'=sum(unlist(lapply(factura$items,function(x) x$discount))),
      'exchange_rate'=factura$exchange,
      'serie'=factura$series,
      'subtotal'=subtotal,
      "mes"=mes,
      'tax'=tax,
      'por_pagar'=factura$amount_due,
      'place_of_issue'=factura$issuer_info$address$zip,
      'place_of_receiver'=factura$customer$address$zip,
      'monto_por_pagar'=factura$amount_due,
      "fecha_timbrado"=factura$stamp$date,
      'pagada'=pagada,
      'monto_pagado'=factura$total-factura$amount_due
    )
    if(length(factura$global)!=0){
      fields_factura$factura_global <- T
    }
    # if(!is.null(factura$external_id)){
    #   orden_venta <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_origen='",factura$external_id,"'"))
    #   if(length(orden_venta)!=0){
    #     fields_factura <- append(fields_factura,list("ordenes_venta"=list(orden_venta[[1]]$id)))
    #   }
    # }
    fields_factura <- append(fields_factura,list("ordenes_venta"=list(orden_venta$id)))
    if(!is.null(factura[["series"]])){
      if(factura[["series"]]=="ECOM" || factura[["series"]]=="SNG"){
        if(!is.null(factura[["folio_number"]])){
          orden_venta <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_origen='",factura[["folio_number"]],"'"))
          if(length(orden_venta)!=0){
            fields_factura <- append(fields_factura,list("ordenes_venta"=list(orden_venta[[1]]$id)))
          }
        }
      }
      if(factura[["series"]]=="MLRNFI"){
        orden_venta <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),
                                               paste0("factura_marketplace='",factura$uuid,"'"))
        if(length(orden_venta)!=0){
          fields_factura <- append(fields_factura,list("ordenes_venta"=list(orden_venta[[1]]$id)))
        }
      }
    }
    
    if(factura$currency=="MXN"){
      fields_factura <- append(fields_factura,list("subtotal_mxn"=subtotal,"total_mxn"=factura$total))
    }
    if(!is.null(factura$folio_number)){
      fields_factura <- append(fields_factura,list('folio'=as.character(factura$folio_number)))
    }
    resp <- airtable_createrecord(fields_factura,"cfdi",Sys.getenv("AIRTABLE_CES_BASE"))
    if(length(orden_supabase)!=0){
      fields_factura$record_id <- resp$id
      fields_factura$id_cfdi <- resp$fields$id_cfdi
      fields_factura$autonumber <- resp$fields$autonumber
      resp_supabase <- supabase_createrecord(fields_factura,"cfdi",Sys.getenv("SUPABASE_BASE_ID_CES"),
                                             Sys.getenv("AUTH_SUPABASE_CES"))
      resp_supabase <- resp_supabase %>% resp_body_json()
      if(last_response()$status_code %in% c(199:299)){
        actualizar <- list(ordenes_venta_id=orden_supabase[[1]]$id_auto,cfdi_id=resp_supabase[[1]]$id_auto)
        supabase_createrecord(
          fields = actualizar,
          tablename = "_nc_m2m_ordenes_venta_cfdi",
          base_id = Sys.getenv("SUPABASE_BASE_ID_CES"),apikey = Sys.getenv("SUPABASE_SECRET_ROLE")
        )
        #print(last_response())
      }
    }
    
    # recibos <- facturapi_obtener_recibos(Sys.getenv("FACTURAPI_KEY"),,paste0("external_id=",factura$external_id))
     id_recibo <- ""
    # for(recibo in recibos){
    #   if(!is.null(recibo$invoice)){
    #     if(recibo$invoice==factura$id){
    #       id_recibo <- recibo$id
    #     }
    #   }
    # }
    # if(id_recibo!=""){
    #   recibo_airtable <- airtable_getrecordslist("recibos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_recibo='",id_recibo,"'"))
    #   if(length(recibo_airtable)!=0){
    #     airtable_updatesinglerecord(list("cfdi"=list(resp$id),"status_factura"="facturado"),"recibos",Sys.getenv("AIRTABLE_CES_BASE"),recibo_airtable[[1]]$id)
    #   }
    # }
    i <- 0
    while(TRUE){
      pdf <- facturapi_descargar_factura(factura$id,Sys.getenv("FACTURAPI_KEY"),"pdf")
      if(pdf$status_code %in% c(199:299)){
        writeBin(pdf$body, paste0("~/facturas/",factura$uuid,".pdf"))
        airtable_subir_pdf(resp$id,paste0("~/facturas/",factura$uuid,".pdf"),"pdf_file",Sys.getenv("AIRTABLE_CES_BASE"),"pdf")
        
        if(length(orden_supabase)!=0){
          
          url <- subir_s3(factura$uuid,pdf$body,"pdf","cfdi/pdf_file")
          supabase_update(resp_supabase[[1]]$id_auto,list("pdf_file"=url),"cfdi",Sys.getenv("SUPABASE_BASE_ID_CES"),
                          Sys.getenv("AUTH_SUPABASE_CES"),index = "id_auto")
        }
        break
      }
      if(i>=10){
        i <- 0
        break
      }
      Sys.sleep(1)
      i <- i + 1
    }
    
    while(TRUE){
      xml <- facturapi_descargar_factura(factura$id,Sys.getenv("FACTURAPI_KEY"),"xml")
      if(xml$status_code %in% c(199:299)){
        writeBin(xml$body, paste0("~/facturas/",factura$uuid,".xml"))
        airtable_subir_pdf(resp$id,paste0("~/facturas/",factura$uuid,".xml"),"xml_file",Sys.getenv("AIRTABLE_CES_BASE"),"xml")
        if(length(orden_supabase)!=0){
          url_xml <- subir_s3(factura$uuid,xml$body,"xml","cfdi/xml_file")
          supabase_update(resp_supabase[[1]]$id_auto,list("xml_file"=url_xml),"cfdi",Sys.getenv("SUPABASE_BASE_ID_CES"),
                          Sys.getenv("AUTH_SUPABASE_CES"),index = "id_auto")
        }
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
}

revisar_recibo <- function(recibos){
  ml_token <- get_active_token()
  amz_token <- amz_get_active_token()
  amz_token_am <- amz_get_active_token("recMNZ3uARMZRBMnz")
  ml_token_asm <- get_active_token("recQLtjnMhd4ZCiJq")
  shp_token <- Sys.getenv("SHOPIFY-RANDUMX-TK")
  recibos_validos <- list()
  fecha_final <- as.Date(format(Sys.Date(), "%Y-%m-01"))-1
  #fecha_final <- as.Date(format(fecha_final,"%Y-%m-31"))
  fecha_inicio <- as.Date(format(fecha_final, "%Y-%m-01"))
  for(i in seq_along(recibos)){
    if(i%%5==0){
      print(i)
      Sys.sleep(.8)
    }
    fecha_final <- as.Date(format(Sys.Date(), "%Y-%m-01"))-1
    if(is.null(recibos[[i]]$fields$canal_venta)){print(recibos[[i]]$fields)}
    
    # if(recibos[[i]]$fields$canal_venta=="shprndmx"){
    #   fecha_final <- as.Date(format(fecha_final,"%Y-%m-28"))
    # }
    if(recibos[[i]]$fields$fecha_creacion>=fecha_inicio && recibos[[i]]$fields$fecha_creacion<=fecha_final){
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
      if(recibos[[i]]$fields$canal_venta == "amazonasm"){
        orden_venta <- airtable_getrecorddata_byid(recibos[[i]]$fields$orden_venta[[1]],"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
        amz_order <- get_amzorder_byid(orden_venta$fields$id_origen,amz_token_am)
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
      if(recibos[[i]]$fields$canal_venta == "directa"){
        recibos_validos[[length(recibos_validos) + 1]] <- recibos[[i]]
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
        orden_venta <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),paste0("ml_pack_id='",id_origen,"'"))
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

tipo_pago_real <- function(orden,canal_venta){
  if(canal_venta == "ml"){
    if(orden$payments[[1]]$payment_type == "credit_card"){
      return("04")
    }else{
      if(orden$payments[[1]]$payment_type == "debit_card"){
        return("28")
      }else{
        return("31")
      }
    }
  }else{
    if(canal_venta == "amz"){
      return("31")
    }else{
      return("31")
    }
    
  }
}

# ---------------------------------------------------------------------------
# Facturacion desde la base de Martin (tabla prefactura)
#
# Lee prefactura.type y arma el CFDI que corresponde:
#   I  Ingreso (factura normal, anticipo, factura total con anticipo, global)
#   E  Egreso (notas de credito: anticipo, devolucion, descuento)
#   P  Pago (complemento de pago)
#   N  Nomina
# Las relaciones de UUID (01-07) salen de la tabla related_documents y aplican a I y E.
# Antes de timbrar valida lo que el SAT va a rechazar, y deja el resultado
# (status, uuid, error_message...) en la misma prefactura.
# Regresa siempre una lista simple para que plumber la pueda convertir a JSON.
# ---------------------------------------------------------------------------

# "01-Efectivo" -> "01". NULL o vacio -> NULL
martin_codigo <- function(x){
  if(is.null(x) || length(x) == 0 || is.na(x[[1]]) || x[[1]] == "") return(NULL)
  sub("-.*", "", x[[1]])
}

# Quita los campos NULL, NA o "" para no mandarlos a Facturapi (las listas vacias se quedan)
martin_limpiar <- function(x){
  vacio <- vapply(x, function(v){
    is.null(v) || (!is.list(v) && length(v) == 1 && (is.na(v) || identical(v, "")))
  }, logical(1))
  x[!vacio]
}

martin_registro <- function(id, tabla){
  r <- airtable_getrecorddata_byid(id, tabla, Sys.getenv("AIRTABLE_MARTIN_BASE"))
  if(is.null(r)) stop(paste0("No se pudo leer el registro ", id, " de la tabla ", tabla))
  r$fields
}

martin_marcar <- function(recordid, campos){
  try(airtable_updatesinglerecord(campos, "prefactura", Sys.getenv("AIRTABLE_MARTIN_BASE"), recordid), silent = TRUE)
}

martin_customer <- function(f){
  martin_limpiar(list(
    legal_name = f$legal_name,
    tax_id = f$tax_id,
    tax_system = martin_codigo(f$tax_system),
    email = f$email,
    address = list(zip = f$zip)
  ))
}

martin_validar_customer <- function(customer){
  errores <- c()
  if(is.null(customer$legal_name)) errores <- c(errores, "Falta legal_name")
  if(is.null(customer$tax_id)) errores <- c(errores, "Falta tax_id (RFC)")
  if(is.null(customer$tax_system)) errores <- c(errores, "Falta tax_system")
  if(is.null(customer$address$zip)) errores <- c(errores, "Falta zip")
  errores
}

# Impuestos de un item o de un documento pagado, a partir de los registros ligados de taxes
martin_taxes <- function(tax_ids){
  lapply(tax_ids, function(tax_id){
    t <- martin_registro(tax_id, "taxes")
    martin_limpiar(list(
      type = t$type,
      rate = t$rate,
      factor = t$factor,
      withholding = !is.null(t$withholding) && t$withholding
    ))
  })
}

martin_items <- function(item_ids){
  errores <- c()
  items <- lapply(item_ids, function(item_id){
    p <- martin_registro(item_id, "items")
    nombre <- if(is.null(p$description)) item_id else substr(p$description, 1, 40)

    taxability <- martin_codigo(p$taxability)
    taxes <- martin_taxes(p$taxes)
    if(is.null(taxability)){
      taxability <- "02"
    }
    if(taxability == "01"){
      taxes <- list()
    }else if(length(taxes) == 0){
      taxes <- list(list(type = "IVA", rate = 0.16))
    }

    pedimentos <- NULL
    if(!is.null(p$customs_keys) && p$customs_keys != ""){
      pedimentos <- trimws(strsplit(p$customs_keys, "\n")[[1]])
      pedimentos <- pedimentos[pedimentos != ""]
      malos <- pedimentos[!grepl("^[0-9]{2}  [0-9]{2}  [0-9]{4}  [0-9]{7}$", pedimentos)]
      if(length(malos) > 0){
        errores <<- c(errores, paste0("Pedimento con formato incorrecto en '", nombre, "': ", paste(malos, collapse = ", "),
                                      ". Debe ser AA  AA  AAAA  AAAAAAA con dos espacios entre bloques"))
      }
      pedimentos <- as.list(pedimentos)
    }

    if(is.null(p$quantity)) errores <<- c(errores, paste0("Falta quantity en '", nombre, "'"))
    if(is.null(p$price)) errores <<- c(errores, paste0("Falta price en '", nombre, "'"))
    if(is.null(p$product_key)) errores <<- c(errores, paste0("Falta product_key en '", nombre, "'"))
    if(is.null(p$unit_key)) errores <<- c(errores, paste0("Falta unit_key en '", nombre, "'"))

    martin_limpiar(list(
      quantity = p$quantity,
      discount = p$discount,
      customs_keys = pedimentos,
      property_tax_account = if(is.null(p$property_tax_account)) NULL else as.list(p$property_tax_account),
      product = martin_limpiar(list(
        description = p$description,
        product_key = martin_codigo(p$product_key),
        unit_key = martin_codigo(p$unit_key),
        unit_name = if(is.null(p$unit_key)) NULL else sub("^[^-]*-", "", p$unit_key),
        price = p$price,
        tax_included = !is.null(p$tax_included) && p$tax_included,
        taxability = taxability,
        taxes = taxes,
        sku = p$sku
      ))
    ))
  })
  list(items = items, errores = errores)
}

# Agrupa las filas de related_documents por tipo de relacion, como lo pide Facturapi
martin_relaciones <- function(rel_ids){
  errores <- c()
  filas <- lapply(rel_ids, function(rel_id){
    r <- martin_registro(rel_id, "related_documents")
    uuid <- r$documents
    if((is.null(uuid) || uuid == "") && length(r$related_prefactura) > 0){
      uuid <- martin_registro(r$related_prefactura[[1]], "prefactura")$uuid
    }
    relacion <- martin_codigo(r$relationship)
    if(is.null(uuid) || uuid == ""){
      errores <<- c(errores, "Hay una relación sin UUID (ni en documents ni en la prefactura ligada, o esa prefactura no está timbrada)")
    }
    if(is.null(relacion)){
      errores <<- c(errores, paste0("Falta relationship para el UUID ", uuid))
    }
    list(relationship = relacion, uuid = trimws(uuid))
  })
  filas <- Filter(function(x) !is.null(x$relationship) && !is.null(x$uuid) && x$uuid != "", filas)
  tipos <- unique(vapply(filas, function(x) x$relationship, character(1)))
  relaciones <- lapply(tipos, function(tipo){
    uuids <- vapply(Filter(function(x) x$relationship == tipo, filas), function(x) x$uuid, character(1))
    list(relationship = tipo, documents = as.list(unique(uuids)))
  })
  list(relaciones = relaciones, errores = errores)
}

# I (ingreso) y E (egreso / nota de credito)
martin_cuerpo_ingreso_egreso <- function(f, tipo){
  errores <- martin_validar_customer(martin_customer(f))

  if(length(f$items) == 0) errores <- c(errores, "La prefactura no tiene items")
  armado_items <- martin_items(f$items)
  errores <- c(errores, armado_items$errores)

  armado_rel <- martin_relaciones(f$related_documents)
  errores <- c(errores, armado_rel$errores)

  forma <- martin_codigo(f$payment_form)
  metodo <- f$payment_method
  if(is.null(forma)) errores <- c(errores, "Falta payment_form")
  if(is.null(metodo)) errores <- c(errores, "Falta payment_method")
  if(is.null(martin_codigo(f$use))) errores <- c(errores, "Falta use")
  if(!is.null(metodo) && !is.null(forma)){
    if(metodo == "PPD" && forma != "99") errores <- c(errores, "Con PPD la forma de pago debe ser 99-Por definir")
    if(metodo == "PUE" && forma == "99") errores <- c(errores, "Con PUE la forma de pago no puede ser 99; pon la forma real")
  }
  if(tipo == "E"){
    if(length(armado_rel$relaciones) == 0) errores <- c(errores, "Una nota de crédito (E) debe relacionar al menos un UUID en related_documents")
    if(!is.null(metodo) && metodo == "PPD") errores <- c(errores, "Una nota de crédito (E) debe ir con PUE")
  }
  if(!is.null(f$currency) && f$currency != "MXN" && is.null(f$exchange)){
    errores <- c(errores, "Falta exchange (tipo de cambio) porque la moneda no es MXN")
  }

  global <- NULL
  if(!is.null(f$global_periodicity)){
    if(is.null(f$global_months) || is.null(f$global_year)){
      errores <- c(errores, "Factura global: faltan global_months o global_year")
    }
    global <- list(periodicity = f$global_periodicity,
                   months = martin_codigo(f$global_months),
                   year = f$global_year)
  }

  body <- martin_limpiar(list(
    type = tipo,
    customer = martin_customer(f),
    items = armado_items$items,
    payment_form = forma,
    payment_method = metodo,
    use = martin_codigo(f$use),
    currency = f$currency,
    exchange = f$exchange,
    conditions = f$conditions,
    series = f$series,
    folio_number = f$folio_number,
    date = f$date,
    export = martin_codigo(f$export),
    global = global,
    external_id = f$external_id
  ))
  if(length(armado_rel$relaciones) > 0) body$related_documents <- armado_rel$relaciones

  list(body = body, errores = errores)
}

# P (complemento de pago)
martin_cuerpo_pago <- function(f){
  errores <- martin_validar_customer(martin_customer(f))
  if(length(f$payments) == 0) errores <- c(errores, "El complemento de pago no tiene registros en payments")

  pagos <- lapply(f$payments, function(pago_id){
    p <- martin_registro(pago_id, "payments")
    forma <- martin_codigo(p$payment_form)
    if(is.null(forma)) errores <<- c(errores, "Un pago no tiene payment_form")
    if(!is.null(forma) && forma == "99") errores <<- c(errores, "Un pago no puede tener forma 99-Por definir")
    if(is.null(p$date)) errores <<- c(errores, "Un pago no tiene date")
    if(length(p$related_documents) == 0) errores <<- c(errores, "Un pago no tiene facturas en related_documents")
    if(!is.null(p$currency) && p$currency != "MXN" && is.null(p$exchange)){
      errores <<- c(errores, "Un pago en moneda distinta de MXN no tiene exchange")
    }

    documentos <- lapply(p$related_documents, function(doc_id){
      d <- martin_registro(doc_id, "payment_related_documents")
      uuid <- d$uuid
      if((is.null(uuid) || uuid == "") && length(d$related_prefactura) > 0){
        fac <- martin_registro(d$related_prefactura[[1]], "prefactura")
        uuid <- fac$uuid
        if(!is.null(fac$payment_method) && fac$payment_method != "PPD"){
          errores <<- c(errores, paste0("La factura ", uuid, " no es PPD; solo las PPD llevan complemento de pago"))
        }
      }
      if(is.null(uuid) || uuid == "") errores <<- c(errores, "Un documento pagado no tiene UUID")
      if(is.null(d$amount)) errores <<- c(errores, paste0("Falta amount para ", uuid))
      if(is.null(d$last_balance)) errores <<- c(errores, paste0("Falta last_balance para ", uuid))
      if(is.null(d$installment)) errores <<- c(errores, paste0("Falta installment para ", uuid))
      if(!is.null(d$amount) && !is.null(d$last_balance) && d$amount > d$last_balance + 0.01){
        errores <<- c(errores, paste0("El monto pagado es mayor que el saldo de ", uuid))
      }

      taxability <- martin_codigo(d$taxability)
      if(is.null(taxability)) taxability <- "02"
      taxes <- list()
      if(taxability != "01"){
        taxes <- martin_taxes(d$taxes)
        if(length(taxes) == 0) taxes <- list(list(type = "IVA", rate = 0.16))
        # base = monto / (1 + traslados - retenciones)
        factor_total <- 1 + sum(vapply(taxes, function(t){
          if(!is.null(t$withholding) && t$withholding) -t$rate else t$rate
        }, numeric(1)))
        base <- round(as.numeric(d$amount) / factor_total, 2)
        taxes <- lapply(taxes, function(t) c(list(base = base), t))
      }

      martin_limpiar(list(
        uuid = trimws(uuid),
        amount = d$amount,
        installment = d$installment,
        last_balance = d$last_balance,
        taxability = taxability,
        currency = d$currency,
        exchange = d$exchange,
        taxes = taxes
      ))
    })

    martin_limpiar(list(
      payment_form = forma,
      date = p$date,
      currency = p$currency,
      exchange = p$exchange,
      numOperacion = p$numOperacion,
      related_documents = documentos
    ))
  })

  body <- martin_limpiar(list(
    type = "P",
    customer = martin_customer(f),
    complements = list(list(type = "pago", data = pagos)),
    series = f$series,
    folio_number = f$folio_number,
    date = f$date,
    external_id = f$external_id
  ))
  list(body = body, errores = errores)
}

# N (nomina)
martin_cuerpo_nomina <- function(f){
  errores <- c()
  if(length(f$nomina_receptor) == 0){
    return(list(body = NULL, errores = "La nómina no tiene empleado en nomina_receptor"))
  }
  e <- martin_registro(f$nomina_receptor[[1]], "nomina_receptor")

  customer <- martin_limpiar(list(
    legal_name = e$legal_name,
    tax_id = e$tax_id,
    tax_system = "605",
    email = e$email,
    address = list(zip = e$zip)
  ))
  errores <- c(errores, martin_validar_customer(customer))
  if(is.null(e$curp)) errores <- c(errores, "Falta curp del empleado")
  if(is.null(f$fecha_pago)) errores <- c(errores, "Falta fecha_pago")
  if(is.null(f$fecha_inicial_pago)) errores <- c(errores, "Falta fecha_inicial_pago")
  if(is.null(f$fecha_final_pago)) errores <- c(errores, "Falta fecha_final_pago")
  if(is.null(f$num_dias_pagados)) errores <- c(errores, "Falta num_dias_pagados")

  conceptos <- lapply(f$nomina_conceptos, function(id) martin_registro(id, "nomina_conceptos"))
  de_clase <- function(clase) Filter(function(x) !is.null(x$clase) && x$clase == clase, conceptos)

  percepciones <- lapply(de_clase("percepcion"), function(x) martin_limpiar(list(
    tipo_percepcion = x$tipo, clave = x$clave, concepto = x$concepto,
    importe_gravado = if(is.null(x$importe_gravado)) 0 else x$importe_gravado,
    importe_exento = if(is.null(x$importe_exento)) 0 else x$importe_exento
  )))
  deducciones <- lapply(de_clase("deduccion"), function(x) martin_limpiar(list(
    tipo_deduccion = x$tipo, clave = x$clave, concepto = x$concepto, importe = x$importe
  )))
  otros_pagos <- lapply(de_clase("otro_pago"), function(x) martin_limpiar(list(
    tipo_otro_pago = x$tipo, clave = x$clave, concepto = x$concepto,
    importe = if(is.null(x$importe)) 0 else x$importe,
    subsidio_causado = x$subsidio_causado
  )))
  if(length(percepciones) == 0 && length(otros_pagos) == 0){
    errores <- c(errores, "La nómina no tiene percepciones ni otros pagos en nomina_conceptos")
  }
  sin_tipo <- Filter(function(x) is.null(x$tipo), conceptos)
  if(length(sin_tipo) > 0) errores <- c(errores, "Hay conceptos de nómina sin tipo (clave SAT)")

  emisor <- martin_limpiar(list(registro_patronal = Sys.getenv("NOMINA_REGISTRO_PATRONAL")))

  data <- martin_limpiar(list(
    fecha_pago = f$fecha_pago,
    fecha_inicial_pago = f$fecha_inicial_pago,
    fecha_final_pago = f$fecha_final_pago,
    num_dias_pagados = f$num_dias_pagados,
    emisor = if(length(emisor) > 0) emisor else NULL,
    receptor = martin_limpiar(list(
      curp = e$curp,
      tipo_contrato = martin_codigo(e$tipo_contrato),
      tipo_regimen = martin_codigo(e$tipo_regimen),
      num_empleado = e$num_empleado,
      periodicidad_pago = martin_codigo(e$periodicidad_pago),
      clave_ent_fed = e$clave_ent_fed,
      num_seguridad_social = e$num_seguridad_social,
      fecha_inicio_rel_laboral = e$fecha_inicio_rel_laboral,
      tipo_jornada = martin_codigo(e$tipo_jornada),
      departamento = e$departamento,
      puesto = e$puesto,
      riesgo_puesto = martin_codigo(e$riesgo_puesto),
      salario_diario_integrado = e$salario_diario_integrado
    ))
  ))
  if(length(percepciones) > 0) data$percepciones <- list(percepcion = percepciones)
  if(length(deducciones) > 0) data$deducciones <- deducciones
  if(length(otros_pagos) > 0) data$otros_pagos <- otros_pagos

  body <- martin_limpiar(list(
    type = "N",
    customer = customer,
    complements = list(list(type = "nomina", data = data)),
    series = f$series,
    folio_number = f$folio_number,
    external_id = f$external_id
  ))
  list(body = body, errores = errores)
}

facturapi_crear_factura_martin <- function(recordid){
  tryCatch(
    expr = {
      f <- martin_registro(recordid, "prefactura")

      if(!is.null(f$status) && f$status == "valid"){
        return(list(error = FALSE, mensaje = paste0("Esta prefactura ya está timbrada: ", f$uuid)))
      }

      tipo <- if(is.null(f$type)) "I" else f$type
      if(!is.null(f$emisor) && f$emisor == "RIR1104111G6-MIPYME Logistica"){
        key <- Sys.getenv("FACTURAPI_KEY_RIR")
      }else{
        key <- Sys.getenv("FACTURAPI_KEY")
      }

      armado <- switch(tipo,
        "I" = martin_cuerpo_ingreso_egreso(f, "I"),
        "E" = martin_cuerpo_ingreso_egreso(f, "E"),
        "P" = martin_cuerpo_pago(f),
        "N" = martin_cuerpo_nomina(f),
        list(body = NULL, errores = paste0("Tipo de CFDI no soportado: ", tipo))
      )

      # Errores de captura: no se manda nada a Facturapi
      if(length(armado$errores) > 0){
        mensaje <- paste(unique(armado$errores), collapse = "\n")
        martin_marcar(recordid, list(status = "error", error_message = mensaje))
        return(list(error = TRUE, mensaje = mensaje))
      }

      cfdi <- facturapi_crear_factura(armado$body, key)
      if(!last_response()$status_code %in% c(199:299)){
        mensaje <- if(is.null(cfdi$message)) "Facturapi rechazó el CFDI sin mensaje" else cfdi$message
        martin_marcar(recordid, list(status = "error", error_message = mensaje))
        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),
                             paste0("Fallo al timbrar desde el botón de Martin\nRecord: ", recordid,
                                    "\nTipo: ", tipo, "\nError: ", mensaje,
                                    "\nBody: ", toJSON(armado$body, auto_unbox = TRUE)))
        return(list(error = TRUE, mensaje = mensaje))
      }

      martin_marcar(recordid, martin_limpiar(list(
        status = cfdi$status,
        facturapi_id = cfdi$id,
        uuid = cfdi$uuid,
        total = cfdi$total,
        stamp_date = cfdi$stamp$date,
        verification_url = cfdi$verification_url,
        error_message = ""
      )))

      # PDF con la misma llave con la que se timbro (antes siempre usaba FACTURAPI_KEY)
      ruta <- paste0("~/facturas/", cfdi$uuid, ".pdf")
      Sys.sleep(5)
      for(i in 1:10){
        pdf <- facturapi_descargar_factura(cfdi$id, key, "pdf")
        if(pdf$status_code %in% c(199:299)){
          writeBin(pdf$body, ruta)
          airtable_subir_pdf(recordid, ruta, "fldPGYWcrenO5qMiz", Sys.getenv("AIRTABLE_MARTIN_BASE"), "pdf")
          break
        }
        Sys.sleep(1)
      }

      list(error = FALSE, uuid = cfdi$uuid, total = cfdi$total,
           mensaje = paste0("Se timbró el CFDI ", tipo, " con UUID ", cfdi$uuid))
    },
    error = function(e){
      mensaje <- paste0("Se rompió la función al facturar: ", conditionMessage(e))
      martin_marcar(recordid, list(status = "error", error_message = mensaje))
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),
                           paste0("Hubo un error al intentar facturar desde el botón de Martin\nRecord: ", recordid,
                                  "\nError: ", conditionMessage(e)))
      list(error = TRUE, mensaje = mensaje)
    }
  )
}

crear_factura <- function(orden,auth_facturapi,id_orden,canal_venta,tipo_de_pago="31",omitir="",
                          datos_direccion=NULL,mes='',fecha=NULL,series="Randu"){
  items <- datos_recibo(canal_venta,orden,id_orden,omitir)
  if(tipo_de_pago == "31"){
    tipo_de_pago <- tipo_pago_real(orden,canal_venta)
  }
  items_json <- sapply(items, function(producto) {
    generar_json(producto$cantidad,  gsub('"', "\\'", producto$nombre), producto$producto_key, producto$precio, producto$sku,producto$descuento)
  })
  recibo <- paste0('{"payment_form": "',tipo_de_pago,'", "items": [', paste(items_json, collapse = ","), ']}')
  recibo_lista <- fromJSON(recibo)
  if(length(id_orden)>1){
    id_orden <- id_orden[[1]]
  }
  recibo_lista <- append(recibo_lista,list("external_id"=id_orden))
  recibo_lista$series <- series
  if(length(datos_direccion)==0){
    datos_direccion <- list(
      "legal_name"="PUBLICO EN GENERAL",
      "tax_id"="XAXX010101000",
      "tax_system"="616",
      "address"=list(zip='76113')
      
    )
    # fecha_ori <- ymd_hms(fecha, tz = "UTC")
    # hoy <- Sys.Date()
    # 
    # # Fecha candidata
    # if (as.numeric(hoy - as.Date(fecha_ori)) > 3) {
    #   fecha_final <- hoy
    # } else {
    #   fecha_final <- as.Date(fecha_ori)
    # }
    # 
    # # Si cambió de mes, usar el último día del mes original
    # if (month(fecha_final) != month(fecha_ori) ||
    #     year(fecha_final) != year(fecha_ori)) {
    #   fecha_final <- ceiling_date(as.Date(fecha_ori), "month") - days(1)
    # }
    
    # 
    # fecha_ori <- ymd_hms(fecha)-6*3600
    # 
    # ahora <- Sys.time()-6*3600
    # # if(month(ahora) == month(fecha_ori)){
    # #   if (difftime(ahora, fecha_ori, units = "hours") > 72) {
    # #     fecha_final <- ahora
    # #     
    # #     # Si cambió de mes
    # #     if (month(fecha_final) != month(fecha_ori) ||
    # #         year(fecha_final) != year(fecha_ori)) {
    # #       
    # #       fecha_final <- ultimo_dia_mes_anterior(Sys.Date())+1800
    # #     }
    # #   } else {
    # #     fecha_final <- fecha_ori
    # #   }
    # #   
    # #   recibo_lista$date <- fecha_final
    # #   
    # # }
    # 
    # ahora <- Sys.time() - 6 * 3600
    # 
    # if (difftime(ahora, fecha_ori, units = "hours") > 72) {
    #   
    #   fecha_final <- ahora
    #   
    #   if (month(fecha_final) != month(fecha_ori) ||
    #       year(fecha_final) != year(fecha_ori)) {
    #     
    #     fecha_final <- ultimo_dia_mes_anterior(Sys.Date()) + 1800
    #   }
    #   
    # } else {
    #   
    #   fecha_final <- fecha_ori
    #   
    # }
    # if(str_detect(fecha_final,"2026-08")){
    #   fecha_final <- ahora
    # }
    # 
    # recibo_lista$date <- fecha_final#obtener_fecha_emision_valida(fecha_ori)
    # 
    # Estandarización UTC-6
    fecha_ori <- ymd_hms(fecha) - 6 * 3600
    ahora <- Sys.time() - 6 * 3600
    
    # Límite mínimo permitido por el SAT (72 horas atrás, dejamos 71.5h por margen de reloj)
    limite_sat <- ahora - 71.5 * 3600
    
    # Fin del mes anterior (ej. 2026-10-31 23:59:59)
    fin_mes_anterior <- floor_date(ahora, "month") - 1
    
    # Verificar si fecha_ori es del mes inmediato anterior
    es_mes_anterior <- (year(fecha_ori) == year(fin_mes_anterior)) && 
      (month(fecha_ori) == month(fin_mes_anterior))
    
    if (fecha_ori >= limite_sat && fecha_ori <= ahora) {
      # 1. Está dentro del rango válido de 72 horas (mismo mes o primeros días del siguiente)
      fecha_final <- fecha_ori
      
    } else if (es_mes_anterior && fin_mes_anterior >= limite_sat) {
      # 2. Es del mes anterior, ya pasó de 72h desde la compra, pero estamos en los primeros
      # 3 días del mes siguiente y el cierre de mes aún entra en la ventana de 72h del SAT
      fecha_final <- fin_mes_anterior
      
    } else {
      # 3. Fuera de rango de 72h (mismo mes viejo o mes anterior ya vencido): usar hora actual
      fecha_final <- ahora
    }
    
    # Formato ISO/Facturapi limpio en zona local
    recibo_lista$date <- format(fecha_final, "%Y-%m-%d %H:%M:%S")
    
    
    factura_year <- year(fecha_final)
    # months <- sprintf("%02d", month(Sys.Date()))
    # factura_year <- year(Sys.Date())
    global <- list(
      "periodicity"="day",
      "months"=mes,
      "year"=factura_year
    )
    recibo_lista$global <- global
    recibo_lista$customer <- datos_direccion
  }
  return(recibo_lista)
}
