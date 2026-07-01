
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
    orden <- orden$data$orders$ed0ges[[1]]$node
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
          "cantidad" =orden$lineItems$edges[[1]]$node$currentQuantity,
          "descuento" = descuento_real
        )  
        
        
        if(length(orden$shippingLines$edges)!=0){
          if(i == length(orden$shippingLines$edges$id) && as.numeric(orden$shippingLines$edges$price[[1]])!=0){
            items_orden[[length(items_orden) + 1]] <- list(
              "nombre" = orden$shippingLines$edges$title[[1]],
              "precio"= sum(as.numeric(orden$shippingLines$edges$price),na.rm=TRUE),
              "sku"=1000,
              "cantidad" = 1,
              "descuento" = NULL
            )  
          }
        }
      }
    }
    
  }
  print(items_orden)
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

registrar_factura <-function(factura,orden_venta) {
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
    if(!is.null(factura$stamp$date)){
      mes <- as.numeric(format(as.Date(factura$stamp$date),"%m"))
    }else{
      mes <- as.numeric(format(Sys.Date(),"%m"))
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
        break
      }
      if(i>=10){
        i <- 0
        break
      }
      Sys.sleep(1)
      i <- i + 1
    }
    if((j%%500)==0){
      Sys.sleep(4)
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

facturapi_crear_factura_martin <- function(recordid){
  tryCatch(
    expr={
      datos <- airtable_getrecorddata_byid(recordid,"prefactura",Sys.getenv("AIRTABLE_MARTIN_BASE"))
      productos <- sapply(datos$fields$items, function(x)
        airtable_getrecorddata_byid(x,"items",Sys.getenv("AIRTABLE_MARTIN_BASE")),
        simplify = F)
      
      codigo_tax_system <- sub("-.*", "", datos$fields$tax_system)
      descripcion_tax_system <- sub("^[^-]*-", "", datos$fields$tax_system)
      
      items <- lapply(productos, function(prod){
        
        # Taxes del producto
        taxes <- list()
        
        if (!is.null(prod$fields$taxes) &&
            length(prod$fields$taxes) > 0) {
          
          taxes <- lapply(prod$fields$taxes, function(tax_id){
            
            tax <- airtable_getrecorddata_byid(
              tax_id,
              "taxes",
              Sys.getenv("AIRTABLE_MARTIN_BASE")
            )
            
            list(
              type = tax$fields$type,
              rate = tax$fields$rate,
              withholding = isTRUE(tax$fields$withholding)
            )
          })
        }else {
          
          taxes <- list(
            list(
              type = "IVA",
              rate = 0.16
            )
          )
          
        }
        
        list(
          quantity = prod$fields$quantity,
          product=list(
            description = prod$fields$description,
            product_key = sub("-.*", "", prod$fields$product_key),
            unit_key = sub("-.*", "", prod$fields$unit_key),
            price = prod$fields$price,
            taxability = if(length(taxes)) "02" else "01",
            taxes = taxes,
            unit_name = sub("^[^-]*-", "", prod$fields$unit_key)
          )
          
        )
      })
      factura <- list(
        customer=list(
          "legal_name"=datos$fields$legal_name,
          "tax_id"=datos$fields$tax_id,
          "tax_system"=codigo_tax_system,
          "address"=list(
            "street" = datos$fields$street,
            "city" = datos$fields$city,
            "state" = datos$fields$state,
            "zip" = datos$fields$zip,
            "country" = datos$fields$country
          )
        ),
        "items"=items,
        "payment_form"=sub("-.*", "", datos$fields$payment_form),
        "use"=sub("-.*", "", datos$fields$use)
        
      )
      cfdi_api <- facturapi_crear_factura(factura,Sys.getenv("FACTURAPI_KEY"))
      if(!last_response()$status_code %in% c(199:299)){
        aux <- last_response() %>% resp_body_string()
        mensaje <- paste0("Fallo al intentar hacer la factura:\n",aux,"\nDatos:",toJSON(factura))
        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
        return(aux)
      }
      i <- 0
      while(TRUE){
        pdf <- facturapi_descargar_factura(cfdi_api$id,Sys.getenv("FACTURAPI_KEY"),"pdf")
        if(pdf$status_code %in% c(199:299)){
          writeBin(pdf$body, paste0("~/facturas/",cfdi_api$uuid,".pdf"))
          
          airtable_subir_pdf(datos$id,paste0("~/facturas/",cfdi_api$uuid,".pdf"),"fldPGYWcrenO5qMiz",Sys.getenv("AIRTABLE_MARTIN_BASE"),"pdf")
          break
        }
        if(i>=10){
          i <- 0
          break
        }
        Sys.sleep(1)
        i <- i + 1
      }
    },
    error=function(e){
      mensaje <- paste0("Hubo un error al intentar facturar desde el boton de Martin\nRecord:",recordid,
                        "\nError: ",e)
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
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
    
    
    fecha_ori <- ymd_hms(fecha)
    
    ahora <- Sys.time() + minutes(30)
    
    if (difftime(ahora, fecha_ori, units = "hours") > 72) {
      fecha_final <- ahora
      
      # Si cambió de mes
      if (month(fecha_final) != month(fecha_ori) ||
          year(fecha_final) != year(fecha_ori)) {
        
        fecha_final <- ultimo_dia_mes_anterior(Sys.Date())+1800
      }
    } else {
      fecha_final <- fecha_ori
    }

    recibo_lista$date <- fecha_final
    
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
