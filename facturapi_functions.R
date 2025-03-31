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

facturapi_descargar_factura <- function(id,auth_facturapi,formato="pdf"){
  res<-request(paste0("https://www.facturapi.io/v2/invoices/",id,"/",formato)) %>% 
    req_method("GET") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() 
}

facturapi_crear_recibo <- function(orden,auth_facturapi,id_orden,canal){
  items <- datos_recibo(canal,orden)
  
  items_json <- sapply(items, function(producto) {
    generar_json(producto$cantidad, producto$nombre, producto$product_key, producto$precio, producto$sku)
  })
  recibo <- paste0('{"payment_form": "31", "items": [', paste(items_json, collapse = ","), ']}')
  recibo_lista <- fromJSON(recibo)
  recibo_lista <- append(recibo_lista,list("external_id"=id_orden))
  
  res1<-request("https://www.facturapi.io/v2/receipts") %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi))  %>% 
    #res1 %>% req_auth_bearer_token(paste0("Bearer",auth_facturapi)) %>%
    req_headers('Content-type'='application/json') %>%
    req_body_json(recibo_lista) %>%
    req_perform() %>%
    resp_body_json()
}

datos_recibo <- function(canal,orden,id_orden){
  items_orden <- list()
  
    if(canal=="ml"){
      for(i in 1:length(orden$order_item)){
        items_orden[[length(items_orden) + 1]] <- list(
        "nombre" = orden$order_items[[1]]$item$title,
        "precio"= orden$order_items[[1]]$unit_price,
        "sku"=orden$order_items[[1]]$item$seller_sku,
        "cantidad" = orden$order_items[[1]]$quantity 
      )  
        
      }
    }
    if(canal=="amz"){
      for(item in orden$payload$OrderItems){
        items_orden[[length(items_orden) + 1]] <- list(
          "nombre" = item$Title,
          "precio"= item$ItemPrice$Amount,
          "sku"=item$SellerSKU,
          "cantidad" = item$QuantityOrdered
        )  
      }
    }
    if(canal == "shp"){
      for(i in 1:length(orden$line_items$id)){
        print(i)
        items_orden[[length(items_orden) + 1]] <- list(
          "nombre" = orden$line_items$name[[i]],
          "precio"= orden$line_items$price[[i]],
          "sku"=orden$line_items$sku[[i]],
          "cantidad" = orden$line_items$quantity[[i]]
        )  
      }
      
    }
  for(i in 1:length(items_orden)){
    if(!is.null(sku)){
      if(!is.na(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
        producto<-airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),
                                          paste0("sku=",sku))
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

generar_json <- function(cantidad, descripcion, product_key, precio, sku) {
  return(paste0('{
                  "quantity": ', cantidad, ',
                  "product": {
                    "description": "', descripcion, '",
                    "product_key": "', product_key, '",
                    "price": ', precio, ',
                    "sku": "', sku, '"
                  }
                }'))
}
