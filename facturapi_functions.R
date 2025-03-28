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

facturapi_crear_recibo <- function(orden,auth_facturapi){
  nombre<-orden$order_items[[1]]$item$title
  clave_producto<- orden$order_items[[1]]$item$id
  precio<- orden$order_items[[1]]$unit_price
  sku<-orden$order_items[[1]]$item$seller_sku
  orden_venta <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),
                                         paste0("id_origen='",orden$id,"'"))
  producto<-airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),
                                        paste0("sku=",sku))
  
  if(!is.null(producto)){
    producto_key <- producto[[1]]$fields$clave_sat 
  }else{
    producto_key <- NULL
  }
  if(!is.null(producto_key)){
    recibo <- paste0('{"payment_form": "31",
                          "items": [{
                          "quantity":',ml_order$order_items[[1]]$quantity ,',
                          "product": {"description": "',nombre,'",
                                      "product_key": "',producto_key, ' ",
                                      "price": ',precio,',
                                      "sku": "',sku,'"}}]
    }')
    }
  else{
    recibo <- paste0('{"payment_form": "31",
                          "items": [{
                          "quantity":',ml_order$order_items[[1]]$quantity ,',
                          "product": {"description": "',nombre,'",
                                      "product_key": "',56101703, ' ",
                                      "price": ',precio,',
                                      "sku": "',sku,'"}}]}')}
  # info<-list('description'=nombre,
  #            'product_key'=60131324,
  #            'price'=precio,
  #            'sku'=skul)
  # items<-list('quantity'=1,
  #             'product'=info)
  # body<-list(
  #   'payment_form' = "03",
  #   'items' = data.frame(items))
  recibo_lista <- fromJSON(recibo)
  if(length(orden_venta)!=0){
    id_ov <- list("external_id"=orden_venta[[1]]$fields$id_ordenes_venta)
    recibo_lista <- append(recibo_lista,id_ov)
  }
  res1<-request("https://www.facturapi.io/v2/receipts") %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi))  %>% 
    #res1 %>% req_auth_bearer_token(paste0("Bearer",auth_facturapi)) %>%
    req_headers('Content-type'='application/json') %>%
    req_body_json(recibo_lista) %>%
    req_perform() %>%
    resp_body_json()
}
