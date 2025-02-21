library(httr2)
library(jsonlite)

facturapi_obtener_facturas <- function(api_key,fecha=NULL) {
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
    
    
    response <- request(url) %>%
      req_method("GET") %>% 
      req_headers('Authorization'=paste0("Bearer ",auth_facturapi)) %>%
      req_perform() 
    
    # Convertir la respuesta JSON a un dataframe
    result <-response  %>% resp_body_json() 
    
    # Verificar si hay facturas
    if (length(result$data) == 0) {
      break  # Si no hay más facturas, salir del ciclo
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
facturapi_crear_recibo <- function(orden,auth_factura){
  orden <-ml_order
  nombre<-orden$order_items[[1]]$item$title
  clave_producto<- orden$order_items[[1]]$item$id
  precio<-orden$order_items[[1]]$unit_price
  skul<-orden$order_items[[1]]$item$seller_sku
  producto_key<-airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),
                                        paste0("sku=",orden$order_items[[1]]$item$seller_sku))[[1]]$fields$clave_sat
  if(!is.null(producto_key)){
    recibo <- paste0('{"payment_form": "31",
                          "items": [{
                          "quantity":',ml_order$order_items[[1]]$quantity ,',
                          "product": {"description": "',nombre,'",
                                      "product_key": "',producto, ' ",
                                      "price": ',precio,',
                                      "sku": "',skul,'"}}]}')}
  else{
    recibo <- paste0('{"payment_form": "31",
                          "items": [{
                          "quantity":',ml_order$order_items[[1]]$quantity ,',
                          "product": {"description": "',nombre,'",
                                      "product_key": "',56101703, ' ",
                                      "price": ',precio,',
                                      "sku": "',skul,'"}}]}')}
  info<-list('description'=nombre,
             'product_key'=60131324,
             'price'=precio,
             'sku'=skul)
  items<-list('quantity'=1,
              'product'=info)
  body<-list(
    'payment_form' = "03",
    'items' = data.frame(items))
  res1<-request("https://www.facturapi.io/v2/receipts") %>% 
    req_method("POST") %>% 
    req_headers('Authorization'=paste0("Bearer ",auth_facturapi))  %>% 
    #res1 %>% req_auth_bearer_token(paste0("Bearer",auth_facturapi)) %>%
    req_headers('Content-type'='application/json') %>%
    req_body_json(fromJSON(recibo)) %>%
    req_perform() %>%
    resp_body_json()
}
