library(httr2)
library(purrr)
library(stringr)


get_all_shopifyrandu_orders <- function(){
  shopify_request <- request("https://randumexico.myshopify.com/admin/api/2022-10/orders.json?status=any") %>% 
    req_method("GET") %>% 
    req_headers('X-Shopify-Access-Token'= Sys.getenv('SHOPIFY-RANDUMX-TK')) %>% 
    req_perform()
  
  shopify_response_headers <-resp_headers(shopify_request)
  shopify_responsebody <- resp_body_json(shopify_request)
  shopify_orders <- shopify_responsebody$orders
  
  while(!is.na(str_extract(shopify_response_headers$Link, '<.*>; rel=\"next'))){
    nextpagelink <- str_remove(shopify_response_headers$Link, '<.*>; rel=\"previous\", ') %>% 
      str_extract('<.*>; rel=\"next') %>% 
      str_remove("<") %>% 
      str_remove(">; rel=\"next")
    
    shopify_request <- request(nextpagelink) %>% 
      req_method("GET") %>% 
      req_headers('X-Shopify-Access-Token'= Sys.getenv('SHOPIFY-RANDUMX-TK')) %>% 
      req_perform()
    
    shopify_response_headers <-resp_headers(shopify_request)
    shopify_responsebody <- resp_body_json(shopify_request)
    shopify_orders <- append(shopify_orders, shopify_responsebody$orders)
    
  }
  shopify_orders
}

get_recent_shopifyrandu_orders <- function(){
  shopify_request <- request("https://randumexico.myshopify.com/admin/api/2022-10/orders.json?status=any") %>% 
    req_method("GET") %>% 
    req_headers('X-Shopify-Access-Token'= Sys.getenv('SHOPIFY-RANDUMX-TK')) %>% 
    req_perform()
  
  shopify_responsebody <- resp_body_json(shopify_request)
  shopify_responsebody$orders
}

register_shopifyorder_in_airtable <- function(shopifyorder){
  lineitems_recordid <- register_lineitems(shopifyorder)
  #client_recordid <- register_client(shopifyorder)
  shippingaddress_resp <- register_address(shopifyorder)
  shippingaddress_id <- shippingaddress_resp$id
  fieldslist <- list(
    'fecha'=shopifyorder$created_at,
    'canal_venta'='shprndmx',
    'ventas_producto'=lineitems_recordid,
    'id_origen'=shopifyorder$name,
    'direccion_envio'=list(shippingaddress_id)
  )
  newov_content  <- airtable_createrecord(fieldslist, "ordenes_venta", Sys.getenv('AIRTABLE_CES_BASE'))
}

register_lineitems <- function(shopifyorder){
  lineitems <- shopifyorder$line_items
  vp_recordids <- vector(mode="list", length(lineitems))
  for(i in 1:length(lineitems)){
    cantidad <- lineitems[[i]]$quantity
    nombre_producto <- lineitems[[i]]$name
    variant_id <- lineitems[[i]]$variant_id
    precio <- as.double(lineitems[[i]]$price)
    sku <- lineitems[[i]]$sku
    id_lineitem <- as.character(lineitems[[i]]$id)
    li_properties <- lineitems[[i]]$properties
    comentarios <- ""
    if(length(li_properties)>0 ){
      for(j in 1:length(li_properties)){
        comentarios <- paste0(comentarios,li_properties[[j]]$name,": ",
                              li_properties[[j]]$value," \n")
      }
    }
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'shopify_variant_id'=variant_id,
      'pendiente_envio'=cantidad,
      'id_lineitem'=id_lineitem,
      'comentarios'=comentarios
    )
    
    if(!is.null(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
      product_recordid_list <- airtable_getrecordslist("productos",Sys.getenv('AIRTABLE_CES_BASE'), 
                                                       formula=paste0("sku=",sku))
      
      recordid_producto <- list(producto=list(product_recordid_list[[1]]$id))
      fieldslist <- append(fieldslist, recordid_producto)
    }
    newvp_content  <- airtable_createrecord(fieldslist, "ventas_producto", Sys.getenv('AIRTABLE_CES_BASE'))
    if(!is.null(newvp_content)){
      vp_recordids[[i]] <- newvp_content$id[[1]]
    }else{
      print(paste0("hubo un problema al registrar la el line_item 
                   (venta_producto #",i))
    }
  }
  vp_recordids
}

register_client <- function(shopifyorder){
  client_data <- vector(mode="list", length(lineitems))
  client_data['email_principal'] <- shopifyorder$email
  client_data['telefono_principal'] <- shopifyorder$shipping_address$phone
  client_data['nombre'] <- shopifyorder$customer$first_name
  client_data['apellido_paterno'] <- shopifyorder$customer$last_name
  #client_data['direccion_principal'] <- ''
}

register_address <- function(shopifyorder){
  #print(shopifyorder$shipping_address)
  address_data <- list(
    'nombre_destinatario'=shopifyorder$shipping_address$first_name,
    'apellido_destinatario'= shopifyorder$shipping_address$last_name,
    'telefono_destino'= shopifyorder$shipping_address$phone,
    'calle'= shopifyorder$shipping_address$address1,
    'codigo_postal'=shopifyorder$shipping_address$zip,
    'ciudad'=shopifyorder$shipping_address$city,
    'estado'=shopifyorder$shipping_address$province,
    'colonia'=shopifyorder$shipping_address$address2,
    'referencias'=shopifyorder$shipping_address$address2
  )
  #print(address_data)
  airtable_createrecord(address_data, "direcciones", Sys.getenv("AIRTABLE_CES_BASE"))
}

get_all_shopifyrandu_products <- function(){
  
  shopify_request <- request("https://randumexico.myshopify.com/admin/api/2022-10/products.json") %>% 
    req_method("GET") %>% 
    req_headers('X-Shopify-Access-Token'= Sys.getenv('SHOPIFY-RANDUMX-TK')) %>% 
    req_perform()
  
  shopify_response_headers <-resp_headers(shopify_request)
  
  shopify_responsebody <- resp_body_json(shopify_request)
  
  shopify_products <- shopify_responsebody$products
  if(length(str_extract(shopify_response_headers$Link, '<.*>; rel=\"next')) > 0 ){
    while(!is.na(str_extract(shopify_response_headers$Link, '<.*>; rel=\"next'))){
      nextpagelink <- str_remove(shopify_response_headers$Link, '<.*>; rel=\"previous\", ') %>% 
        str_extract('<.*>; rel=\"next') %>% 
        str_remove("<") %>% 
        str_remove(">; rel=\"next")
      
      shopify_request <- request(nextpagelink) %>% 
        req_method("GET") %>% 
        req_headers('X-Shopify-Access-Token'= Sys.getenv('SHOPIFY-RANDUMX-TK')) %>% 
        req_perform()
      shopify_response_headers <-resp_headers(shopify_request)
      shopify_responsebody <- resp_body_json(shopify_request)
      shopify_products <- append(shopify_products, shopify_responsebody$products)
      
    }
  }
  shopify_products
}

allshopifyvariants <- function(shopifyprods){
  variantidvec <- c()
  skuvec <- c()
  k <- 1
  for(i in 1:length(shopifyprods)){
    for(j in 1:length(shopifyprods[[i]]$variants)){
      if(!is.null(shopifyprods[[i]]$variants[[j]]$sku)){
        variantidvec[k] <- as.character(shopifyprods[[i]]$variants[[j]]$id)
        skuvec[k] <- as.integer(shopifyprods[[i]]$variants[[j]]$sku)
        k <- k+1 
      }
    }
  }
  return(tibble(variantid=variantidvec,sku=skuvec))
}

get_partes_producto <- function(sku){
  product_recordid_list <- airtable_getrecordslist("productos",Sys.getenv('AIRTABLE_CES_BASE'), 
                                                   formula=paste0("sku=",sku))
  
  recordid_producto <- list(producto=list(product_recordid_list[[1]]$id))
}

register_lineitemsv2 <- function(shopifyorder){
  lineitems <- shopifyorder$line_items
  vp_recordids <- vector(mode="list", length(lineitems))
  for(i in 1:length(lineitems)){
    cantidad <- lineitems[[i]]$quantity
    nombre_producto <- lineitems[[i]]$name
    variant_id <- lineitems[[i]]$variant_id
    precio <- as.double(lineitems[[i]]$price)
    sku <- lineitems[[i]]$sku
    id_lineitem <- as.character(lineitems[[i]]$id)
    li_properties <- lineitems[[i]]$properties
    comentarios <- ""
    if(length(li_properties)>0 ){
      for(j in 1:length(li_properties)){
        comentarios <- paste0(comentarios,li_properties[[j]]$name,": ",
                              li_properties[[j]]$value," \n")
      }
    }
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'shopify_variant_id'=variant_id,
      'pendiente_envio'=cantidad,
      'id_lineitem'=id_lineitem,
      'comentarios'=comentarios
    )
    
    if(!is.null(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
      product_recordid_list <- airtable_getrecordslist("productos",Sys.getenv('AIRTABLE_RIR_BASE'), 
                                                       formula=paste0("sku=",sku))
      
      recordid_producto <- list(producto=list(product_recordid_list[[1]]$id))
      partes_producto <- product_recordid_list[[1]]$fields$partes_producto
      if(length(partes_producto)>1){
        for(i in 1:length(partes_producto)){
          recordid_parte <- airtable_getrecordslist("productos",Sys.getenv('AIRTABLE_RIR_BASE'), 
                                                    formula=paste0("sku=",sku))
          recordid_parte <- list(producto=list(product_recordid_list[[1]]$id))
        }
        
      }
      fieldslist <- append(fieldslist, recordid_producto)
    }
    newvp_content  <- airtable_createrecord(fieldslist, "ventas_producto", Sys.getenv('AIRTABLE_CES_BASE'))
    if(!is.null(newvp_content)){
      vp_recordids[[i]] <- newvp_content$id[[1]]
    }else{
      print(paste0("hubo un problema al registrar la el line_item 
                   (venta_producto #",i))
    }
  }
  vp_recordids
}

register_lineitemsv3 <- function(shopifyorder){
  #Esta version esta adaptada para la api de shopify graphql
  fieldslist1 <- list()
  #lineitems <- shopifyorder$line_items
  lineitems <- split(shopifyorder$line_items, seq(nrow(shopifyorder$line_items)))
  vp_recordids <- vector(mode="list", length(lineitems))
  for(i in 1:length(lineitems)){
    cantidad <- lineitems[[i]]$quantity
    nombre_producto <- lineitems[[i]]$name
    variant_id <- lineitems[[i]]$variant_id
    precio <- as.double(lineitems[[i]]$price)
    sku <- lineitems[[i]]$sku
    id_lineitem <- as.character(lineitems[[i]]$id)
    li_properties <- lineitems[[i]]$properties
    comentarios <- ""
    if(length(li_properties[[1]])>0 ){
      for(j in 1:length(li_properties$name)){
        comentarios <- paste0(comentarios,li_properties$name[[j]],": ",
                              li_properties$value[[j]]," \n")
      }
    }
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'shopify_variant_id'=variant_id,
      'pendiente_envio'=cantidad,
      'id_lineitem'=id_lineitem,
      'comentarios'=comentarios
    )
    
    if(!is.null(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
      product_recordid_list <- airtable_getrecordslist("productos",Sys.getenv('AIRTABLE_CES_BASE'), 
                                                       formula=paste0("sku=",sku))
      
      recordid_producto <- list(producto=list(product_recordid_list[[1]]$id))
      fieldslist <- append(fieldslist, recordid_producto)
    }
    newvp_content  <- airtable_createrecord(fieldslist, "ventas_producto", Sys.getenv('AIRTABLE_CES_BASE'))
    if(!is.null(newvp_content)){
      vp_recordids[[i]] <- newvp_content$id[[1]]
    }else{
      print(paste0("hubo un problema al registrar la el line_item
                   (venta_producto #",i))
    }
    #fieldslist1[[i]] <- fieldslist
  }
  vp_recordids
  #fieldslist1
}

register_shopifyorder_in_airtablev2 <- function(shopifyorder){
  lineitems_recordid <- register_lineitemsv3(shopifyorder)
  #client_recordid <- register_client(shopifyorder)
  shippingaddress_resp <- register_address(shopifyorder)
  shippingaddress_id <- shippingaddress_resp$id
  fieldslist <- list(
    'fecha'=shopifyorder$created_at,
    'canal_venta'='shprndmx',
    'ventas_producto'=lineitems_recordid,
    'id_origen'=shopifyorder$name,
    'direccion_envio'=list(shippingaddress_id)
  )
  newov_content  <- airtable_createrecord(fieldslist, "ordenes_venta", Sys.getenv('AIRTABLE_CES_BASE'))
}
