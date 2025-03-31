
amz_get_active_token <- function(){
  tkdata <- airtable_getrecorddata_byid("recLIQv40EFSx9vyr", "tokens", Sys.getenv('AIRTABLE_DEV_BASE'))
  tkdata <- tkdata$fields
  token_expires <- lubridate::as_datetime(tkdata$token_expires)
  refresh_token <- tkdata$refresh_token
  access_token <- tkdata$access_token
  
  if(Sys.time()<token_expires){
    validat <- access_token
  }else{
    request("https://api.amazon.com/auth/o2/token") %>% 
      req_method("POST") %>% 
      req_body_json(list('grant_type'='refresh_token',
                         'client_id'=Sys.getenv('AMZ_APP_CLIENTID'),
                         'client_secret'=Sys.getenv('AMZ_APP_CLIENTSECRET'),
                         'refresh_token'=refresh_token)) %>% 
      req_perform()
    
    newtokendata <- last_response() %>% resp_body_json()
    newaccess_token <- newtokendata$access_token
    newrefreshtoken <- newtokendata$refresh_token
    newexpire <- last_response() %>% resp_date() + 3600 
    
    airtable_updatesinglerecord(fieldslist = list('access_token'=newaccess_token,
                                                  'refresh_token'=newrefreshtoken,
                                                  'token_expires'=newexpire), 
                                tablename = "tokens", base_id = Sys.getenv('AIRTABLE_DEV_BASE'),
                                recordid = "recLIQv40EFSx9vyr")
    validat <- newaccess_token
  }
  validat
}

get_amzorderitem_byid <- function(orderid, amz_token){
  order_url <- paste0("https://sellingpartnerapi-na.amazon.com/orders/v0/orders/",orderid,"/orderItems")
  order_response <- request(order_url) %>%
    req_headers(
      Authorization = paste("Bearer ", amz_token),
      "x-amz-access-token" = amz_token
    ) %>%
    req_perform() %>%
    resp_body_json()
}

get_amzorder_byid <- function(orderid, amz_token){
  order_url <- paste0("https://sellingpartnerapi-na.amazon.com/orders/v0/orders/", orderid)
    order_response <- request(order_url) %>%
    req_headers(
      Authorization = paste("Bearer ", amz_token),
      "x-amz-access-token" = amz_token
    ) %>%
    req_perform() %>%
    resp_body_json()
}

get_amzorderaddress_byid <- function(orderid,amz_token){
  order_url <- paste0("https://sellingpartnerapi-na.amazon.com/orders/v0/orders/", orderid,"/address")
  order_response <- request(order_url) %>%
    req_headers(
      Authorization = paste("Bearer ", amz_token),
      "x-amz-access-token" = amz_token
    ) %>%
    req_perform() %>%
    resp_body_json()
}

register_amzorder_in_airtable <- function(amz_order){
  lineitems_recordid <- amz_register_lineitems(amz_order)
  #client_recordid <- register_client(shopifyorder)
  shippingaddress_recordid <- amz_register_address(amz_order)
  
  fieldslist <- list(
    'fecha'=amz_order$payload$PurchaseDate,
    'canal_venta'='amazonrnd',
    'ventas_producto'=lineitems_recordid,
    'id_origen'=amz_order$payload$AmazonOrderId
  )
  if(!is.null(shippingaddress_recordid)){
    fieldslist <- append(fieldslist,list("direccion_envio"=list(shippingaddress_recordid)))
  }
  newov_content  <- airtable_createrecord(fieldslist, "ordenes_venta", "appofqh5EnlVGAtn2")
  return(newov_content)
}

amz_register_lineitems <- function(amz_order){
  amz_token <- amz_get_active_token()
  amz_items <- get_amzorderitem_byid(amz_order$payload$AmazonOrderId,amz_token)
  vp_recordids <- vector(mode="list", length(amz_items$payload$OrderItems))
  for(i in 1:length(length(amz_items$payload$OrderItems))){
    cantidad <- amz_items$payload$OrderItems[[i]]$QuantityOrdered
    nombre_producto <- amz_items$payload$OrderItems[[i]]$Title
    precio <- as.double(amz_items$payload$OrderItems[[i]]$ItemPrice$Amount)#Es probable que se cambie
    sku <- str_extract(amz_items$payload$OrderItems[[i]]$SellerSKU,"\\d+")
    id_lineitem <- as.character(amz_items$payload$OrderItems[[i]]$OrderItemId)
    comentarios <- paste0(nombre_producto,"\n Fecha limite: ",
                          amz_order$payload$EarliestShipDate)
    fieldslist <- list(
      'cantidad'=cantidad,
      'helper_product_name'=nombre_producto,
      'precio_unitario'=precio,
      'pendiente_envio'=cantidad,
      'id_lineitem'=id_lineitem,
      'comentarios'=comentarios
    )
    if(!is.null(sku)){
      if(!is.na(sku) && str_detect(sku,"^\\d\\d\\d\\d\\d$") ){
        product_recordid_list <- airtable_getrecordslist("productos",Sys.getenv('AIRTABLE_CES_BASE'), 
                                                         formula=paste0("sku=",sku))
        if(length(product_recordid_list)!=0){
          recordid_producto <- list(producto=list(product_recordid_list[[1]]$id))
          fieldslist <- append(fieldslist, recordid_producto) 
        }
      }
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

amz_register_address <- function(amz_order){
  address_data <- list(
    #'nombre_destinatario'=,
    #'apellido_destinatario'= shopifyorder$shipping_address$last_name,
    #'telefono_destino'= shopifyorder$shipping_address$phone,
    #'calle'= shopifyorder$shipping_address$address1,
    'codigo_postal'=amz_order$payload$ShippingAddress$PostalCode,
    'ciudad'=amz_order$payload$ShippingAddress$City,
    'estado'=amz_order$payload$ShippingAddress$StateOrRegion,
    'colonia'=amz_order$payload$ShippingAddress$Municipality,
    'referencias'=amz_order$payload$ShippingAddress$Municipality
  )
  #print(address_data)
  recordid <- airtable_createrecord(address_data, "direcciones", Sys.getenv("AIRTABLE_CES_BASE"))
  if(last_response()$status_code %in% c(199:299)){
    return(recordid$id)
  }
  else{
    return(NULL)
  }
}