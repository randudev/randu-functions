# Función para obtener detalles de la orden por nombre
consulta_por_nombre <- function(order_name,access_token) {
  shopify_url <- "https://xzbxiv-py.myshopify.com/admin/api/2025-01/graphql.json"
  # Consulta GraphQL para obtener los detalles de la orden por nombre usando paste0
  query <- paste0('
  {
    orders(first: 10, query: "name:', order_name, '") {
      edges {
        node {
          id
          name
          fulfillmentOrders(first: 10) {
            edges {
              node {
                id
                status
                lineItems(first: 5) {
                  edges {
                    node {
                      id
                    }
                  }
                }
              }
            }
          }
          lineItems(first: 10)  {
              edges {
                node{
                  id
                  name
                }
              }
            }
        }
      }
    }
  }
  ')
  
  # Realizar la solicitud GraphQL
  response <- request(shopify_url) %>%
    req_headers('X-Shopify-Access-Token' =access_token,
                "Content-Type" = "application/json") %>%
    req_body_json(list(query = query)) %>%
    req_perform()
  
  # Parsear la respuesta JSON
  respuesta <- resp_body_json(response)
  
  return(respuesta)
}
orders <- consulta_por_nombre(order_name,access_token)

mandar_numero_rastreo <- function(numeros_rastreo,paqueteria,fulfill_id,access_token){
  #shopify_url <- 
  shopify_url <- "https://xzbxiv-py.myshopify.com/admin/api/2025-01/graphql.json"
  if(paqueteria=="PAQUETEXPRESS"){
    url <- "https://www.paquetexpress.com.mx/"
  }else{
    url <- NULL
  }
  fulfillment_data <- list(
    lineItemsByFulfillmentOrder = list(
      list(
        fulfillmentOrderId = fulfill_id
      )
    ),
    #orderId=order_id,
    notifyCustomer = TRUE,
    trackingInfo = list(
      company = paqueteria,
      number = numeros_rastreo[[1]]
    )
  )
  if(!is.null(url)){
    fulfillment_data$trackingInfo$url <- url
  }
  if(length(numeros_rastreo)>1){
    fulfillment_data$trackingInfo$numbers <- numeros_rastreo[-1]
  }
  
  variables <- list(
    fulfillment = fulfillment_data,
    message = "RanduMx"
  )
  
  mutation <- '
  mutation fulfillmentCreate($fulfillment: FulfillmentInput!, $message: String) {
  fulfillmentCreate(fulfillment: $fulfillment, message: $message) {
    fulfillment {
      id
    }
    userErrors {
      field
      message
    }
  }
  }'
  
  response <- request(shopify_url) %>%
    req_method("POST") %>% 
    req_headers('X-Shopify-Access-Token' =access_token,
                "Content-Type" = "application/json") %>%
    req_body_json(list(query = mutation,variables = variables)) %>%
    req_perform() %>% 
    resp_body_json()
  return(response)
}

enviar_nota <- function(order_id,numeros_rastreo,paqueteria,access_token){
  
  notes <- paste("Paqueteria:",paqueteria,"\nNúmeros de rastreo adicionales:\n", paste(numeros_rastreo, collapse = "\n"))
  
  notes_mutation <- paste0('
    mutation {
      orderUpdate(input: {
        id: "', order_id, '",
        note: "', notes, '"
      }) {
        order {
          id
        }
      }
    }
    ')
  
  # Realizar la solicitud para actualizar las notas
  response_notes <- request(shopify_url) %>%
    req_headers('X-Shopify-Access-Token' =access_token,
                "Content-Type" = "application/json") %>%
    req_body_json(list(query = notes_mutation)) %>%
    req_perform() %>% 
    resp_body_json()
  return(response_notes)
}

fulfill_id <- orders$data$orders$edges[[1]]$node$fulfillmentOrders$edges[[1]]$node$id
res <- mandar_numero_rastreo(numeros_rastero,paqueteria,fulfill_id,access_token)
nota <- enviar_nota(orders$data$orders$edges[[1]]$node$id,numeros_rastero,paqueteria,access_token)
