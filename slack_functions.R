enviar_mensaje_slack <- function(url,mensaje){
  request <- request(url) %>%
    req_method("POST") %>%
    req_headers("Content-Type" = "application/json") %>%
    req_body_json(list(text = mensaje)) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
}

slack_mensaje_pregunta <- function(preguntas){
  questions_id <- preguntas$id_resource
  for(i in seq_along(questions_id)){
    cuerpo <- fromJSON(preguntas$body[[i]])
    if(!is.null(cuerpo$attempts)){
      if(cuerpo$attempts != 1){
        next
      }
    }
    
    if(cuerpo$user_id == Sys.getenv("SELLERID_ML_ASM")){
      recordid_token <- "recQLtjnMhd4ZCiJq"
      canal <- "mercadolibreasm"
    }else{
      recordid_token <- ""
      canal <- NULL
    }
    ml_token <- get_active_token(recordid_token)
    questions <- ml_preguntas(questions_id[[i]],ml_token)
    if(last_response()$status_code %in% c(199:299)){
      if(questions$status == "UNANSWERED"){
        item <- ml_obtener_item(questions$item_id,ml_token)
        articulo <- item$title
        nombre <- ""
        if(!is.null(questions$from$id)){
          ml_user <- ml_obtener_user(questions$from$id,ml_token)
          if(!is.null(ml_user$first_name)){
            nombre <- paste(ml_user$first_name,ml_user$last_name)
          }else{
            nombre <- ml_user$nickname
          }
        }
        mensaje <-paste0("El articulo: ",item$id," ",articulo,"\nhttps://articulo.mercadolibre.com.mx/MLM-",
                         str_sub(item$id,4,nchar(item$id)),"\nRecibio una pregunta de ",nombre,"\n\nID pregunta: ",questions$id,"\n",questions$text)
        
        if(recordid_token == "recQLtjnMhd4ZCiJq"){
          mensaje <- paste0(Sys.getenv("SELLERID_ML_ASM"), " ", mensaje)
        }
        
        enviar_mensaje_slack(Sys.getenv("SLACK_QUESTIONS_URL"),mensaje = mensaje)      
      }
    }
  }
}

slack_responder_pregunta <- function(resp_mensaje,ml_token){
  texto <- resp_mensaje$event$text
  hilo<- slack_obtener_hilo(Sys.getenv("SLACK_BOT_TOKEN"),resp_mensaje$event$channel,resp_mensaje$event$thread_ts)
  if(!is.null(hilo[[1]]$subtype)){
    if(str_detect(texto,"<@U08LEBF4Q85> ") && str_detect(hilo[[1]]$text,"ID pregunta: ")){
      if(str_detect(hilo[[1]]$text,Sys.getenv("SELLERID_ML_ASM"))){
          recordid_token <- "recQLtjnMhd4ZCiJq"
          canal <- "mercadolibreasm"
      }else{
        recordid_token <- ""
        canal <- NULL
      }
      ml_token <- get_active_token(recordid_token)
      mensaje_slack <- str_split(texto,"<@U08LEBF4Q85> ")[[1]][2]
      id <- str_split(str_split(hilo[[1]]$text,"ID pregunta: ")[[1]][2],"\\n")[[1]][1]
      if(es_numero(id) && nchar(mensaje_slack)!=0){
        ml_responder_pregunta(id,mensaje_slack,ml_token)
        if(last_response()$status_code %in% (199:299)){
          respuesta_slack <- paste0("Se respondio la pregunta: ", id)
          slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),resp_mensaje$event$channel,resp_mensaje$event$ts,respuesta_slack)
        }
      } 
    }
  }
}

slack_responder_en_hilo <- function(bot_token, canal, thread_ts, mensaje) {
  req <- request("https://slack.com/api/chat.postMessage") %>%
    req_auth_bearer_token(bot_token) %>%
    req_body_json(list(
      channel = canal,
      text = mensaje,
      thread_ts = thread_ts
    )) %>%
    req_perform() %>% 
    resp_body_json()
}

slack_obtener_hilo <- function(bot_token, canal, thread_ts){
    request("https://slack.com/api/conversations.replies") %>%
      req_headers(Authorization = paste0("Bearer ", bot_token)) %>%
      req_url_query(channel = canal, ts = thread_ts) %>%
      req_perform() %>%
      resp_body_json() %>%
      .$messages
}

slack_npu <- function(cuerpo,ml_token){
  bot_token <- Sys.getenv("SLACK_BOT_TOKEN")
  mensaje_persona <- str_split(cuerpo$event$text,"<@U08LEBF4Q85> ")[[1]][2]
  titulo <- str_match(mensaje_persona, '"(.*?)"')[,2]
  sin_comillas <- str_remove_all(mensaje_persona, '"[^"]*"')
  precio <- as.numeric(str_extract(sin_comillas, "\\d+"))
  if(es_numero(precio)){
    titulo <- paste0(titulo," BSL")
    resp <- ml_crear_publicacion(titulo,precio,ml_token)
    if(last_response()$status_code %in% c(199:299)){
      Sys.sleep(10)
      item <- ml_status_item(resp$id,ml_token, "active")
      ml_crear_descripcion(resp$id, titulo,ml_token)
      mensaje <- paste0(item$id,": ",item$permalink)
      tryCatch(expr={
        publicaciones_slack <- readRDS("publicacions_slack.RDS")
        publicaciones_slack <- append(publicaciones_slack,item$id)
        saveRDS(publicaciones_slack,"publicacions_slack.RDS")
      },
      error=function(er){
        publicaciones_slack <- list(item$id)
        saveRDS(publicaciones_slack,"publicacions_slack.RDS")
      }
      )
      print(mensaje)
      slack_responder_en_hilo(bot_token,cuerpo$event$channel,cuerpo$event$event_ts,mensaje)
    }
  }
}

slack_iniciar_conversacion <- function(cuerpo,ml_token){
  tryCatch(
    expr={
      
      bot_token <- Sys.getenv("SLACK_BOT_TOKEN")
      mensaje_persona <- str_split(cuerpo$event$text,"<@U08LEBF4Q85> ")[[1]][2]
      texto_sin_espacio_inicio <- sub("^\\s+", "", mensaje_persona)
      sin_comillas <- str_remove_all(mensaje_persona, '"[^"]*"')
      numero_orden <- str_extract(sin_comillas, "\\d+")
      mensaje_enviar <- str_split(texto_sin_espacio_inicio,paste0("/ic ",numero_orden," "))[[1]][2]
      if(nchar(mensaje_enviar)>0){
        ml_order <- mlorder_bypackid(numero_orden,ml_token)
        if(length(ml_order$orders)!=0){
          ml_order <- mlorder_bypackid(paste0(ml_order$orders[[1]]$id),ml_token)
        }
        mensajes_ml <- ml_obtener_mensajes(ml_order,ml_token)
        if(length(mensaje_ml$messages) != 0){
          responder_mensaje(ml_order,ml_token,mensaje_enviar)
        }else{
          ml_primer_mensaje(ml_order,ml_order,mensaje_enviar)
          if(!last_response()$status_code %in% c(199:299)){
            responder_mensaje(ml_order,ml_token,mensaje_enviar)
          }
        }
        if(last_response()$status_code %in% c(199:299)){
          Sys.sleep(1)
          channel_id <- cuerpo$event$channel
          message_ts <- cuerpo$event$ts
          
          add_slack_reaction(Sys.getenv("SLACK_BOT_TOKEN"), channel_id, message_ts)
          print(mensaje)
          slack_responder_en_hilo(bot_token,cuerpo$event$channel,cuerpo$event$event_ts,mensaje)
        }else{
          mensaje_no <- "No se pudo enviar el mensaje, revise"
          mensaje_error <- paste0("No se pudo enviar el mensaje: ",mensaje_enviar,
                                  "\nURL: ",last_response()$url,
                                  "\nStatus",last_response()$status_code,
                                  "\nlast_response() %>% resp_body_json(): ",toJSON(last_response() %>% resp_body_json()))
          slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$event_ts,mensaje_no)
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
        }
      }
    },error=function(e){
      mensaje_error <- paste0("Hubo un error : ",e)
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
    }
  )
}

slack_mensaje_fulfillment_stock <- function(operaciones){
  operacion_id <- operaciones$id_resource
  for(i in seq_along(operacion_id)){
    cuerpo <- fromJSON(operaciones$body[[i]])
    if(cuerpo$user_id == Sys.getenv("SELLERID_ML_ASM")){
      recordid_token <- "recQLtjnMhd4ZCiJq"
      canal <- "mercadolibreasm"
    }else{
      recordid_token <- ""
      canal <- NULL
    }
    ml_token <- get_active_token(recordid_token)
    operacion <- ml_operaciones_fulfillment(operacion_id[[i]],ml_token)
    if(last_response()$status_code %in% c(199:299)){
      if(operacion$result$available_quantity==0 && operacion$result$total == 0){
        id_item <- ""
        permalink <- ""
        nombre <- ""
        publicacion <- airtable_getrecordslist("publicaciones",Sys.getenv("AIRTABLE_CES_BASE"),paste0("id_inventario_marketplace='",operacion$inventory_id,"'"))
        if(last_response()$status_code %in% c(199:299) && length(publicacion)!=0){
          id_item <- publicacion[[1]]$fields$id_canal
          nombre <- publicacion[[1]]$fields$titulo
          permalink <-   publicacion[[1]]$fields$link_ml$url      
        }
        mensaje_operacion <- paste0("El stock de la publicacion: ", id_item, "  ", nombre,"\n",permalink
                                    ,"\nID del inventario de fulfillment: ",operacion$inventory_id,"\n"
                                    ,"Se quedo sin stock en full por la operacion: ",operacion$type," ",operacion$detail$available_quantity)
        enviar_mensaje_slack(Sys.getenv("SLACK_STOCK_FULL_ML_URL"),mensaje_operacion)
      }
    }
  }
}

slack_cambiar_envio <- function(cuerpo){
  hilo <- slack_obtener_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$thread_ts)
  texto <- hilo[[1]]$text
  if(str_detect(cuerpo$event$text,"test")){
    return(0)
  }
  id_orden <- str_split(str_split(texto,"\\n")[[1]][1]," ")[[1]][1]
  orden_venta <- airtable_getrecordslist("ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),
                                         paste0("id_origen='",id_orden,"'"))
  id_solicitudes <- ""
  bandera <- T
  if(length(orden_venta)!=0){
    if(length(orden_venta[[1]]$fields$solicitud_produccion)!=0){
      for(i in seq_along(orden_venta[[1]]$fields$solicitud_produccion)){
        fields <- list("tipo_empaque"="estándar")
        airtable_updatesinglerecord(fields,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),
                                    orden_venta[[1]]$fields$solicitud_produccion[[i]])
        solicitud <- airtable_getrecorddata_byid(orden_venta[[1]]$fields$solicitud_produccion[[i]],"solicitudes_produccion",
                                                 Sys.getenv("AIRTABLE_CES_BASE"))
        id_solititud <- solicitud$fields$id_solicitud
        
        if(!last_response()$status_code %in% c(200:299)){
          mensaje_error <-paste0("Tuvimos un error: ",last_response()$status_code,"\n",
                                 toJSON(last_response() %>% resp_body_json()))
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
          respuesta_slack <- paste0("No se pudo cambiar el tipo de empaque de la solicitud: ", solicitud$fields$id_solicitud)
          slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$ts,respuesta_slack)
          id_solititud <- ""
        }
        id_solicitudes <- paste0(id_solicitudes,id_solititud," ")
      }
    }
    airtable_updatesinglerecord(list("envio_local"=FALSE),"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"),
                                orden_venta[[1]]$id)
    if(!last_response()$status_code %in% c(200:299)){
      mensaje_error <-paste0("Tuvimos un error: ",last_response()$status_code,"\n",
                             toJSON(last_response() %>% resp_body_json()))
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
      respuesta_slack <- paste0("No se pudo cambiar el envio local de: ", orden_venta[[1]]$fields$id_ordenes_venta)
      slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$ts,respuesta_slack)
    }else{
      respuesta_slack <- ""
      if(str_detect(id_solicitudes,"SP")){
        respuesta_slack <- paste0("Se modifico exitosamente las solicitudes de produccion: ", id_solicitudes)
      }
      respuesta_slack <- paste0(respuesta_slack,"\n","Se modifico el envio local de la orden de venta:", 
                                orden_venta[[1]]$fields$id_ordenes_venta)
      slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$ts,respuesta_slack)
    }
  }
}

enviar_a_slack <- function(token,canal,mensaje = NULL,archivo = NULL,nombre_archivo = NULL,titulo_archivo = NULL){
  if (!is.null(mensaje)) {
    res <- request("https://slack.com/api/chat.postMessage") %>% 
      req_method("POST") %>% 
      req_headers(Authorization = paste("Bearer", token)) %>% 
      req_body_json(list(
        channel = canal,
        text = mensaje
      )) %>% 
      req_error(is_error = function(resp) FALSE) %>%
      req_perform() %>% 
      resp_body_json()
    if(last_response()$status_code %in% c(199:299)){
      return(res)
    }else{
      mensaje_error <- paste0("No se pudo enviar el mensaje: ", mensaje,
                              "\nRevisa ", last_response() %>% resp_body_string())
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
    }
    
  }
  
  # Enviar archivo si se proporciona
  if (!is.null(archivo)) {
    if (!file.exists(archivo)) {
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),"Archivo no encontrado")
    }
    
    res <- request("https://slack.com/api/files.upload") %>% 
      req_method("POST") %>% 
      req_headers(Authorization = paste("Bearer", token)) %>% 
      req_body_multipart(
        channels = canal,
        file = curl::form_file(archivo, type = "auto"),
        filename = nombre_archivo %||% basename(archivo),
        title = titulo_archivo %||% basename(archivo)
      ) %>% 
      req_perform() %>% 
      resp_body_json()
    
    if(last_response()$status_code %in% c(199:299)){
      return(res)
    }else{
      mensaje_error <- paste0("No se pudo enviar el mensaje: ", mensaje,
                              "\nRevisa ", last_response() %>% resp_body_string())
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
    }
  }
}

buscar_mensajes_slack <- function(texto, token, max_paginas = 10) {
  url <- "https://slack.com/api/search.messages"
  resultados <- list()
  pagina <- 1
  
  while (pagina <= max_paginas) {
    #cat("Página", pagina, "\n")
    
    req <- request(url) %>% 
      req_headers(Authorization = paste("Bearer", token)) %>% 
      req_url_query(query = texto, count = 100, page = pagina)
    
    resp <- req_perform(req)
    data <- resp_body_json(resp)
    
    if (!data$ok){
      mensaje <- paste("ocurrio un error al buscar mensajes ", data$error)
    } 
    
    resultados <- append(resultados, data$messages$matches)
    
    total_paginas <- data$messages$pagination$page_count
    if (pagina >= total_paginas) break
    
    pagina <- pagina + 1
  }
  
  # Convertir resultados a data.frame
  data.frame(
    canal   = sapply(resultados, \(x) x$channel$name),
    usuario = sapply(resultados, \(x) x$username),
    texto   = sapply(resultados, \(x) x$text),
    ts      = sapply(resultados, \(x) x$ts),
    stringsAsFactors = FALSE
  )
}

add_slack_reaction <- function(token, channel, timestamp, emoji = "white_check_mark") {
  request("https://slack.com/api/reactions.add") %>% 
    req_method("POST") %>% 
    req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) %>% 
    req_body_json(list(
      name = emoji,
      channel = channel,
      timestamp = timestamp
    )) %>% 
    req_perform() %>% 
    resp_body_json()
}

slack_responder_mensajes_ml <- function(resp_mensaje){
  texto <- resp_mensaje$event$text
  if(!is.null(resp_mensaje$event$thread_ts)){
    ts <- resp_mensaje$event$thread_ts
  }else{
    ts <- resp_mensaje$event$ts
  }
  hilo<- slack_obtener_hilo(Sys.getenv("SLACK_BOT_TOKEN"),resp_mensaje$event$channel,ts)
  if(!is.null(hilo[[1]]$subtype)){
    if(str_detect(texto,"<@U08LEBF4Q85> ") ){
      if(str_detect(hilo[[1]]$text,Sys.getenv("SELLERID_ML_ASM"))){
        recordid_token <- "recQLtjnMhd4ZCiJq"
        canal <- "mercadolibreasm"
      }else{
        recordid_token <- ""
        canal <- NULL
      }
      ml_token <- get_active_token(recordid_token)
      mensaje_slack <- str_split(texto,"<@U08LEBF4Q85> ")[[1]][2]
      id <- str_split(hilo[[1]]$text,":\n")[[1]][1]
      if(es_numero(id) && nchar(mensaje_slack)!=0){
        ml_order <- mlorder_bypackid(id,ml_token)
        if(length(ml_order$orders)!=0){
          ml_order <- mlorder_bypackid(paste0(ml_order$orders[[1]]$id),ml_token)
        }
        responder_mensaje(ml_order,ml_token,mensaje_slack)
        if(last_response()$status_code %in% (199:299)){
          respuesta_slack <- paste0("Se respondio el mensaje")
          channel_id <- resp_mensaje$event$channel
          message_ts <- resp_mensaje$event$ts
          
          add_slack_reaction(Sys.getenv("SLACK_BOT_TOKEN"), channel_id, message_ts)
          
          #slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),resp_mensaje$event$channel,resp_mensaje$event$ts,respuesta_slack)
        }else{
          mensaje_error <- paste0("No se pudo enviar el mensaje de respuesta: ",texto,
                                  "\nDel canal mensajes_mercado_libre ",toJSON(last_response() %>% resp_body_json()))
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
        }
      } 
    }
  }else{
    if(str_detect(texto,"<@U08LEBF4Q85> ") ){
      if(str_detect(hilo[[1]]$text,Sys.getenv("SELLERID_ML_ASM"))){
        recordid_token <- "recQLtjnMhd4ZCiJq"
        canal <- "mercadolibreasm"
      }else{
        recordid_token <- ""
        canal <- NULL
      }
      ml_token <- get_active_token(recordid_token)
      if(str_detect(texto,"<@U08LEBF4Q85> /ic")){
        mensaje_persona <- str_split(texto,"<@U08LEBF4Q85> ")[[1]][2]
        texto_sin_espacio_inicio <- sub("^\\s+", "", mensaje_persona)
        sin_comillas <- str_remove_all(mensaje_persona, '"[^"]*"')
        numero_orden <- str_extract(sin_comillas, "\\d+")
        mensaje_slack <- str_split(texto_sin_espacio_inicio,paste0("/ic ",numero_orden," "))[[1]][2]
        id <- numero_orden
      }else{
        id <- str_split(hilo[[1]]$text,":\n")[[1]][1]
        mensaje_slack <- str_split(texto,"<@U08LEBF4Q85> ")[[1]][2]
      }
      
      if(es_numero(id) && nchar(mensaje_slack)!=0){
        ml_order <- mlorder_bypackid(id,ml_token)
        if(length(ml_order$orders)!=0){
          ml_order <- mlorder_bypackid(paste0(ml_order$orders[[1]]$id),ml_token)
        }
        responder_mensaje(ml_order,ml_token,mensaje_slack)
        if(last_response()$status_code %in% (199:299)){
          respuesta_slack <- paste0("Se respondio el mensaje")
          channel_id <- resp_mensaje$event$channel
          message_ts <- resp_mensaje$event$ts
          
          add_slack_reaction(Sys.getenv("SLACK_BOT_TOKEN"), channel_id, message_ts)
          
          #slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),resp_mensaje$event$channel,resp_mensaje$event$ts,respuesta_slack)
        }else{
          mensaje_error <- paste0("No se pudo enviar el mensaje de respuesta: ",texto,
                                  "\nDel canal mensajes_mercado_libre ",toJSON(last_response() %>% resp_body_json()))
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
        }
      } 
    }
  }
}

slack_status_publi <- function(cuerpo,ml_token,amz_token){
  tryCatch(
    expr={
      mensaje_confirmacion <- "" 
      if(str_detect(tolower(cuerpo$event$text),"agencia")){
        if(str_detect(tolower(cuerpo$event$text),"activar")){
          ml_status_publicacion_agencia(ml_token,"active",F)
          mensaje_confirmacion <- paste0("Se activaron exitosamente las publicaciones de agencia")
        }
        if(str_detect(tolower(cuerpo$event$text),"pausar")){
          ml_status_publicacion_agencia(ml_token,"paused",F)
          mensaje_confirmacion <- paste0("Se pausaron exitosamente las publicaciones de agencia")
        }
        if(last_response()$status_code %in% c(199:299)){
          slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$thread_ts,mensaje_confirmacion)
        }
      }else{
        sku <- str_split(cuerpo$event$text," ")[[1]][3]
        if(is.na(as.numeric(sku))){
          slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$thread_ts,"No se pudo encontrar el sku recuerda que va al final")
          return(0)
        }
        producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",sku))
        if(length(producto)==0){
          slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$thread_ts,"No se pudo encontrar el producto revisa el sku")
          return(0)
        }else{
          producto <- producto[[1]]
        }
        
        if(str_detect(tolower(cuerpo$event$text),"activar")){
          if(length(producto$fields$pertence_paquetes)!=0){
            producto_partes <- airtable_getrecordslist("paquetes_producto",Sys.getenv("AIRTABLE_CES_BASE"),paste0("FIND('",producto$fields$id_productos,"',{parte})"))
            if(length(producto_partes)!=0){
              aux_productos_parte <- sapply(producto_partes,function(x){
                res <- airtable_getrecorddata_byid(x$fields$productos[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
              },simplify = F) %>% compact()
              
              publicaciones_amazon <- lapply(aux_productos_parte, function(x) {
                res <- airtable_getrecordslist(
                  "publicaciones",
                  Sys.getenv("AIRTABLE_CES_BASE"),
                  paste0("AND(canal='amazon randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                         x$fields$id_productos, "',{producto}))")
                )
                if (length(res) == 0 || is.null(res)) return(NULL)
                return(res)
              })
              
              publicaciones_amazon <- Filter(Negate(is.null), publicaciones_amazon)
              
              publicaciones_amazon <- unlist(publicaciones_amazon, recursive = FALSE)
              
              amz_token <- amz_get_active_token()
              if(length(publicaciones_amazon)!=0){
                for(publi_amz in publicaciones_amazon){
                  item_amz <- amz_getitems(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),publi_amz$fields$product_id)
                  if(length(item_amz$fulfillmentAvailability)!=0){
                    if(length(item_amz$fulfillmentAvailability[[1]]$quantity)!=0){
                      if(item_amz$fulfillmentAvailability[[1]]$quantity==0){
                        amazon_update_listing(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),item_amz$sku,"25")
                        if(!last_response()$status_code %in% c(199:299)){
                          mensaje_amz <- paste0("Ocurrio un error al pausar el item: ",item_amz$sku,"\nError: ",
                                                last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_amz)
                        }
                      }
                    }
                  }
                }
                mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se activaron las publicaciones de Amazon")
              }
              
              
              publicaciones_ml <- lapply(aux_productos_parte, function(x) {
                res <- airtable_getrecordslist(
                  "publicaciones",
                  Sys.getenv("AIRTABLE_CES_BASE"),
                  paste0("AND(canal='mercadolibre randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                         x$fields$id_productos, "',{producto}))")
                )
                if (length(res) == 0 || is.null(res)) return(NULL)
                return(res)
              })
              
              publicaciones_ml <- Filter(Negate(is.null), publicaciones_ml)
              
              publicaciones_ml <- unlist(publicaciones_ml, recursive = FALSE)
              
              ml_token <- get_active_token()
              if(length(publicaciones_ml)!=0){
                for(publi_ml in publicaciones_ml){
                  item_ml <- ml_obtener_item(publi_ml$fields$id_canal,ml_token)
                  if(!is.null(item_ml$status)){
                    if(!is.null(item_ml$available_quantity) ){
                      if(item_ml$available_quantity<=0){
                        item_aux <- ml_stock_item(item_ml$id,ml_token,10)
                        if(item_aux$status=="active"){
                          next
                        }
                      }
                    }
                    if(item_ml$status=="paused"){
                      ml_status_item(item_ml$id,ml_token,"active")
                      if(!last_response()$status_code %in% c(199:299)){
                        mensaje_ml <- paste0("Ocurrio un error al pausar el item: ",item_amz$id,"\nError: ",
                                             last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_ml)
                      }
                    }
                  }
                }
                if(mensaje_confirmacion!=''){
                  mensaje_confirmacion <- paste0(mensaje_confirmacion," y Mercado Libre exitosamente")
                }else{
                  mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se activaron las publicaciones de Mercado Libre")
                }
              }
              
              
            }
            
          }else{
            publicaciones_amazon <- airtable_getrecordslist(
              "publicaciones",
              Sys.getenv("AIRTABLE_CES_BASE"),
              paste0("AND(canal='amazon randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                     producto$fields$id_productos, "',{producto}))"))
            if(length(publicaciones_amazon)!=0){
              for(publi_amz in publicaciones_amazon){
                item_amz <- amz_getitems(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),publi_amz$fields$product_id)
                if(length(item_amz$fulfillmentAvailability)!=0){
                  if(length(item_amz$fulfillmentAvailability[[1]]$quantity)!=0){
                    if(item_amz$fulfillmentAvailability[[1]]$quantity==0){
                      amazon_update_listing(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),item_amz$sku,"25")
                      if(!last_response()$status_code %in% c(199:299)){
                        mensaje_amz <- paste0("Ocurrio un error al pausar el item: ",item_amz$sku,"\nError: ",
                                              last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_amz)
                      }
                    }
                  }
                }
                
              }
              mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se activaron las publicaciones de Amazon")
            }
            
            publicaciones_ml <- airtable_getrecordslist(
              "publicaciones",
              Sys.getenv("AIRTABLE_CES_BASE"),
              paste0("AND(canal='mercadolibre randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                     producto$fields$id_productos, "',{producto}))"))
            if(length(publicaciones_ml)!=0){
              for(publi_ml in publicaciones_ml){
                item_ml <- ml_obtener_item(publi_ml$fields$id_canal,ml_token)
                if(item_ml$available_quantity<=0){
                  item_aux <- ml_stock_item(item_ml$id,ml_token,10)
                  if(item_aux$status=="active"){
                    next
                  }
                }
                if(!is.null(item_ml$status)){
                  if(item_ml$status=="paused"){
                    ml_status_item(item_ml$id,ml_token,"active")
                    if(!last_response()$status_code %in% c(199:299)){
                      mensaje_ml <- paste0("Ocurrio un error al pausar el item: ",item_amz$id,"\nError: ",
                                           last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_ml)
                    }
                  }
                }
              }
              if(mensaje_confirmacion!=''){
                mensaje_confirmacion <- paste0(mensaje_confirmacion," y Mercado Libre exitosamente")
              }else{
                mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se activaron las publicaciones de Mercado Libre")
              }
            }
          }
        }
        if(str_detect(tolower(cuerpo$event$text),"pausar")){
          if(length(producto$fields$pertenece_paquetes)!=0){
            producto_partes <- airtable_getrecordslist("paquetes_producto",Sys.getenv("AIRTABLE_CES_BASE"),paste0("FIND('",producto$fields$id_productos,"',{parte})"))
            if(length(producto_partes)!=0){
              aux_productos_parte <- sapply(producto_partes,function(x){
                res <- airtable_getrecorddata_byid(x$fields$productos[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
              },simplify = F) %>% compact()
              
              publicaciones_amazon <- lapply(aux_productos_parte, function(x) {
                res <- airtable_getrecordslist(
                  "publicaciones",
                  Sys.getenv("AIRTABLE_CES_BASE"),
                  paste0("AND(canal='amazon randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                         x$fields$id_productos, "',{producto}))")
                )
                if (length(res) == 0 || is.null(res)) return(NULL)
                return(res)
              })
              
              publicaciones_amazon <- Filter(Negate(is.null), publicaciones_amazon)
              
              publicaciones_amazon <- unlist(publicaciones_amazon, recursive = FALSE)
              
              amz_token <- amz_get_active_token()
              if(length(publicaciones_amazon)!=0){
                for(publi_amz in publicaciones_amazon){
                  item_amz <- amz_getitems(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),publi_amz$fields$product_id)
                  if(length(item_amz$fulfillmentAvailability)!=0){
                    if(length(item_amz$fulfillmentAvailability[[1]]$quantity)!=0){
                      if(item_amz$fulfillmentAvailability[[1]]$quantity>0){
                        amazon_update_listing(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),item_amz$sku,"0")
                        if(!last_response()$status_code %in% c(199:299)){
                          mensaje_amz <- paste0("Ocurrio un error al pausar el item: ",item_amz$sku,"\nError: ",
                                                last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_amz)
                        }
                      }
                    }
                  }
                }
                mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se pausaron las publicaciones de Amazon")
              }
              
              
              publicaciones_ml <- lapply(aux_productos_parte, function(x) {
                res <- airtable_getrecordslist(
                  "publicaciones",
                  Sys.getenv("AIRTABLE_CES_BASE"),
                  paste0("AND(canal='mercadolibre randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                         x$fields$id_productos, "',{producto}))")
                )
                if (length(res) == 0 || is.null(res)) return(NULL)
                return(res)
              })
              
              publicaciones_ml <- Filter(Negate(is.null), publicaciones_ml)
              
              publicaciones_ml <- unlist(publicaciones_ml, recursive = FALSE)
              
              ml_token <- get_active_token()
              if(length(publicaciones_ml)!=0){
                for(publi_ml in publicaciones_ml){
                  item_ml <- ml_obtener_item(publi_ml$fields$id_canal,ml_token)
                  if(!is.null(item_ml$status)){
                    if(item_ml$status=="active"){
                      ml_status_item(item_ml$id,ml_token,"paused")
                      if(!last_response()$status_code %in% c(199:299)){
                        mensaje_ml <- paste0("Ocurrio un error al pausar el item: ",item_amz$id,"\nError: ",
                                             last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_ml)
                      }
                    }
                  }
                }
                if(mensaje_confirmacion!=''){
                  mensaje_confirmacion <- paste0(mensaje_confirmacion," y Mercado Libre exitosamente")
                }else{
                  mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se activaron las publicaciones de Mercado Libre exitosamente")
                }
              }
              
              
            }
            
          }else{
            publicaciones_amazon <- airtable_getrecordslist(
              "publicaciones",
              Sys.getenv("AIRTABLE_CES_BASE"),
              paste0("AND(canal='amazon randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                     producto$fields$id_productos, "',{producto}))"))
            if(length(publicaciones_amazon)!=0){
              for(publi_amz in publicaciones_amazon){
                item_amz <- amz_getitems(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),publi_amz$fields$product_id)
                if(length(item_amz$fulfillmentAvailability)!=0){
                  if(length(item_amz$fulfillmentAvailability[[1]]$quantity)!=0){
                    if(item_amz$fulfillmentAvailability[[1]]$quantity>0){
                      amazon_update_listing(amz_token,Sys.getenv("SELLERID_AMZ_RANDU"),item_amz$sku,"0")
                      if(!last_response()$status_code %in% c(199:299)){
                        mensaje_amz <- paste0("Ocurrio un error al pausar el item: ",item_amz$sku,"\nError: ",
                                              last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                        enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_amz)
                      }
                    }
                  }
                }
                
              }
              mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se pausaron las publicaciones de Amazon")
            }
            
            publicaciones_ml <- airtable_getrecordslist(
              "publicaciones",
              Sys.getenv("AIRTABLE_CES_BASE"),
              paste0("AND(canal='mercadolibre randu',status!='',tipo_envio!='Fulfillment by marketplace',FIND('", 
                     producto$fields$id_productos, "',{producto}))"))
            if(length(publicaciones_ml)!=0){
              for(publi_ml in publicaciones_ml){
                item_ml <- ml_obtener_item(publi_ml$fields$id_canal,ml_token)
                if(!is.null(item_ml$status)){
                  if(item_ml$status=="active"){
                    ml_status_item(item_ml$id,ml_token,"paused")
                    if(!last_response()$status_code %in% c(199:299)){
                      mensaje_ml <- paste0("Ocurrio un error al pausar el item: ",item_amz$id,"\nError: ",
                                           last_response()$status_code,"\n Body: ",last_response() %>% resp_body_string())
                      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_ml)
                    }
                  }
                }
              }
              if(mensaje_confirmacion!=''){
                mensaje_confirmacion <- paste0(mensaje_confirmacion," y Mercado Libre exitosamente")
              }else{
                mensaje_confirmacion <- paste0(mensaje_confirmacion,"Se activaron las publicaciones de Mercado Libre")
              }
            }
          }
        }
        if(mensaje_confirmacion!=''){
          slack_responder_en_hilo(Sys.getenv("SLACK_BOT_TOKEN"),cuerpo$event$channel,cuerpo$event$thread_ts,mensaje_confirmacion)
        }
      }
      
      
    },error=function(e){
      mensaje_error <- paste0("Ocurrio un error al procesar un mensaje de slack: ",e,"\nEl cuerpo es:\n",toJSON(cuerpo))
      enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
    }
  )
  
}
