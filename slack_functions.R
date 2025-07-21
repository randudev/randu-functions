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
    cat("Página", pagina, "\n")
    
    req <- request(url) |>
      req_headers(Authorization = paste("Bearer", token)) |>
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
  hilo<- slack_obtener_hilo(Sys.getenv("SLACK_BOT_TOKEN"),resp_mensaje$event$channel,resp_mensaje$event$thread_ts)
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
  }
}
