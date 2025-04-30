enviar_mensaje_slack <- function(url,mensaje){
  request <- request(url) %>%
    req_method("POST") %>%
    req_headers("Content-Type" = "application/json") %>%
    req_body_json(list(text = mensaje)) %>%
    req_perform()
}

slack_mensaje_pregunta <- function(questions_id, ml_token){
  for(i in seq_along(questions_id)){
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

