library(httr2)
library(jsonlite)
if (!require("emayili")) {install.packages("emayili")}
if (!require("tidyr")) {install.packages("tidyr")}
library(tidyr)
library(emayili)

generar_qr_imagen <- function(link_qr,sp,nombre_producto,recordid){
  renderform_api_key  <- Sys.getenv("RENDERFORM_API_KEY")
  renderform_template_id <- "faulty-foxes-sting-lazily-1437"
  renderform_url <- "https://api.renderform.io/api/v2/render"
  body <- list(
    template = renderform_template_id,
    data = list(
      "id_solicitud.text" = sp,
      "descripcion.text"= nombre_producto,
      "codigo_barras.src"= link_qr 
    )
  )
  
  response <- request(renderform_url) %>%
    req_headers("Content-Type" = "application/json") %>%
    req_headers("x-api-key" = renderform_api_key) %>%
    req_body_json(body) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  airtable_updatesinglerecord(list('qr_image'=list(list('url'=response$href))),"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),recordid)
  
  if(last_response()$status_code %in% c(199:299)){
    return(response$href)
  }else{
    print("Ocurrio un problema al subirla imagen: ",sp) 
    return(NULL)
  }
}

#-----Enviar por correo los errores------
email_error <- function(status,funcion,origen,archivo=""){
  
  
  email <- envelope() %>%
    from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
    to(Sys.getenv("EMAIL_ERROR_FAST_MAIL") ) %>%
    subject(paste0("Error: ",uuid::UUIDgenerate())) %>%
    text(paste0("¡Tuvimos un error ", status,"! Con en la funcion: ",funcion, ", y origen: ",origen, ". Adjunto archivo")) %>%
    attachment(path = archivo) # Ruta al archivo a adjuntar
  
  smtp <- server(
    host = "smtp.fastmail.com",
    port = 465,
    username = Sys.getenv("EMAIL_FAST_MAIL"),
    password = Sys.getenv("EMAIL_KEY"),
    use_ssl = TRUE
  )
  
  smtp(email)
  
}
email_error_general <- function(mensaje,archivo=NULL){
  email <- envelope() %>%
    from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
    to(Sys.getenv("EMAIL_ERROR_FAST_MAIL") ) %>%
    subject(paste0("Error : ",uuid::UUIDgenerate())) %>%
    text(paste0("¡Tuvimos un problema: ", mensaje)) 
  
  if(!is.null(archivo)){
    email <- email %>% attachment(path = archivo) # Ruta al archivo a adjuntar
  }
  
  smtp <- server(
    host = "smtp.fastmail.com",
    port = 465,
    username = Sys.getenv("EMAIL_FAST_MAIL"),
    password = Sys.getenv("EMAIL_KEY"),
    use_ssl = TRUE
  )
  
  smtp(email)
}

guardar <- function(origen="", resp, req, con, func, tabla){
  tryCatch(expr={
    logs <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(resp_body_json(resp)),
                   url=resp$url, status=resp$status_code, header =toJSON(req$headers),
                   request=toJSON(req$body$data),funcion=func, origenes=origen)
    insertar_fila(con, tabla, logs)
  },
  error=function(er){
    print(er)
    tabla <- tibble(time=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),rspns=toJSON(resp_body_json(resp)),
                    url=resp$url,status=resp$status_code,header =toJSON(req$headers),
                    request=toJSON(req$body$data), funciones=func,origenes=origen)
    saveRDS(tabla,"api_logs.RDS",compress = FALSE)
    print("Ocurrio un error al en la conexion")
  })
}

