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

registrar_producto <- function(producto,venta_producto){
  orden_venta <- airtable_getrecorddata_byid(venta_producto$fields$ordenes_venta[[1]],"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
  fields <- list()
  #print(str_detect(tolower(producto$fields$id_productos),"juego"))
  if(!str_detect(tolower(producto$fields$id_productos),"juego") & !str_detect(tolower(producto$fields$id_productos),"10700")){
    if(length(producto$fields$partes_producto) != 0){
      for(parte in producto$fields$partes_producto){
        print(parte)
        aux_parte <- airtable_getrecorddata_byid(parte,"partes_producto",Sys.getenv("AIRTABLE_CES_BASE")) 
        #aux_parte <- airtable_getrecorddata_byid(parte,"partes_producto",Sys.getenv("AIRTABLE_RIR_BASE"))
        if(!is.null(aux_parte$fields$parte)){
          
          parte_producto <- airtable_getrecorddata_byid(aux_parte$fields$parte[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
          #parte_producto <- airtable_getrecorddata_byid(aux_parte$fields$parte[[1]],"productos",Sys.getenv("AIRTABLE_RIR_BASE"))
          if(parte_producto$fields$cantidad_disponible<venta_producto$fields$cantidad){
            if(!is.null(parte_producto$fields$item_produccion)){
              fields[[length(fields) + 1]] <- list(
                "tabla"="solicitudes",
                "comentarios"= "Creada mediante R",
                "cantidad"=venta_producto$fields$cantidad,
                "producto"=list(parte_producto$id),
                "venta_producto"=list(venta_producto$id),
                "tipo_empaque"="estándar",
                "origen"="pedido"
              )
              if(orden_venta$fields$canal_venta=="mercadolibrernd"){
                ml_token <- get_active_token()
                ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
                ml_shipping <- get_dir_mlorder(ml_order,ml_token)
                if(ml_shipping$logistic$mode == "me2"){
                  fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="8 - Antes de las 2"))
                } else{
                  fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="2 - Media Alta"))
                }
                
              }
              if(orden_venta$fields$canal_venta=="shprndmx"){
                fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="1 - Media"))
              }
              if(orden_venta$fields$canal_venta=="amazonrnd" | orden_venta$fields$canal_venta=="amazonasm"){
                fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="7 - Extrema"))
              }
              #airtable_createrecord(fields,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
              
            }
            else{
              print("No se registro la solicitud de produccion porque una parte de stock no esta disponible")
              break
            }
          }else{
            
            fields[[length(fields) + 1]] <- list(
              "tabla"="transacciones",
              'tipo'='reserva',
              "producto"=list(parte_producto$id),
              "ventas_producto"=list(venta_producto$id),
              "comentarios"="Creado mediante R",
              "cantidad"=venta_producto$fields$cantidad
            )
            
          }
          
        }
      }
      if(length(fields) == length(producto$fields$partes_producto)){
        for(field in fields){
          #print(!is.null(field$producto_solicitado))
          #print(field)
          if(field$tabla == "transacciones"){
            field$tabla <- NULL
            aux <- airtable_createrecord(field,"transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
          }else{
            field$tabla <- NULL
            aux <- airtable_createrecord(field,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
          }
          if(is.null(aux)){
            break
          }
          print(paste0("Se registro exitosamente el proceso necesario para el producto"))
        }
        if(!is.null(aux)){
          
          airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
        }else{
          print(last_response() %>% resp_body_json())
          print(paste0("Revisar la venta producto: ",venta_producto$fields$id_ventas_producto))
          print(101)
          return(1)
          
        }
      }
      else{print("No se registro")
        print(venta_producto$fields$id_ventas_producto)}
    }else{
      if(venta_producto$fields$cantidad>producto$fields$cantidad_disponible){
        if(!is.null(producto$fields$item_produccion)){
          fields <- list(
            "comentarios"= "Creada mediante R",
            "cantidad"=venta_producto$fields$cantidad,
            "producto"=list(producto$id),
            "venta_producto"=list(venta_producto$id),
            "tipo_empaque"="estándar",
            "origen"="pedido"
          )
          if(orden_venta$fields$canal_venta=="mercadolibrernd"){
            ml_token <- get_active_token()
            ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
            ml_shipping <- get_dir_mlorder(ml_order,ml_token)
            if(ml_shipping$logistic$mode == "me2"){
              fields <- append(fields,list('prioridad'="8 - Antes de las 2"))
            } else{
              fields <- append(fields,list('prioridad'="2 - Media Alta"))
            }
          }
          if(orden_venta$fields$canal_venta=="shprndmx"){
            fields <- append(fields,list('prioridad'="1 - Media"))
          }
          if(orden_venta$fields$canal_venta=="amazonrnd" | orden_venta$fields$canal_venta=="amazonasm"){
            fields <- append(fields,list('prioridad'="7 - Extrema"))
          }
          aux <- airtable_createrecord(fields,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
          if(!is.null(aux)){
            airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
          }
          
        }
        else{
          print(paste0("No hay stock de producto: ",producto$fields$id_productos))
        }
      }else{
        
        fields <- list(
          'tipo'='reserva',
          "producto"=list(producto$id),
          "ventas_producto"=list(venta_producto$id),
          "comentarios"="Creado mediante R",
          "cantidad"=venta_producto$fields$cantidad
        )
        aux <- airtable_createrecord(fields,"transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
        print(paste0("Se registro exitosamente el proceso necesario para el producto"))
        if(!is.null(aux)){
          airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
        }else{
          print(paste0("Revisar la venta producto: ",venta_producto$fields$id_ventas_producto))
          print(154)
          return(1)
        }
      }
    } 
  }
  return(0)
}

