paquetes <- c("tidyr","emayili")

cargar_paquetes <- function(paquetes) {
  for (pkg in paquetes) {
    if (!pkg %in% loadedNamespaces()) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    }
  }
}

cargar_paquetes(paquetes)

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
  #airtable_updatesinglerecord(list('qr_image'=list(list('url'=response$href))),"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),recordid)
  
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

email_error_general <- function(mensaje,archivo=NULL, max_retries=3, delay=5){
  print(mensaje)
  if(nchar(mensaje)>300){
    mensaje <- substr(mensaje,1,300)
  }
  Sys.sleep(3)
  tryCatch(
    expr = {
      # email <- envelope() %>%
      #   from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
      #   to(Sys.getenv("EMAIL_ERROR_FAST_MAIL") ) %>%
      #   subject(paste0("Error : ",uuid::UUIDgenerate())) %>%
      #   text(paste0("¡Tuvimos un problema: ", mensaje)) 
      # 
      email <- envelope(
        to = Sys.getenv("EMAIL_ERROR_FAST_MAIL"),
        from = Sys.getenv("EMAIL_FAST_MAIL"),
        subject = paste0("Error : ",uuid::UUIDgenerate()),
        text = paste0("¡Tuvimos un problema: ", mensaje)
      )
      
      if(!is.null(archivo)){
        email <- email %>% attachment(path = archivo) # Ruta al archivo a adjuntar
      }
      
      smtp <- server(
        host = "smtp.fastmail.com",
        port = 465,
        username = Sys.getenv("EMAIL_FAST_MAIL"),
        password = Sys.getenv("EMAIL_KEY"),
        use_ssl = TRUE,
        timeout = 25 
      )
      for (i in 1:max_retries) {
        tryCatch({
          # Enviar el correo
          smtp(email)
          message("Correo enviado exitosamente.")
          break  # Si se envió correctamente, salir del bucle
        }, error = function(e) {
          print(paste0("Intento de reenvío", i, "fallido. Error:", conditionMessage(e)))
          if (i < max_retries) {
            Sys.sleep(delay)  # Espera antes de intentar nuevamente
          } else {
            print("Reintentos agotados. No se pudo enviar el correo.")
          }
        })
      }
    },error=function(e){
      error_mensaje <- paste0(e)
      print(error_mensaje)
    }
  )
}

enviar_email <- function(mensaje,correo,archivo=NULL){
  email <- envelope() %>%
    from(Sys.getenv("EMAIL_FAST_MAIL") ) %>%
    to(correo) %>%
    subject(paste0("Mensaje : ",uuid::UUIDgenerate())) %>%
    text(paste0(mensaje)) 
  
  if(!is.null(archivo)){
    email <- email %>% attachment(path = archivo) # Ruta al archivo a adjuntar
  }
  
  smtp <- server(
    host = "smtp.fastmail.com",
    port = 465,
    username = Sys.getenv("EMAIL_FAST_MAIL"),
    password = Sys.getenv("EMAIL_KEY"),
    use_ssl = TRUE,
    timeout = 30 
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
  if(!str_detect(tolower(producto$fields$id_productos),"10700") & !str_detect(tolower(producto$fields$id_productos),"10276") & !str_detect(tolower(producto$fields$id_productos),"10011") & !str_detect(tolower(producto$fields$id_productos),"personalizado")){
    if(length(producto$fields$partes_producto) != 0){
      for(parte in producto$fields$partes_producto){
        #print(parte)
        aux_parte <- airtable_getrecorddata_byid(parte,"partes_producto",Sys.getenv("AIRTABLE_CES_BASE")) 
        #aux_parte <- airtable_getrecorddata_byid(parte,"partes_producto",Sys.getenv("AIRTABLE_RIR_BASE"))
        if(!is.null(aux_parte$fields$parte)){
          
          parte_producto <- airtable_getrecorddata_byid(aux_parte$fields$parte[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
          #parte_producto <- airtable_getrecorddata_byid(aux_parte$fields$parte[[1]],"productos",Sys.getenv("AIRTABLE_RIR_BASE"))
          if(parte_producto$fields$cantidad_disponible<venta_producto$fields$cantidad){
            if(!is.null(parte_producto$fields$item_produccion)){
              if(str_detect(tolower(producto$fields$id_productos),"juego")){
                fields[[length(fields) + 1]] <- list(
                  "tabla"="solicitudes",
                  "comentarios"= paste0(orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
                  "cantidad"=venta_producto$fields$cantidad,
                  "producto"=list(producto$id),
                  "venta_producto"=list(venta_producto$id),
                  "tipo_empaque"="estándar",
                  "origen"="pedido"
                )
              }else{
                fields[[length(fields) + 1]] <- list(
                  "tabla"="solicitudes",
                  "comentarios"= paste0(orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
                  "cantidad"=venta_producto$fields$cantidad*aux_parte$fields$cantidad,
                  "producto"=list(parte_producto$id),
                  "venta_producto"=list(venta_producto$id),
                  "tipo_empaque"="estándar",
                  "origen"="pedido"
                )
              }
              
              if(parte_producto$fields$categoria == "Empaque"){
                fields[[length(fields)]]$origen <- "empaque CNC"
              }
              if(orden_venta$fields$canal_venta=="mercadolibrernd"){
                ml_token <- get_active_token()
                ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
                if(!is.null(ml_order$shipping$id)){
                  ml_shipping <- get_dir_mlorder(ml_order,ml_token)
                  if(ml_shipping$logistic$mode == "me2"){
                    fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="8 - Antes de la 1"))
                  } else{
                    fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="2 - Media Alta"))
                  }
                }else{
                  fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="2 - Media Alta"))
                }
              }
              if(orden_venta$fields$canal_venta=="shprndmx"){
                fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="1 - Media"))
              }
              if(orden_venta$fields$canal_venta=="amazonrnd" | orden_venta$fields$canal_venta=="amazonasm"){
                fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="7 - Extrema"))
              }
              if(orden_venta$fields$canal_venta=="walmartrnd"){
                fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="7 - Extrema"))
              }
              if(orden_venta$fields$canal_venta=="dstrnd"){
                fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="3 - Alta"))
              }
              #airtable_createrecord(fields,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
              
            }
            else{
              print("No se registro la solicitud de produccion porque una parte de stock no esta disponible")
              print(venta_producto$fields$id_ventas_producto)
              break
            }
          }
          else{
            fields[[length(fields) + 1]] <- list(
              "tabla"="transacciones",
              'tipo'='reserva',
              "producto"=list(parte_producto$id),
              "ventas_producto"=list(venta_producto$id),
              "comentarios"= paste0(orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
              "cantidad"=venta_producto$fields$cantidad*aux_parte$fields$cantidad
            )
            if(is.null(parte_producto$fields$item_produccion)){
              cantidad_restante <- parte_producto$fields$cantidad_disponible - venta_producto$fields$cantidad
              if(cantidad_restante<=6){
                mensaje <- paste0("Advertencia: El producto ", parte_producto$fields$id_productos, "solo cuenta con ", 
                                  cantidad_restante, " unidades\nRevisa")
                enviar_mensaje_slack(Sys.getenv("SLACK_STOCK_URL"),mensaje)
                #enviar_email(mensaje,"mauricio@randu.mx")
                #enviar_email(mensaje,"yatzel@randu.mx")
              }
            }
          }
          
        }
      }
      if(length(fields) == length(producto$fields$partes_producto)){
        for(field in fields){
          if(field$tabla == "transacciones"){
            field$tabla <- NULL
            aux <- airtable_createrecord(field,"transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
          }else{
            field$tabla <- NULL
            aux <- airtable_createrecord(field,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
            if(!is.null(aux)){
              link_qr <- aux$fields$barcode_link
              if(is.null(link_qr)){
                link_qr <- paste0("https://barcodeapi.org/api/qr/",aux$fields$id_solicitud,"%7C",aux$id)
              }
              sp <- aux$fields$id_solicitud
              nombre_producto <- aux$fields$producto_solicitado
              url_etiqueta <- generar_qr_imagen(link_qr ,sp,nombre_producto,aux$id)
            }
            # airtable_updatesinglerecord(list('qr_image' = list(list('url'= url_etiqueta))),
            #                             'solicitudes_produccion',Sys.getenv("AIRTABLE_CES_BASE"),aux$id)
          }
          if(is.null(aux)){
            break
          }
          print(paste0("-Se registro exitosamente el proceso necesario para el producto"))
          print(paste0(venta_producto$fields$id_ventas_producto))
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
            "comentarios"= paste0(orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
            "cantidad"=venta_producto$fields$cantidad,
            "producto"=list(producto$id),
            "venta_producto"=list(venta_producto$id),
            "tipo_empaque"="estándar",
            "origen"="pedido"
          )
          if(orden_venta$fields$canal_venta=="mercadolibrernd"){
            ml_token <- get_active_token()
            ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
            if(!is.null(ml_order$shipping$id)){
              ml_shipping <- get_dir_mlorder(ml_order,ml_token)
              if(ml_shipping$logistic$mode == "me2"){
                fields <- append(fields,list('prioridad'="8 - Antes de la 1"))
              } else{
                fields <- append(fields,list('prioridad'="2 - Media Alta"))
              }
            }else{
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
          
          print(paste0("-Las solicitudes de la venta producto : ", venta_producto$fields$id_ventas_producto, " se registro exitosamente"))
          if(!is.null(aux)){
            link_qr <- aux$fields$barcode_link
            if(is.null(link_qr)){
              link_qr <- paste0("https://barcodeapi.org/api/qr/",aux$fields$id_solicitud,"%7C",aux$id)
            }
            sp <- aux$fields$id_solicitud
            nombre_producto <- aux$fields$producto_solicitado
            url_etiqueta <- generar_qr_imagen(link_qr ,sp,nombre_producto,aux$id)
            airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
          }
          
        }
        else{
          print(paste0("No hay stock de producto: ",producto$fields$id_productos))
          print(venta_producto$fields$id_ventas_producto)
        }
      }else{
        
        fields <- list(
          'tipo'='reserva',
          "producto"=list(producto$id),
          "ventas_producto"=list(venta_producto$id),
          "comentarios"= paste0(orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
          "cantidad"=venta_producto$fields$cantidad
        )
        if(is.null(producto$fields$item_produccion)){
          cantidad_restante <- producto$fields$cantidad_disponible - venta_producto$fields$cantidad
          if(cantidad_restante<=6){
            mensaje <- paste0("Advertencia: El producto ", producto$fields$id_productos, "solo cuenta con ", 
                              cantidad_restante, " unidades\nRevisa")
            enviar_mensaje_slack(Sys.getenv("SLACK_STOCK_URL"),mensaje)
            #enviar_email(mensaje,"mauricio@randu.mx")
            #enviar_email(mensaje,"yatzel@randu.mx")
          }
        }
        aux <- airtable_createrecord(fields,"transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
        print(paste0("-Se registro exitosamente el proceso necesario para el producto"))
        print(paste0("-",venta_producto$fields$id_ventas_producto))
        if(!is.null(aux)){
          airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
        }else{
          print(paste0("-Revisar la venta producto: ",venta_producto$fields$id_ventas_producto))
          print(154)
          return(1)
        }
      }
    } 
  }
  return(0)
}

es_numero <- function(cadena) {
  return(grepl("^[0-9]+$", cadena))
}

ultimo_dia_mes_anterior <- function(fecha) {
  fecha <- as.Date(fecha)
  primer_dia_mes_actual <- as.Date(format(fecha, "%Y-%m-01"))
  ultimo_dia_mes_anterior <- primer_dia_mes_actual - 1
  hora_actual <- Sys.time()-6*3600
  resultado <- as.POSIXct(paste(ultimo_dia_mes_anterior, format(hora_actual, "%H:%M:%S")), tz = "UTC")
  return(resultado)
}

ejecutar <- function(horas_permitidas){
  hora_actual <- format(Sys.time()-6*3600, "%H:%M")
  return(hora_actual %in% horas_permitidas)
}

quitar_null <- function(lista){
  mi_lista_sin_null <- Filter(Negate(is.null), lista)
  return(mi_lista_sin_null)
}
