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

generar_qr_imagen <- function(link_qr,sp,nombre_producto,recordid,instructivo=NULL){
  renderform_api_key  <- Sys.getenv("RENDERFORM_API_KEY")
  renderform_template_id <- "faulty-foxes-sting-lazily-1437"
  renderform_url <- "https://api.renderform.io/api/v2/render"
  body <- list(
    template = renderform_template_id,
    data = list(
      "id_solicitud.text" = sp,
      "descripcion.text"= nombre_producto,
      "codigo_barras.src"= link_qr,
      "instructivo.text"=instructivo
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
    print(paste0("Ocurrio un problema al subirla imagen: ",sp))
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
  mensajes_enviar <- list()
  pausar <- list()
  orden_venta <- airtable_getrecorddata_byid(venta_producto$fields$ordenes_venta[[1]],"ordenes_venta",Sys.getenv("AIRTABLE_CES_BASE"))
  fields <- list()
  # if(orden_venta$fields$canal_venta == "shprndmx" && str_detect(tolower(producto$fields$id_productos),"escritorio de altura ajustable en escuadra base negro")){
  #   return(0)
  # }
  if(!str_detect(tolower(producto$fields$id_productos),"10700")  & !str_detect(tolower(producto$fields$id_productos),"10011") & !str_detect(tolower(producto$fields$id_productos),"personalizado")){
    if(length(producto$fields$paquetes) != 0){
      if(length(producto$fields$item_produccion)!=0){
        if(producto$fields$cantidad_disponible_navex93 < venta_producto$fields$cantidad){
          tipo_empaque <- "estándar"
          if(venta_producto$fields$cantidad >= 5){
            tipo_empaque <- "tarima"
          }
          if(!is.null(orden_venta$fields$entrega_qro) || !is.null(orden_venta$fields$entrega_cdmx)){
            tipo_empaque <- "ligero"
          }
          ov <- ""
          if(is.null(orden_venta$fields$id_origen)){
            ov <- orden_venta$fields$id_ordenes_venta
          }
          fields <- list(
            "comentarios"= paste0(ov,orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
            "cantidad"=venta_producto$fields$cantidad,
            "producto"=list(producto$id),
            "venta_producto"=list(venta_producto$id),
            "tipo_empaque"=tipo_empaque,
            "origen"="pedido",
            "fecha_solicitud"=Sys.time()-6*3600
          )
          
          if(orden_venta$fields$canal_venta=="mercadolibrernd"){
            ml_token <- get_active_token()
            ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
            if(!is.null(ml_order$shipping$id)){
              ml_shipping <- get_dir_mlorder(ml_order,ml_token)
              if(ml_shipping$logistic$mode == "me2"){
                fecha <- ml_shipping$lead_time$buffering$date
                if(is.null(fecha)){
                  fields <- append(fields,list('prioridad'="8 - Antes de las 12"))
                  dia_sp <- calcular_fecha_envio(ml_shipping)
                  fields$comentarios <- paste0("AGENCIA: ",format(dia_sp, "%Y-%m-%d")," ",fields$comentarios)
                }else{
                  fields <- append(fields,list('prioridad'=prioridad_agencia(fecha)))
                  fields$comentarios <- paste0("AGENCIA: ",format(as.POSIXct(fecha, tz = "UTC"), "%Y-%m-%d")," ",fields$comentarios)
                }
                
              } else{
                fields <- append(fields,list('prioridad'="2 - Media Alta"))
              }
            }else{
              fields <- append(fields,list('prioridad'="2 - Media Alta"))
            }
          }
          if(length(orden_venta$fields$canal_venta)!=0){
            if(orden_venta$fields$canal_venta=="shprndmx"){
              fields <- append(fields,list('prioridad'="1 - Media"))
            }
            if(orden_venta$fields$canal_venta=="amazonrnd" | orden_venta$fields$canal_venta=="amazonasm"){
              fields <- append(fields,list('prioridad'="7 - Extrema"))
            }
            if(orden_venta$fields$canal_venta=="walmartrnd" || orden_venta$fields$canal_venta=="coppel"){
              fields <- append(fields,list('prioridad'="7 - Extrema"))
            }
            if(orden_venta$fields$canal_venta=="dstrnd"){
              fields <- append(fields,list('prioridad'="3 - Alta"))
            }
            if(orden_venta$fields$canal_venta=="directa"){
              fields <- append(fields,list('prioridad'="1 - Media"))
            }
            todas <- c("directa","dstrnd","walmartrnd","mercadolibrernd","amazonrnd","coppel","amazonasm","shprndmx")
            if(!orden_venta$fields$canal_venta %in% todas){
              fields <- append(fields,list('prioridad'="1 - Media"))
            }
          }else{
            fields <- append(fields,list('prioridad'="1 - Media"))
          }
          aux <- airtable_createrecord(fields,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
          
          print(paste0("-Las solicitudes de la venta producto : ", venta_producto$fields$id_ventas_producto, " se registro exitosamente"))
          if(!is.null(aux)){
            pedir_piezas(aux)
            link_qr <- aux$fields$barcode_link
            if(is.null(link_qr)){
              link_qr <- paste0("https://barcodeapi.org/api/qr/",aux$fields$id_solicitud,"%7C",aux$id)
            }
            sp <- aux$fields$id_solicitud
            nombre_producto <- paste0(aux$fields$producto_solicitado,".")
            url_etiqueta <- generar_qr_imagen(link_qr ,sp,nombre_producto,aux$id)
            airtable_updatesinglerecord(list('qr_image' = list(list('url'= url_etiqueta))),
                                        'solicitudes_produccion',Sys.getenv("AIRTABLE_CES_BASE"),aux$id)
            if(!last_response()$status_code %in% c(199:299)){
              mensaje <- paste0("No se subio la etiqueta: ",sp,
                                "\nlast_response() %>% resp_body_json():\n",
                                toJSON(last_response() %>% resp_body_json()),
                                "last_request()$body:\n",
                                toJSON(last_request()$body) )
              enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
            }
            airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
          }
          
        }else{
          ov <- ""
          if(is.null(orden_venta$fields$id_origen)){
            ov <- orden_venta$fields$id_ordenes_venta
          }
          fields <- list(
            'tipo'='reserva',
            "producto"=list(producto$id),
            "ventas_producto"=list(venta_producto$id),
            "comentarios"= paste0(ov,orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
            "ubicacion"="navex93",
            "cantidad"=venta_producto$fields$cantidad
          )
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
        
      }else{
        for(parte in producto$fields$paquetes){
          #print(parte)
          aux_parte <- airtable_getrecorddata_byid(parte,"tblugSFaPSzMDlITS",Sys.getenv("AIRTABLE_CES_BASE")) 
          #aux_parte <- airtable_getrecorddata_byid(parte,"tblugSFaPSzMDlITS",Sys.getenv("AIRTABLE_RIR_BASE"))
          if(!is.null(aux_parte$fields$paquete)){
            
            parte_producto <- airtable_getrecorddata_byid(aux_parte$fields$paquete[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
            if(parte_producto$fields$sku==10698){
              parte_producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",10006 ))[[1]]
            }
            if(parte_producto$fields$sku==10697){
              parte_producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",10005 ))[[1]]
            }
            #parte_producto <- airtable_getrecorddata_byid(aux_parte$fields$parte[[1]],"productos",Sys.getenv("AIRTABLE_RIR_BASE"))
            if(parte_producto$fields$cantidad_disponible_navex93 < venta_producto$fields$cantidad){
              if(!is.null(parte_producto$fields$item_produccion)){
                tipo_empaque <- "estándar"
                if(venta_producto$fields$cantidad >= 5 ){
                  tipo_empaque <- "tarima"
                }
                if(!is.null(orden_venta$fields$entrega_qro) || !is.null(orden_venta$fields$entrega_cdmx)){
                  tipo_empaque <- "ligero"
                }
                ov <- ""
                if(is.null(orden_venta$fields$id_origen)){
                  ov <- orden_venta$fields$id_ordenes_venta
                }
                if(str_detect(tolower(producto$fields$id_productos),"juego") && !str_detect(tolower(parte_producto$fields$id_productos),"caja bulto")){
                  fields[[length(fields) + 1]] <- list(
                    "tabla"="solicitudes",
                    "comentarios"= paste0(ov,orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
                    "cantidad"=venta_producto$fields$cantidad,
                    "producto"=list(producto$id),
                    "venta_producto"=list(venta_producto$id),
                    "tipo_empaque"=tipo_empaque,
                    "origen"="pedido",
                    "fecha_solicitud"=Sys.time()-6*3600
                  )
                }else{
                  fields[[length(fields) + 1]] <- list(
                    "tabla"="solicitudes",
                    "comentarios"= paste0(ov,orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
                    "cantidad"=venta_producto$fields$cantidad*aux_parte$fields$cantidad,
                    "producto"=list(parte_producto$id),
                    "venta_producto"=list(venta_producto$id),
                    "tipo_empaque"=tipo_empaque,
                    "origen"="pedido",
                    "fecha_solicitud"=Sys.time()-6*3600
                  )
                }
                if(!is.null(parte_producto$fields$categoria)){
                  if(parte_producto$fields$categoria == "Empaque"){
                    fields[[length(fields)]]$origen <- "empaque CNC"
                  }
                }
                
                if(orden_venta$fields$canal_venta=="mercadolibrernd"){
                  ml_token <- get_active_token()
                  ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
                  if(!is.null(ml_order$shipping$id)){
                    ml_shipping <- get_dir_mlorder(ml_order,ml_token)
                    if(ml_shipping$logistic$mode == "me2"){
                      fecha <- ml_shipping$lead_time$buffering$date
                      if(is.null(fecha)){
                        fields <- append(fields,list('prioridad'="8 - Antes de las 12"))
                        dia_sp <- calcular_fecha_envio(ml_shipping)
                        fields$comentarios <- paste0("AGENCIA: ",format(dia_sp, "%Y-%m-%d")," ",fields$comentarios)
                      }else{
                        fields <- append(fields,list('prioridad'=prioridad_agencia(fecha)))
                        fields$comentarios <- paste0("AGENCIA: ",format(as.POSIXct(fecha, tz = "UTC"), "%Y-%m-%d")," ",fields$comentarios)
                      }
                      #fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="8 - Antes de las 12"))
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
                if(orden_venta$fields$canal_venta=="walmartrnd" || orden_venta$fields$canal_venta=="coppel"){
                  fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="7 - Extrema"))
                }
                if(orden_venta$fields$canal_venta=="dstrnd"){
                  fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="3 - Alta"))
                }
                if(orden_venta$fields$canal_venta=="directa"){
                  fields[[length(fields)]] <- append(fields[[length(fields)]],list('prioridad'="1 - Media"))
                }
                
              }
              else{
                print("No se registro la solicitud de produccion porque una parte de stock no esta disponible")
                print(venta_producto$fields$id_ventas_producto)
                break
              }
            }
            else{
              ov <- ""
              if(is.null(orden_venta$fields$id_origen)){
                ov <- orden_venta$fields$id_ordenes_venta
              }
              fields[[length(fields) + 1]] <- list(
                "tabla"="transacciones",
                'tipo'='reserva',
                "producto"=list(parte_producto$id),
                "ventas_producto"=list(venta_producto$id),
                "comentarios"= paste0(ov,orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
                "cantidad"=venta_producto$fields$cantidad*aux_parte$fields$cantidad,
                "ubicacion"="navex93"
              )
              
              if(is.null(parte_producto$fields$item_produccion) ){
                cantidad_restante <- parte_producto$fields$cantidad_disponible_navex93 - venta_producto$fields$cantidad
                if(cantidad_restante<=6){
                  mensaje <- paste0("Advertencia: El producto ", parte_producto$fields$id_productos, " solo cuenta con ", 
                                    cantidad_restante, " unidades en Navex\nRevisa")
                  #enviar_mensaje_slack(Sys.getenv("SLACK_STOCK_URL"),mensaje)
                  
                  mensajes_enviar[[length(mensajes_enviar) + 1]] <- mensaje
                  cantidad_jiroba <- parte_producto$fields$cantidad_inventario_jiroba
                  if(cantidad_restante+cantidad_jiroba==0){
                    pausar[[length(pausar)+1]] <-  parte_producto
                    mensajes_enviar[[length(mensajes_enviar)]] <- paste0(mensajes_enviar[[length(mensajes_enviar)]],
                                                                         "NO HAY INVENTARIO EN NAVEX Y JIROBA\n*SE PAUSARON LAS PUBLICACIONES EN MERCADO LIBRE Y AMAZON*")
                  }
                  #enviar_email(mensaje,"mauricio@randu.mx")
                  #enviar_email(mensaje,"yatzel@randu.mx")
                }
              }
            }
            
          }
        }
        if(length(fields) == length(producto$fields$paquetes)){
          for(field in fields){
            if(field$tabla == "transacciones"){
              field$tabla <- NULL
              aux <- airtable_createrecord(field,"transacciones_almacen",Sys.getenv("AIRTABLE_CES_BASE"))
            }else{
              field$tabla <- NULL
              aux <- airtable_createrecord(field,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
              
              if(!is.null(aux)){
                pedir_piezas(aux)
                link_qr <- aux$fields$barcode_link
                if(is.null(link_qr)){
                  link_qr <- paste0("https://barcodeapi.org/api/qr/",aux$fields$id_solicitud,"%7C",aux$id)
                }
                sp <- aux$fields$id_solicitud
                nombre_producto <- paste0(aux$fields$producto_solicitado,".")
                url_etiqueta <- generar_qr_imagen(link_qr ,sp,nombre_producto,aux$id)
                airtable_updatesinglerecord(list('qr_image' = list(list('url'= url_etiqueta))),
                                            'solicitudes_produccion',Sys.getenv("AIRTABLE_CES_BASE"),aux$id)
                if(!last_response()$status_code %in% c(199:299)){
                  mensaje <- paste0("No se subio la etiqueta: ",sp,
                                    "\nlast_response() %>% resp_body_json():\n",
                                    toJSON(last_response() %>% resp_body_json()),
                                    "last_request()$body:\n",
                                    toJSON(last_request()$body) )
                  enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
                }
              }
              
            }
            if(is.null(aux)){
              break
            }
            print(paste0("-Se registro exitosamente el proceso necesario para el producto"))
            print(paste0(venta_producto$fields$id_ventas_producto))
          }
          if(!is.null(aux)){
            airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
            if(length(mensajes_enviar)!=0){
              for(mensaje_stock in mensajes_enviar){
                enviar_mensaje_slack(Sys.getenv("SLACK_STOCK_URL"),mensaje_stock)
              }
              
            }
            if(length(pausar) != 0){
              for(producto_pausar in pausar){
                pausar_todas_publicaciones(producto_pausar)
              }
            }
          }else{
            print(last_response() %>% resp_body_json())
            print(paste0("Revisar la venta producto: ",venta_producto$fields$id_ventas_producto))
            print(101)
            return(1)
            
          }
        }
        else{print("No se registro")
          print(venta_producto$fields$id_ventas_producto)}
      }
      
    }else{
      if(venta_producto$fields$cantidad > producto$fields$cantidad_disponible_navex93){
        if(!is.null(producto$fields$item_produccion)){
          tipo_empaque <- "estándar"
          if(venta_producto$fields$cantidad >= 5){
            tipo_empaque <- "tarima"
          }
          if(!is.null(orden_venta$fields$entrega_qro) || !is.null(orden_venta$fields$entrega_cdmx)){
            tipo_empaque <- "ligero"
          }
          ov <- ""
          if(is.null(orden_venta$fields$id_origen)){
            ov <- orden_venta$fields$id_ordenes_venta
          }
          fields <- list(
            "comentarios"= paste0(ov,orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
            "cantidad"=venta_producto$fields$cantidad,
            "producto"=list(producto$id),
            "venta_producto"=list(venta_producto$id),
            "tipo_empaque"=tipo_empaque,
            "origen"="pedido",
            "fecha_solicitud"=Sys.time()-6*3600
          )
          
          if(orden_venta$fields$canal_venta=="mercadolibrernd"){
            ml_token <- get_active_token()
            ml_order <- get_mlorder_byid(orden_venta$fields$id_origen,ml_token)
            if(!is.null(ml_order$shipping$id)){
              ml_shipping <- get_dir_mlorder(ml_order,ml_token)
              if(ml_shipping$logistic$mode == "me2"){
                fecha <- ml_shipping$lead_time$buffering$date
                if(is.null(fecha)){
                  fields <- append(fields,list('prioridad'="8 - Antes de las 12"))
                  dia_sp <- calcular_fecha_envio(ml_shipping)
                  fields$comentarios <- paste0("AGENCIA: ",format(dia_sp, "%Y-%m-%d")," ",fields$comentarios)
                }else{
                  fields <- append(fields,list('prioridad'=prioridad_agencia(fecha)))
                  fields$comentarios <- paste0("AGENCIA: ",format(as.POSIXct(fecha, tz = "UTC"), "%Y-%m-%d")," ",fields$comentarios)
                }
                #fields <- append(fields,list('prioridad'="8 - Antes de las 12"))
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
          if(orden_venta$fields$canal_venta=="walmartrnd" || orden_venta$fields$canal_venta=="coppel"){
            fields <- append(fields,list('prioridad'="7 - Extrema"))
          }
          if(orden_venta$fields$canal_venta=="dstrnd"){
            fields <- append(fields,list('prioridad'="3 - Alta"))
          }
          if(orden_venta$fields$canal_venta=="directa"){
            fields <- append(fields,list('prioridad'="1 - Media"))
          }
          aux <- airtable_createrecord(fields,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
          
          print(paste0("-Las solicitudes de la venta producto : ", venta_producto$fields$id_ventas_producto, " se registro exitosamente"))
          if(!is.null(aux)){
            pedir_piezas(aux)
            link_qr <- aux$fields$barcode_link
            if(is.null(link_qr)){
              link_qr <- paste0("https://barcodeapi.org/api/qr/",aux$fields$id_solicitud,"%7C",aux$id)
            }
            sp <- aux$fields$id_solicitud
            nombre_producto <- paste0(aux$fields$producto_solicitado,".")
            url_etiqueta <- generar_qr_imagen(link_qr ,sp,nombre_producto,aux$id)
            airtable_updatesinglerecord(list('qr_image' = list(list('url'= url_etiqueta))),
                                        'solicitudes_produccion',Sys.getenv("AIRTABLE_CES_BASE"),aux$id)
            if(!last_response()$status_code %in% c(199:299)){
              mensaje <- paste0("No se subio la etiqueta: ",sp,
                                "\nlast_response() %>% resp_body_json():\n",
                                toJSON(last_response() %>% resp_body_json()),
                                "last_request()$body:\n",
                                toJSON(last_request()$body) )
              enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
            }
            airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
          }
          
        }
        else{
          print(paste0("No hay stock de producto: ",producto$fields$id_productos))
          print(venta_producto$fields$id_ventas_producto)
        }
      }else{
        ov <- ""
        if(is.null(orden_venta$fields$id_origen)){
          ov <- orden_venta$fields$id_ordenes_venta
        }
        fields <- list(
          'tipo'='reserva',
          "producto"=list(producto$id),
          "ventas_producto"=list(venta_producto$id),
          "comentarios"= paste0(ov,orden_venta$fields$id_origen," creada mediante R ", orden_venta$field$ml_pack_id),
          "ubicacion"="navex93",
          "cantidad"=venta_producto$fields$cantidad
        )
        
        if(is.null(producto$fields$item_produccion)){
          cantidad_restante <- producto$fields$cantidad_disponible_navex93 - venta_producto$fields$cantidad
          if(cantidad_restante<=6){
            mensaje <- paste0("Advertencia: El producto ", producto$fields$id_productos, "solo cuenta con ", 
                              cantidad_restante, " unidades en Navex\nRevisa")
            cantidad_jiroba <- producto$fields$cantidad_inventario_jiroba
            if(cantidad_restante+cantidad_jiroba==0){
              pausar_todas_publicaciones(producto)
              mensaje <- paste0(mensaje,"\nNO HAY INVENTARIO EN NAVEX Y JIROBA\n*SE PAUSARON LAS PUBLICACIONES EN MERCADO LIBRE Y AMAZON*")
            }
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

crear_pdf <- function(html){
  url <-  "https://api.pdfendpoint.com/v1/convert"
  pdf_key <- Sys.getenv("PDF_API_KEY")
  resp <- request(url) %>% 
    req_method("POST") %>% 
    req_headers("Content-Type"= "application/json",
                "Authorization"= paste0("Bearer ",pdf_key)) %>%
    req_body_json(list("html"=html,
                       "margin_top"= "0.5cm",
                       "margin_bottom"= "0.5cm",
                       "margin_right"= "0.5cm",
                       "margin_left"= "0.5cm",
                       "no_backgrounds"= T,
                       "orientation"= "vertical",
                       "no_blank_pages"=T)) %>% 
    req_error(is_error = function(resp) FALSE) %>%
    req_perform() %>% 
    resp_body_json()
  if(last_response()$status_code %in% c(199:299)){
    return(resp$data$url)
  }
}

buscar_primera_por_valor <- function(lst, valor_buscado = "SKU") {
  resultado <- NULL
  
  recorrer <- function(x) {
    if (!is.null(resultado)) return()  # ya encontramos
    if (is.list(x)) {
      if (any(map_lgl(x, ~ identical(.x, valor_buscado)))) {
        resultado <<- x
      } else {
        walk(x, recorrer)
      }
    }
  }
  
  recorrer(lst)
  resultado
}

safe_abs_numeric <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  if (is.na(num)) num <- 0
  abs(num)
}

imprimir_etiqueta_pernos <- function(){
  ezeep_at <- ezeep_getactivetoken()
  while(T){
    respuesta <- readline("Escanea la etiqueta que deseas imprimir o escribe terminar: ")
    if(str_detect(respuesta,"\\|") && !str_detect(respuesta,"SP")){
      texto_escaneado <- strsplit(respuesta,"\\|") 
      id_producto <- texto_escaneado[[1]][1]
      record_producto <- detectar_bloq_mayus(texto_escaneado[[1]][2],"rec")
      producto <- airtable_getrecorddata_byid(record_producto,"productos",Sys.getenv("AIRTABLE_CES_BASE"))
      if(length(producto)!=0){
        if(length(producto$fields$qr_image)!=0){
          ezeep_resp <- ezeep_printbyurl(producto$fields$qr_image[[1]]$url,ezeep_at,"impresora_azul",copies=50)
          if(!is.null(ezeep_resp$jobid)){
            print(paste0('Se solicitó correctamente la impresión del producto ', producto$fields$id_productos))
          }
        }
      }
    }else{
      break
    }
  }
}

invertir_may_min <- function(x) {
  # Cambiar mayúsculas a minúsculas y viceversa
  sapply(strsplit(x, NULL), function(chars) {
    paste(sapply(chars, function(char) {
      if (char == tolower(char)) {
        return(toupper(char)) 
      } else {
        return(tolower(char))  
      }
    }), collapse = "")
  })
}

detectar_bloq_mayus <- function(char,patron, inicio=1){
  rec <- substr(char,inicio,nchar(patron)+inicio-1)
  if(rec != patron){
    char<- invertir_may_min(char)
  }
  return(char)
}

first_letter <- function(char){
  first_char <- substr(char, 1, 1)
  is_upper <- grepl("^[A-Z]$", first_char)
  if(!is_upper){
    char <- invertir_may_min(char)
  }
  return(char)
}

actualizar_o_sumar <- function(tabla, nuevo_sku, nueva_cantidad,nombre,record_id='') {
  if (nuevo_sku %in% tabla$sku) {
    if(es_numero(tabla$cantidad[tabla$sku == nuevo_sku]) && es_numero(nueva_cantidad)){
      tabla$cantidad[tabla$sku == nuevo_sku] <- as.numeric(tabla$cantidad[tabla$sku == nuevo_sku]) + as.numeric(nueva_cantidad)
    }
  } else {
    
    nueva_fila <- data.frame(sku = nuevo_sku, cantidad = as.numeric(nueva_cantidad),nombre=nombre,recordid=record_id, stringsAsFactors = FALSE)
    tabla <- rbind(tabla, nueva_fila)
  }
  return(tabla)
}

obtener_tabla_resumen <- function(){
  archivo <- "~/resumen_guias.RDS"
  if (file.exists(archivo)) {
    tabla_resumen <- readRDS(archivo)
    if(tabla_resumen$fecha!=Sys.Date()){
      tabla_resumen <- list("fecha"=Sys.Date(),"tabla_resumen"=data.frame(
        sku = character(),
        nombre=character(),
        cantidad = numeric(),
        recordid = character(),
        stringsAsFactors = FALSE
      ))
    }
  } else {
    tabla_resumen <- list("fecha"=Sys.Date(),"tabla_resumen"=data.frame(
      sku = character(),
      nombre=character(),
      cantidad = numeric(),
      recordid = character(),
      stringsAsFactors = FALSE
    ))
    saveRDS(tabla_resumen, archivo)
  }
  return(tabla_resumen)
}

resumen_productos <- function(tabla,lista_paquete){
  for(paquete in lista_paquete){
    sku <- as.numeric(sub("^(\\d+).*", "\\1",paquete$nombre))
    if(!is.null(sku)){
      if(!is.na(sku)){
        tabla <- actualizar_o_sumar(tabla,sku,paquete$cantidad,paquete$nombre)
      }
    }
  }
  return(tabla)
}

resumen_guias <- function(){
  archivo <- "~/resumen_guias.RDS"
  if (file.exists(archivo)) {
    tabla_resumen <- readRDS(archivo)
  }else{
    print("NO SE PUDO CREAR EL ARCHIVO")
  }
  tabla_list <- unname(split(tabla_resumen$tabla_resumen, seq(nrow(tabla_resumen$tabla_resumen))))
  
  # Creamos la lista para el template
  data_template <- list(
    fecha = tabla_resumen$fecha,
    tiene_filas = length(tabla_list) > 0,
    filas = tabla_list
  )
  
  # Template Mustache (whisker)
  template <- "
<!DOCTYPE html>
<html>
<head>
  <meta charset='UTF-8'>
  <title>Resumen de tabla_resumen</title>
  <style>
    body { font-family: Arial, sans-serif; padding: 20px; }
    table { border-collapse: collapse; width: 100%; margin-top: 20px; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f2f2f2; }
  </style>
</head>
<body>
  <h1>Resumen de tabla_resumen</h1>
  <p><strong>Fecha:</strong> {{fecha}}</p>
  <h2>Tabla</h2>
  <table>
    <tr>
      <th>sku</th>
      <th>nombre</th>
      <th>cantidad</th>
      <th>recordid</th>
    </tr>
    {{#tiene_filas}}
      {{#filas}}
        <tr>
          <td>{{sku}}</td>
          <td>{{nombre}}</td>
          <td>{{cantidad}}</td>
          <td>{{recordid}}</td>
        </tr>
      {{/filas}}
    {{/tiene_filas}}
    {{^tiene_filas}}
      <tr>
        <td colspan='4' style='text-align:center; color:gray;'>Tabla vacía</td>
      </tr>
    {{/tiene_filas}}
  </table>
</body>
</html>
"
  
  # Renderizar el HTML con whisker
  html_rendered <- whisker.render(template, data_template)
  
  # Guardar en archivo
  writeLines(html_rendered, "resumen_tabla.html")
  message("HTML generado correctamente: resumen_tabla_html")
  browseURL(normalizePath("resumen_tabla.html"))
}

resumen_guias_shiny <- function() {
  archivo <- "~/resumen_guias.RDS"
  if (!file.exists(archivo)) {
    print("NO SE PUDO CREAR EL ARCHIVO")
    return(NULL)
  }
  
  tabla_resumen <- readRDS(archivo)
  tabla_list <- unname(split(tabla_resumen$tabla_resumen, seq(nrow(tabla_resumen$tabla_resumen))))
  
  data_template <- list(
    fecha = tabla_resumen$fecha,
    tiene_filas = length(tabla_list) > 0,
    filas = tabla_list
  )
  
  template <- "
<!DOCTYPE html>
<html>
<head>
  <meta charset='UTF-8'>
  <title>Resumen de </title>
  <style>
    body { font-family: Arial, sans-serif; padding: 20px; }
    table { border-collapse: collapse; width: 100%; margin-top: 20px; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f2f2f2; }
  </style>
</head>
<body>
  <h1>Resumen de tabla_resumen</h1>
  <p><strong>Fecha:</strong> {{fecha}}</p>
  <h2>Tabla</h2>
  <table>
    <tr>
      <th>sku</th>
      <th>nombre</th>
      <th>cantidad</th>
      <th>recordid</th>
    </tr>
    {{#tiene_filas}}
      {{#filas}}
        <tr>
          <td>{{sku}}</td>
          <td>{{nombre}}</td>
          <td>{{cantidad}}</td>
          <td>{{recordid}}</td>
        </tr>
      {{/filas}}
    {{/tiene_filas}}
    {{^tiene_filas}}
      <tr>
        <td colspan='4' style='text-align:center; color:gray;'>Tabla vacía</td>
      </tr>
    {{/tiene_filas}}
  </table>
</body>
</html>
"
  
  html_rendered <- whisker.render(template, data_template)
  output_file <- "~/resumen_tabla.html"
  #dir.create("www", showWarnings = FALSE)
  writeLines(html_rendered, output_file)
  message("✅ HTML generado correctamente: ", normalizePath(output_file))
  
  return(output_file)
}

pausar_todas_publicaciones <- function(producto){
  tryCatch(expr={
    if(length(producto$fields$pertenece_paquetes)!=0){
      producto_partes <- airtable_getrecordslist("tblugSFaPSzMDlITS",Sys.getenv("AIRTABLE_CES_BASE"),paste0("FIND('",producto$fields$id_productos,"',{paquete})"))
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
      }
      
      
    }

  },error=function(e){
    mensaje_error <- paste0("Ocurrio un error general al intentar pausar las publicaciones de un producto: ",e)
    if(length(producto)!=0){
      mensaje_error <- paste0(producto$fields$id_productos,"\n",mensaje_error)
    }
    enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje_error)
  })
 
}

pedir_piezas <- function(solicitud){
  producto <- airtable_getrecorddata_byid(solicitud$fields$producto[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
  
  if(length(producto$fields$piezas_producto)!=0){
    cantidad_piezas <- length(producto$fields$piezas_producto)
    airtable_updatesinglerecord(list("cantidad_piezas"=cantidad_piezas),"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"),solicitud$id)
    i <- 1
    for(pieza in producto$fields$piezas_producto){
      aux_pieza <- airtable_getrecorddata_byid(pieza,"piezas_producto",Sys.getenv("AIRTABLE_CES_BASE"))
      pz_producto <- airtable_getrecorddata_byid(aux_pieza$fields$pieza[[1]],"productos",Sys.getenv("AIRTABLE_CES_BASE"))
      fields <- list("origen"="pieza",
                     "comentarios"=paste0("Pieza de ",solicitud$fields$id_solicitud," ",solicitud$fields$comentarios),
                     "cantidad"=solicitud$fields$cantidad*aux_pieza$fields$cantidad,
                     "prioridad"=solicitud$fields$prioridad,
                     "producto"=list(pz_producto$id),
                     "indice_pieza"=i,
                     "indice_padre"=solicitud$fields$autonumber_upd+1000,
                     "cantidad_piezas"=cantidad_piezas,
                     "sp_padre"=list(solicitud$id),
                     "fecha_solicitud"=Sys.time()-6*3600
                     )
      pz <- airtable_createrecord(fields,"solicitudes_produccion",Sys.getenv("AIRTABLE_CES_BASE"))
      
      if(!is.null(pz)){
        i <- i+1
        link_qr <- pz$fields$barcode_link
        if(is.null(link_qr)){
          link_qr <- paste0("https://barcodeapi.org/api/qr/",pz$fields$id_solicitud,"%7C",pz$id)
        }
        sp <- paste0(pz$fields$id_solicitud)
        nombre_producto <- paste0(pz$fields$producto_solicitado,".")
        url_etiqueta <- generar_qr_imagen(link_qr ,sp,nombre_producto,pz$id,pz_producto$fields$nombre_instructivo)
        airtable_updatesinglerecord(list('qr_image' = list(list('url'= url_etiqueta))),
                                    'solicitudes_produccion',Sys.getenv("AIRTABLE_CES_BASE"),pz$id)
        if(!last_response()$status_code %in% c(199:299)){
          mensaje <- paste0("No se subio la etiqueta: ",sp,
                            "\nlast_response() %>% resp_body_json():\n",
                            toJSON(last_response() %>% resp_body_json()),
                            "last_request()$body:\n",
                            toJSON(last_request()$body) )
          enviar_mensaje_slack(Sys.getenv("SLACK_ERROR_URL"),mensaje)
        }
        #airtable_updatesinglerecord(list("vp_revisada"=TRUE),"ventas_producto",Sys.getenv("AIRTABLE_CES_BASE"),venta_producto$id)
      }
    }
  }
}

prioridad_agencia <- function(fecha){
  fecha_posix <- as.POSIXct(
    fecha,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  
  fecha_posix <- as.POSIXct(
    paste0(format(fecha_posix, "%Y-%m-%d"),
           "14",format(fecha_posix, ":%M:%S")),
    tz = "UTC"
  )
  
  ahora <- as.POSIXct(Sys.time(), tz = "UTC")
  
  dif_horas <- as.numeric(difftime(fecha_posix, ahora,units = "hours"))
  
  prioridad <- ifelse(
    dif_horas <= 24, "8 - Antes de las 12",
    ifelse(dif_horas <= 36, "6 - Para hoy", "3 - Alta")
  )
  return(prioridad)
}
