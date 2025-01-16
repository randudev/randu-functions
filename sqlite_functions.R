library(DBI)
library(RSQLite)

abrir_conexion_SQLite <- function(db_name) {
  con <- dbConnect(RSQLite::SQLite(), db_name)
  return(con)
}

insertar_fila <- function(con, tabla, datos) {
  
  query <- paste0("INSERT INTO ", tabla, " (time, rspns, url,status,header,request,origenes) VALUES (?, ?, ?, ?, ?, ?, ?)")
  valores <- unname(as.list(datos))
  dbExecute(con, query, params = valores)
}

insertar_fila_errores <- function(con, datos) {
  # Crear una consulta SQL para insertar datos
  query <- paste0("INSERT INTO ", "errores", " (time, rspns, url, status, header, request, funcion, origenes) VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
  valores <- unname(as.list(datos))  # Esto elimina los nombres de las columnas
  # Ejecutar la consulta, pasando los valores a insertar
  dbExecute(con, query, params = valores)
}

desconectar <- function(con){
  dbDisconnect(con)
}
crear_tabla1 <- function(con) {
  query <- "
    CREATE TABLE IF NOT EXISTS airtable_getrecordslist (
      time TEXT,
      rspns TEXT,
      url TEXT,
      status INTEGER,
      header TEXT,
      request TEXT,
      origenes TEXT
    );
  "
  
  dbExecute(con, query)
}
crear_tabla2 <- function(con) {
  query <- "
    CREATE TABLE IF NOT EXISTS airtable_getrecorddata_byid (
      time TEXT,
      rspns TEXT,
      url TEXT,
      status INTEGER,
      header TEXT,
      request TEXT,
      origenes TEXT
    );
  "
  
  dbExecute(con, query)
}
crear_tabla3 <- function(con) {
  query <- "
    CREATE TABLE IF NOT EXISTS airtable_updatesinglerecord (
      time TEXT,
      rspns TEXT,
      url TEXT,
      status INTEGER,
      header TEXT,
      request TEXT,
      origenes TEXT
    );
  "
  
  dbExecute(con, query)
}

crear_tabla4 <- function(con) {
  query <- "
    CREATE TABLE IF NOT EXISTS airtable_createrecord (
      time TEXT,
      rspns TEXT,
      url TEXT,
      status INTEGER,
      header TEXT,
      request TEXT,
      origenes TEXT
    );
  "
  
  dbExecute(con, query)
}

crear_tabla5 <- function(con) {
  query <- "
    CREATE TABLE IF NOT EXISTS errores(
      time TEXT,
      rspns TEXT,
      url TEXT,
      status INTEGER,
      header TEXT,
      request TEXT,
      funcion TEXT,
      origenes TEXT
    );
  "
  
  dbExecute(con, query)
}

close_all_connections <- function(env = .GlobalEnv) {
  objects <- ls(env)  # Listar todos los objetos en el entorno
  for (obj in objects) {
    conn <- get(obj, envir = env)
    if (inherits(conn, "DBIConnection") && dbIsValid(conn)) {
      dbDisconnect(conn)
      message(paste("Conexión cerrada:", obj))
    }
  }
}
print_all_connections <- function(env = .GlobalEnv) {
  objects <- ls(env)  # Listar todos los objetos en el entorno
  for (obj in objects) {
    conn <- get(obj, envir = env)
    if (inherits(conn, "DBIConnection") && dbIsValid(conn)) {
      print(conn)
      message(paste("Conexión cerrada:", obj))
    }
  }
}
#tibble(time=Sys.time(),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
#       status=last_response()$status_code,header =toJSON(last_request()$headers),request=toJSON(last_request()$body$data),
##       funcion="airtable_getrecorddata_byid",origenes=origen)
##ibble(time=Sys.time(),rspns=toJSON(last_response() %>% resp_body_json()),url=last_response()$url,
#      status=last_response()$status_code,header =toJSON(last_request()$headers),
#      request=toJSON(last_request()$body$data),funcion="airtable_getrecordslist",origenes=origen)