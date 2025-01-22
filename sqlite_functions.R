library(DBI)
library(RSQLite)

abrir_conexion_SQLite <- function(db_name) {
  con <- dbConnect(RSQLite::SQLite(), db_name)
  return(con)
}

insertar_fila <- function(con, tabla, datos) {
  
  query <- paste0("INSERT INTO ", tabla, " (time, rspns, url,status,header,request, funcion,origenes) VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
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
insertar_orden_venta <- function(con,datos){
  query <- paste0("INSERT INTO ", "ordenes_venta", " (orden_venta) VALUES (?)")
  valores <- unname(as.list(datos))  # Esto elimina los nombres de las columnas
  # Ejecutar la consulta, pasando los valores a insertar
  dbExecute(con, query, params = valores)
}

desconectar <- function(con){
  dbDisconnect(con)
}
crear_tabla <- function(con) {
  query <- "
    CREATE TABLE IF NOT EXISTS api_logs (
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
#------Esta no-------
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

crear_tabla_orden <- function(con) {
  query <- "
    CREATE TABLE IF NOT EXISTS ordenes_venta (
      orden_venta TEXT PRIMARY KEY
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
#----------
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

leer_tabla <- function(con, tabla){
  df <- dbReadTable(con, tabla)
}
