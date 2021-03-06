# The MIT License (MIT)
# Copyright (c) 2016 Mario Hernández Morales
#
library(RODBC) # Para realizar las conexiones con PostgreSQL

#Municipio 
mun.php <- function() {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, "SELECT * FROM municipio;")
  odbcClose(dbconn)
  colnames(d)[1] <- "cve"
  d
}

#Localidad
loc.php <- function(m = "015") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT cve_loc AS cve, nom_loc AS nombre, lat, lon FROM cat_localidad WHERE cve_mun = '", m,"';") )
  odbcClose(dbconn)
  d
}

loc2.php <- function(m = "015") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT cve_loc AS cve, nom_loc AS nombre, lat, lon FROM cat_localidad WHERE dnom AND cve_mun = '", m,"';") )
  odbcClose(dbconn)
  d
}

loc3.php <- function(m = "015") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT cve_loc AS cve, nom_loc AS nombre, lat, lon FROM cat_localidad WHERE cnom AND cve_mun = '", m,"';") )
  odbcClose(dbconn)
  d
}

#Asentamiento
#Incorpora el tipo de asentamiento
col.php <- function(m = "015", l = "0001") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT cve_asen, nombre || ' ' || nom_asen as nom_asen, nom_asen as nom_asen0 FROM colonia as a, cat_tipo_asen as c WHERE ",
    "(cve_mun = '", m, "') AND (",
    "(cve_mun_u = '", m, "' AND cve_loc_u = '", l, "')",
    " OR (cve_mun_r = '", m, "' AND cve_loc_r = '", l, "' AND distancia < 1000)) AND a.cve_tipo_asen = c.cve_tipo_asen;"))
  odbcClose(dbconn)
  d0 <- d[c(1, 3)]
  colnames(d0)[2] <- "nom_asen"
  rbind(d[c(1, 2)], d0)
}

#Vialidad
#Incorpora el tipo de vialdad
cal.php <- function(m = "015", l = "0001") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT cve_via, descripcion || ' ' || nom_via as nom_via, nom_via as nom_via0 FROM vialidad as v, cat_vialidad as c WHERE cve_mun = '", m, "' AND cve_loc = '", l, "' AND es_llave AND v.cve_tipo_vial = c.cve_tipo_vial ORDER BY nom_via;"))
  odbcClose(dbconn)
  d0 <- d[c(1, 3)]
  colnames(d0)[2] <- "nom_via"
  rbind(d[c(1, 2)], d0)
}

cal2.php <- function(m = "015", l = "0001") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT cve_via, descripcion || ' ' || nom_via as nom_via, nom_via as nom_via0, lat, lon FROM vialidad as v, cat_vialidad as c WHERE cve_mun = '", m, "' AND cve_loc = '", l, "' AND es_llave AND v.cve_tipo_vial = c.cve_tipo_vial ORDER BY nom_via;"))
  odbcClose(dbconn)
  d
}

#Número exterior
#Incorpora calles homónimas
#Se corrige, intercambiando las claves por la clave única.
num.php <- function(c = "27000101583") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("(SELECT lat, lon, num FROM geocode1 WHERE cve_via IN (SELECT via_unica FROM vialidad WHERE cve_via = ", c, "));"))
  odbcClose(dbconn)
  d
}

#Entrecalles
#Combina nombres similares y con el tipo de vialidad
ecal.php <- function(c = "015000100024") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT v.via_unica AS cve, descripcion || ' ' || nom_via as nom_via, v.nom_via as nom_via0 FROM vialidad as v, cat_vialidad as c WHERE v.cve_via IN ",
    "(SELECT cve_via  FROM interseccion WHERE (cve_via0 IN (SELECT cve_via FROM vialidad WHERE via_unica = ", c, ")) UNION ",
    " SELECT cve_via0 FROM interseccion WHERE (cve_via  IN (SELECT cve_via FROM vialidad WHERE via_unica = ", c, "))) ",
    "AND v.cve_tipo_vial = c.cve_tipo_vial GROUP BY cve, descripcion, nom_via ORDER BY cve ASC;"))
  odbcClose(dbconn)
  d0 <- d[c(1, 3)]
  colnames(d0)[2] <- "nom_via"
  rbind(d[c(1, 2)], d0)
}

#fix(d)

### Localiza según información conocida

# Proporciona la localidad (gloc.php)
# Proporciona el municipio (anterior con d_loc = "0001")
# Útil solo para las coordenadas del municipio
gloc.php <- function(m = "015", l = "0001") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT lat, lon FROM cat_localidad WHERE cve_mun = '", m, "' AND cve_loc = '", l, "';"))
  odbcClose(dbconn)
  d
}

# Proporciona el nombre de la localidad con la clave de Inegi
nloc.php <- function(m = "015", l = "0001") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0("SELECT nom_loc AS nombre FROM cat_localidad WHERE cve_mun = '", m, "' AND cve_loc = '", l, "';"))
  odbcClose(dbconn)
  if(length(d$nombre) == 0)
    return("")
  return(as.character(d$nombre))
}

# Proporciona las coordenadas del centroide interno de la colonia
# incluye el radio máximo al contorno.
gcol.php <- function(c = "0780") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0(
    "SELECT lat, lon, radio FROM colonia WHERE cve_asen = '", c, "' ;"))
  odbcClose(dbconn)
  d
}

# Proporciona la calle
gcal.php <- function(c = 15006700440) {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0(
    "SELECT lat, lon FROM vialidad WHERE cve_via = ", c, " ;"))
  odbcClose(dbconn)
  d
}

# Direcciones almacenadas
gn.php <- function(c = "27000101583", n = "1") {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0(
    "(SELECT lat, lon FROM geocode1 WHERE cve_via IN ",
    "(SELECT via_unica FROM vialidad WHERE cve_via = ", c, ") AND num LIKE '", n, "') UNION ",
    "(SELECT lat, lon FROM geocode0 WHERE cve_via IN ",
    "(SELECT via_unica FROM vialidad WHERE cve_via = ", c, ") AND num LIKE '", n, "');"))
  odbcClose(dbconn)
  d
}

# Proporciona una entrecalle
gecal.php <- function(c = 15006700440, e = 15006700441) {
  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0(
    "SELECT lat, lon FROM interseccion WHERE ",
    "((cve_via  IN (SELECT cve_via FROM vialidad WHERE via_unica = '",c,"')) ",
    "AND (cve_via0 IN (SELECT cve_via FROM vialidad WHERE via_unica = '",e,"'))) OR ",
    "((cve_via  IN (SELECT cve_via FROM vialidad WHERE via_unica = '",e,"')) ",
    "AND (cve_via0 IN (SELECT cve_via FROM vialidad WHERE via_unica = '",c,"'))) ",
    "ORDER BY lat DESC LIMIT 1;"))
  odbcClose(dbconn)
  d
}

#-------------------------------------------------------------------
# Zonas metropolitanas
#Número de registro en el Sistema Urbano Nacional 2010
#   Clave del municipio
#       Nombre del municipio
#             zona metropolitana
#14	020	León	León
#14	037	Silao	León
#15	025	Purísima del Rincón	San Francisco del Rincón
#15	031	San Francisco del Rincón	San Francisco del Rincón
#16	021	Moroleón	Moroleón-Uriangato
#16	041	Uriangato	Moroleón-Uriangato

#Número de registro en el Sistema Urbano Nacional 2010
# 	Clave de la localidad
#             Nombre de la localidad
#                               Conurbación
#67	110050001	Apaseo el Grande	Apaseo el Grande
#67	110050051	San Pedro Tenango el Nuevo	Apaseo el Grande
#68	110150001	Guanajuato	Guanajuato
#68	110150067	Marfil	Guanajuato
#68	110150126	Yerbabuena	Guanajuato
#69	110170001	Irapuato	Irapuato
#69	110170059	Arandas	Irapuato
#69	110170359	Villas de Irapuato	Irapuato
#70	110280001	Salvatierra	Salvatierra
#70	110280064	Urireo	Salvatierra
#71	110330001	San Luis de la Paz	San Luis de la Paz
#71	110330110	Misión de Chichimecas	San Luis de la Paz

#Número de registro en el Sistema Urbano Nacional 2010
#     Clave de la localidad
#               Nombre de la ciudad
#193	110010001	Abasolo
#194	110020001	Acámbaro
#195	110030001	San Miguel de Allende
#196	110040001	Apaseo El Alto
#197	110110001	Cortazar
#198	110140001	Dolores Hidalgo Cuna de la Independencia Nacional
#199	110180001	Jaral del Progreso
#200	110260001	Romita
#201	110270001	Salamanca
#202	110300001	San Felipe
#203	110320001	San José Iturbide
#204	110350001	Juventino Rosas
#205	110420001	Valle de Santiago
#206	110460001	Yuriria
conurbacion <- function(r_loc.cve_loc = "0150001") {
  m = substr(r_loc.cve_loc, 1, 3)
  l = substr(r_loc.cve_loc, 4, 7)

  dbconn <- odbcConnect("local")
  d <- sqlQuery(dbconn, paste0(
    "SELECT cat_localidad.cve_mun AS m, cat_localidad.cve_loc AS l, cat_municipio.nom_mun, cat_localidad.nom_loc, cat_localidad.lat, cat_localidad.lon ",
    "FROM cat_localidad, cat_municipio WHERE cat_localidad.cve_mun = cat_municipio.cve_mun AND sun ",
    "IN(SELECT sun FROM cat_localidad WHERE cat_localidad.cve_mun = '", m,"' AND cat_localidad.cve_loc = '", l,"') AND ",
    "NOT(cat_localidad.cve_mun = '", m,"' AND cat_localidad.cve_loc = '", l,"') ",
    "ORDER BY nom_loc;"))
  odbcClose(dbconn)
  if(length(d$l) > 0) {
    d$m <- mapply(str_pad, d$m, 3, pad = "0")
    d$l <- mapply(str_pad, d$l, 4, pad = "0")
    d$l <- paste0(d$m, d$l)
    colnames(d)[2] <- "cve"
    colnames(d)[3] <- "nombre"
    d$niv <- 4
    d$BM <- 2e-7
    d$nombre <- paste(d$nombre, d$nom_loc, sep = ", ") 
    d[c(7, 8, 2, 3, 5, 6)]
  } else{
    NULL
  }
}

# Rutinas para la administración de la  bitácora de procesamiento por lotes
ldom.php <- function(file_in, sheet, file_out, size) {
  dbconn <- odbcConnect("local")
  id <- sqlQuery(dbconn, paste0("INSERT INTO ldom (id, file_in, sheet, file_out, time_start, size) VALUES (DEFAULT,'",file_in, "',", sheet,",'", file_out,"', now() AT TIME ZONE 'America/Mexico_City',",size,") RETURNING id;"))
  odbcClose(dbconn)
  id
}

# Actualiza el tamaño del lote
size.ldom.php <- function(id, size) {
  dbconn <- odbcConnect("local")
  sqlQuery(dbconn, paste0("UPDATE ldom SET size = ", size, " WHERE id = ",id,";"))
  odbcClose(dbconn)
}

# Da de alta en identificador del proceso
pid.ldom.php <- function(id, pid) {
  dbconn <- odbcConnect("local")
  sqlQuery(dbconn, paste0("UPDATE ldom SET pid = ", pid, " WHERE id = ",id,";"))
  odbcClose(dbconn)
}

avance.ldom.php <- function(id) {
  dbconn <- odbcConnect("local")
  sqlQuery(dbconn, paste0("UPDATE ldom SET biased = biased + 1 WHERE id = ",id,";"))
  odbcClose(dbconn)
}

fin.ldom.php <- function(id) {
  dbconn <- odbcConnect("local")
  sqlQuery(dbconn, paste0("UPDATE ldom SET time_end = now() AT TIME ZONE 'America/Mexico_City', pid = NULL WHERE id = ",id,";"))
  odbcClose(dbconn)
}

lee.ldom.php <- function() {
  dbconn <- odbcConnect("local")
  id <- sqlQuery(dbconn, "SELECT * FROM ldom ORDER BY id DESC")
  odbcClose(dbconn)
  id
}

# Elimina un registro
remove.ldom.php <- function(id) {
  dbconn <- odbcConnect("local")
  sqlQuery(dbconn, paste0("DELETE FROM ldom WHERE id = ",id,";"))
  odbcClose(dbconn)
}

# Reinicia la numeración del registro
restart.ldom.php <- function(id) {
  dbconn <- odbcConnect("local")
  sqlQuery(dbconn, "ALTER SEQUENCE public.ldom_id_seq RESTART WITH 1;")
  odbcClose(dbconn)
}

# NOTAS
# Rodbc se instala con
#> sudo apt-get install r-cran-rodbc odbc-postgresql

# Configurar conexión con:
#> more /etc/odbc.ini
#[ODBC Data Sources]
#local = Bases de datos local
#
#[local]
#Driver = /usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so
#Servername = localhost
#Port = 5432
#Database = *
#Username = **
#Password = ***
#Protocol = 9.3.10
#ReadOnly = 0
#
#[ODBC]
#InstallDir = /usr/lib/x86_64-linux-gnu/

# Iniciar la aplicación con:
#> odbcinst -q -d
