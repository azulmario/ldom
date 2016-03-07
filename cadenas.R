library(stringr)

# Limpieza de textos
limpieza <- function(tw = "") {
  tw <- toupper(tw)
  tw <- gsub("[[:punct:]]", " ", tw) # Símbolos de puntuación
  #  tw <- gsub("[[:digit:]]", " ", tw) # Los números son importantes
  tw <- gsub("[ |\t]{2,}", " ", tw)  # Tabuladores
  tw <- gsub("'", " ", tw)  # Comillas
  tw <- gsub("Á", "A", tw)
  tw <- gsub("É", "E", tw)
  tw <- gsub("Í", "I", tw)
  tw <- gsub("Ó", "O", tw)
  tw <- gsub("Ú", "U", tw)
  tw <- gsub("Ü", "U", tw)
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final
  tw
}

# Limpieza de números
limpieza0 <- function(tw = "") {
  tw <- gsub("[[:alpha:]]", "", tw) # Quita letras
  tw <- gsub("[\\.]", "ńù", tw)
  tw <- gsub("[[:punct:]]", "", tw) # Quita puntuación
  tw <- gsub("ńù", ".", tw)
  tw <- gsub("[ |\t]{2,}", "", tw)  # Tabuladores
  tw <- gsub("[[:space:]]", "", tw) # Quita espacios
  tw <- gsub("[\\.]+", ".", tw) # Quita puntos múltiples
  as.numeric(tw)
}

# Función de ejemplo
#URL_parts <- function(x) {
#  m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
#  parts <- do.call(rbind, lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
#  colnames(parts) <- c("protocol","host","port","path")
#  parts
#}

# Excepciones al separar campo vld con número exterior
Cachelist_SN <- c(
  'A LA TINAJA DE LOS RODRIGUEZ 4 K',
  'A LAS CAÑAS 2.8 K',
  'A LOS RODRIGUEZ 10 K',
  'ACCESO LATERAL 1',
  'ACCESO LATERAL 2',
  'AGULIA 2',
  'ALAMEDA 1',
  'ALAMEDA 2',
  'ALTEÑA 1',
  'ALTEÑA 2',
  'ALVARO OBREGON 1',
  'ALVARO OBREGON 2',
  'ANDEN 1',
  'ANDEN 2',
  'ANDEN 3',
  'ANDEN 4',
  'ANDEN 5',
  'ANDEN 6',
  'ANDEN 7',
  'ANDEN 8',
  'ANDEN 9',
  'ARROYO 1',
  'ARROYO 2',
  'ARROYO 3',
  'ARTICULO 115',
  'ARTICULO 123',
  'ARTICULO 136',
  'ARTICULO 27',
  'ARTICULO 3',
  'BANDERA NACIONAL 1',
  'BANDERA NACIONAL 2',
  'BANDERA NACIONAL 3',
  'BENEMERITO DE LAS 2 AMERICAS',
  'BENITO JUAREZ 4',
  'BUENOS AIRES 1',
  'BUENOS AIRES 2',
  'BUGAMBILIA 2',
  'CALICHE NUMERO 1',
  'CALICHE NUMERO 2',
  'CALIFORNIA 1',
  'CALIFORNIA 2',
  'CALIFORNIA 3',
  'CALLEJUELA 1 DE MAYO',
  'CARRANZA 1',
  'CELAYA 2000',
  'CEYLAN 204',
  'CEYLAN 210',
  'CEYLAN 216',
  'CEYLAN 222',
  'CEYLAN 304',
  'CEYLAN 310',
  'CEYLAN 316',
  'CINCO 1',
  'CINCO 2',
  'CIRCUITO DE LOS PORTALES 1',
  'CIRCUITO DE LOS PORTALES 2',
  'CIUDAD 16 DE SEPTIEMBRE',
  'CLOSTER 1',
  'CLOSTER 2',
  'CLOSTER 3',
  'CLOSTER 4',
  'CONSTITUCION 1810',
  'CONSTITUCION 1814',
  'CONSTITUCION 1917',
  'CONSTITUCION DE 1824',
  'CONSTITUCION DE 1857',
  'CONSTITUCION DE 1917',
  'CONSTITUCION DE 1925 ORIENTE',
  'CONSTITUYENTES DEL 17',
  'COSTA AZUL 303',
  'COSTA AZUL 304',
  'COSTA AZUL 310',
  'COSTA AZUL 311',
  'COSTA AZUL 316',
  'CRUSILLO 18',
  'CUARTA 10 DE MAYO',
  'CUARTA 11 DE JUNIO',
  'CUARTA 8 DE MARZO',
  'CUATRO 1',
  'DE 16 DE SEPTIEMBRE',
  'DE 20 DE NOVIEMBRE',
  'DE ENMEDIO 2',
  'DE ESCUADRON 201',
  'DE GUIJAS NUMERO 3',
  'DE HIDALGO NUMERO 1',
  'DE HIDALGO NUMERO 2',
  'DE LA ALBERCA (7 LUMINARIAS)',
  'DE LA MERCED 1',
  'DE LA MERCED 2',
  'DE LA PALMA 1',
  'DE LOS TULES 1',
  'DE LOS TULES 2',
  'DE LOS TULES 3',
  'DE TROJES 1',
  'DE TROJES 2',
  'DE TROJES 3',
  'DECRETO 204',
  'DEL 30 DE AGOSTO',
  'DEL POZO NUMERO 7',
  'DREN 10 DE LOS PEPINOS',
  'EDUCACION NUM. 2',
  'EJE ORIENTE PONIENTE 1',
  'EJE ORIENTE PONIENTE 3',
  'ESCUADRON 201',
  'FEDERAL 90 IRAPUATO-LA PIEDAD',
  'FEDERAL NUMERO 45',
  'FELIPE ANGELES 1',
  'FELIPE ANGELES 2',
  'FLORES MAGON 1',
  'FLORES MAGON 2',
  'FLORES MAGON 3',
  'FRANCISCO I. MADERO 1',
  'FRANCISCO I. MADERO 2',
  'FRANCISCO I. MADERO 3',
  'FRANCISCO VILLA 1',
  'FRANCISCO VILLA 2',
  'FUNDADORES 2',
  'GALLEGOS 1',
  'GLOBO 1',
  'GLOBO 2',
  'GLORIETA 18 DE MARZO',
  'GUANAJUATO NUMERO 2',
  'GUANAJUATO NUMERO 3',
  'HACIENDA DEL CORTE 2',
  'HACIENDA DEL CORTE 3',
  'HACIENDA GUANAME 1',
  'HACIENDA GUANAME 2',
  'HEROE DEL 47',
  'HEROES DE 1810',
  'HIDALGO 1',
  'HIDALGO 2',
  'HIDALGO NUMERO 2',
  'HIDALGO NUMERO 3',
  'HIDALGO NUMERO 4',
  'IMPRESIONISTAS 1',
  'IMPRESIONISTAS 2',
  'INDEPENDENCIA 2',
  'INDUSTRIALES 2000',
  'INVIERNO 20',
  'JUAN PABLO II NUMERO 3365',
  'JUAN PABLO II NUMERO 3433',
  'JUAN PABLO II NUMERO 3468',
  'JUAN PABLO II NUMERO 3499',
  'JUAN PABLO II NUMERO 3538',
  'JUAN PABLO II NUMERO 3565',
  'JUAN PABLO II NUMERO 3631',
  'JUAREZ 1',
  'LAS 3 M',
  'LAS 3 TUMBAS',
  'LAS 5 ESQUINAS',
  'LEON 400',
  'LINDAVISTA 1',
  'LOPEZ GUERRERO 1',
  'LOPEZ MERCADO 1',
  'LOPEZ MERCADO 3',
  'LOPEZ MERCADO 4',
  'LOS PINOS 2',
  'MAQUINA 501',
  'MAR DE JAVA 205',
  'MAR DE JAVA 211',
  'MAR DE JAVA 217',
  'MAR DE JAVA 303',
  'MAR DE JAVA 309',
  'MAR DE JAVA 315',
  'MARIANO J. GARCIA NUMERO 286',
  'MARTIRES 22 DE ABRIL',
  'MARTIRES DE 1813',
  'MEXICO 08',
  'MEXICO 2010',
  'MEXICO 68',
  'MIGUEL HIDALGO NUMERO 2',
  'MIGUEL HIDALGO NUMERO 3',
  'MODULO 1',
  'MODULO 3',
  'MODULO 4',
  'MODULO 5',
  'MORELOS 2',
  'MOROLEON 2000',
  'N. 1',
  'NARCISO MENDOZA 2',
  'NARCISO MENDOZA 3',
  'NOGAL 3',
  'NORTE 2',
  'NORTE 3',
  'NORTE 5',
  'NORTE 7',
  'NORTE 9',
  'NUMERO 1',
  'NUMERO 1 5 DE FEBRERO',
  'NUMERO 2',
  'NUMERO 3',
  'NUMERO 4',
  'NUMERO 5',
  'NUMERO 5 DE LAZARO CARDENAS',
  'NUMERO 6',
  'NUMERO 7',
  'NUMERO 8',
  'NUMERO 9',
  'OBREGON 2',
  'ORIENTE 2',
  'ORIENTE 3',
  'ORIENTE 5',
  'ORIENTE 7',
  'PINOS 1',
  'PINOS 2',
  'PIPILA 2',
  'PIPILA 3',
  'PLAZA 20 DE AGOSTO',
  'PLAZA 5 DE FEBRERO',
  'PLAZA 5 DE MAYO',
  'PLAZA 6 DE NOVIEMBRE',
  'PLAZUELA 2 DE ABRIL',
  'PONIENTE 2',
  'PONIENTE 4',
  'PONIENTE 6',
  'PONIENTE 7',
  'PRESA DEL 40',
  'PRIMERA 10 DE MAYO',
  'PRIMERA 11 DE JUNIO',
  'PRIMERA 12 DE DICIEMBRE',
  'PRIMERA 20 DE NOVIEMBRE',
  'PRIMERA 5 DE FEBRERO',
  'PRIMERA 5 DE MAYO',
  'PRIMERA 8 DE MARZO',
  'PRIMERA PRIVADA 18 DE DICIEMBRE',
  'PRIMERA PRIVADA 2 DE ABRIL',
  'PRIMERA PRIVADA 24 DE JUNIO',
  'PRIMERA PRIVADA 5 DE FEBRERO',
  'PRIMERA PRIVADA 5 DE MAYO',
  'PRIMERO DE MAYO DE 1886',
  'PRINCIPAL 1',
  'PRINCIPAL 2',
  'PROLONGACION NUMERO 5',
  'PUERTO RICO EJE ORIENTE-PONIENTE 2',
  'PURISIMA 1',
  'PURISIMA 2',
  'QUINTA 10 DE MAYO',
  'RAMIREZ 1',
  'RAMIREZ 2',
  'REAL 88',
  'REAL 89',
  'REAL 90',
  'REAL 91',
  'REAL 92',
  'REAL 93',
  'REAL 94',
  'REAL 95',
  'REAL 96',
  'REAL DE 14',
  'RENOVACION 2',
  'REVOLUCION 1910',
  'REVOLUCION DE 1910',
  'RIO SABINA 47',
  'RIVERA CANAL 13',
  'RIVERA DEL RIO 1',
  'RIVERA DEL RIO 112',
  'RIVERA DEL RIO 116',
  'RIVERA DEL RIO 124',
  'RIVERA DEL RIO 128 A',
  'SAN ANTONI0',
  'SAN ANTONIO 1',
  'SAN ANTONIO 2',
  'SAN ANTONIO 3',
  'SAN ANTONIO 4',
  'SAN JUAN DIEGO 2002',
  'SANTA BARBARA 1',
  'SANTA BARBARA 2',
  'SEGUNDA 10 DE MAYO',
  'SEGUNDA 11 DE JUNIO',
  'SEGUNDA 12 DE DICIEMBRE',
  'SEGUNDA 16 DE SEPTIEMBRE',
  'SEGUNDA 20 DE NOVIEMBRE',
  'SEGUNDA 5 DE FEBRERO',
  'SEGUNDA 5 DE MAYO',
  'SEGUNDA 8 DE MARZO',
  'SEGUNDA CERRADA 16 DE SEPTIEMBRE',
  'SEGUNDA PRIVADA 18 DE DICIEMBRE',
  'SEGUNDA PRIVADA 2 DE ABRIL',
  'SEGUNDA PRIVADA 20 DE NOVIEMBRE',
  'SEGUNDA PRIVADA 20 DE NOVIEMBRE SUR',
  'SEGUNDA PRIVADA 5 DE FEBRERO',
  'SEIS 1',
  'SEIS 2',
  'SISMO 1985',
  'SUR 2',
  'SUR 9',
  'TECNOCHITLAN 353',
  'TECNOCHITLAN 438',
  'TERAN 20',
  'TERCERA 10 DE MAYO',
  'TERCERA 11 DE JUNIO',
  'TERCERA 18 DE DICIEMBRE',
  'TERCERA 5 DE MAYO',
  'TERCERA 8 DE MARZO',
  'TERCERA DE 16 DE SEPTIEMBRE',
  'TERCERA PRIVADA 20 DE NOVIEMBRE',
  'TORRES LANDA 3',
  'TRES 2',
  'URIANGATO 400 AÑOS',
  'VALLE DE BALAM 1',
  'VALLE DE BALAM 2',
  'VALLE DE BALAM 3',
  'VICENTE GUERRERO 2',
  'VICTORIA 2',
  'VILLA SALAMANCA 400',
  'VILLAS CENTENARIO 718',
  'VISTA ALEGRE 2000')
SN_parts <- function(x) {
  t <- x
  for(s in Cachelist_SN)
    t <- str_replace_all(t, s, str_replace_all(s, " ", "âẍò"))
  t
}

# Separa calle y número
CN_parts <- function(x) {
  x = SN_parts(x)
  m <- regexec("([[:alnum:][:space:][\\.,;'&:_]+)+([[:blank:]]+)([#])?([[:blank:]]+)?([[:digit:]]+)([[:blank:]]+)?([[:alpha:]]+)?", x)
  gsub("âẍò", " ", do.call(rbind, lapply(regmatches(x, m), `[`, c(2L, 6L, 8L))))
}

# Separa calle y entrecalle
CE_parts <- function(x) {
  m <- regexec("([[:alnum:][:punct:][:space:]]+)+([[:blank:]][E][N][T][R][E][[:blank:]])([[:space:][:punct:][:alnum:]]+)+", x)
  do.call(rbind, lapply(regmatches(x, m), `[`, c(2L, 4L)))
}

# Separa calles en esquina
CS_parts <- function(x) {
  x = SS_parts(x)
  m <- regexec("([[:alnum:][:punct:][:space:]]+)+([[:blank:]][Y][[:blank:]])([[:space:][:punct:][:alnum:]]+)+", x)
  gsub("âẍò", " ", do.call(rbind, lapply(regmatches(x, m), `[`, c(2L,4L))))
}

# Excepciones al separar campo vld con entrecalles
Cachelist_SS <- c(
  'AGUILAR Y MAYA',
  'AGULAR Y MAYA',
  'ALCOCER Y GODOY',
  'ALLENDE Y UNZAGA',
  'BARON Y MORALES',
  'BODO Y ARROYITO',
  'BUCARELI Y URSULA',
  'BUSTO Y MOYA',
  'CARRANZA Y SALGADO',
  'CEDEÑA Y ARIAS',
  'CIGÜENZA Y GONGORA',
  'CORINTO Y AZABACHE',
  'CUARENTA Y NUEVE',
  'DERECHOS Y DEBERES',
  'EVORUCO Y CEVORUCO',
  'FLORES Y TULIPANES',
  'FRANCISCO Y MADERO',
  'FRATERNIDAD Y LUCHA',
  'FRIAS Y SOTO',
  'FUNEZ Y RAMIREZ',
  'GAMARRA Y DAVALOS',
  'GARCIA Y MOYEDA',
  'GARZA Y MELO',
  'GOMEZ Y PORTUGAL',
  'GRANA Y ORO',
  'GRANADEROS Y COMERCIANTES',
  'GUADALUPE Y GABRIEL',
  'GUERRA Y AGUILAR',
  'HIDALGO Y COSTILLA',
  'LIBERTAD Y SAN CAYETANO',
  'MALVA Y PLATA',
  'MANRIQUEZ Y Z.',
  'MIER Y TERAN',
  'MORELOS Y PAVON',
  'MUÑOZ Y OROZCO',
  'NAVARRETE Y G.',
  'NAZARENO Y ORO',
  'OCIO Y OCAMPO',
  'OLIVA Y OROZCO',
  'OROZCO Y BERRA',
  'ORTEGA Y DELGADO',
  'PALAFOX Y MENDOZA',
  'PASO Y TRONCOSO',
  'PEÑA Y PEÑA',
  'PERALTA Y ELIZONDO',
  'PORTILLA Y TORRES',
  'RIAÑO Y BARCENAS',
  'RIO Y LA PALOMA',
  'RUIZ Y FLORES',
  'SAN PEDRO Y SAN PABLO',
  'SOLLANO Y DAVALOS',
  'SOTO Y G.',
  'SOTO Y GAMA',
  'TAJO Y ADJUNTAS',
  'TIERRA Y LIBERTAD',
  'TRINIDAD Y TOBAGO',
  'TULIPAN Y LAS FLORES',
  'VALVERDE Y TELLEZ',
  'VERDAD Y RAMOS',
  'VIAS Y CONEX',
  'ZARATE Y SANCHEZ',
  'ZARATE Y VILLEGAS')
SS_parts <- function(x) {
 t <- x
 for(s in Cachelist_SS)
   t <- str_replace_all(t, s, str_replace_all(s, " ", "âẍò"))
 t
}

# Separa calle y número de una misma cadena
# incluido el siguiente caso:
# a ENTRE b Y c
calle_partir <- function(tw) {
  tw <- toupper(tw)
  tw <- gsub(" NO. ", " # ", tw)  # Caso: CANAL NO. 2
  
  tw <- gsub(" ESQUINA CON ", " Y ", tw)  # Caso: CANAL NO. 2
  #Si tw no tiene espacios, poner espacio entre caracteres y números (solamente)
  tw <- gsub("([[:alpha:]]+)([[:digit:]]+)", "\\1 \\2", tw)

  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final

  #@todo Separar el tipo de vialidad (incluido varios tipos)
  # Así, incluiría más exclusiones como en:
  # PRIVADA 4 (El 4 es el nombre de la privada)
  # CALLE 1
  # AMPLIACION 1986
  # [Hay que considerar que al separar no influya y no se equivoqué al buscar]
  
  parts1 <- CS_parts(tw)
  if(!is.na(parts1[1]))
    tw <- parts1[1]
  parts2 <- CE_parts(tw)
  if(!is.na(parts2[1]))
    tw <- parts2[1]
  parts3 <- CN_parts(tw)
  if(is.na(parts3[1]))
    parts3 <- c(tw, NA, NA)
  c(parts3, parts2[2], parts1[2])
}

# Casos válidos para número exterior sin número (quitando los espacios)
#S.N
#S.N.
#S/N
#SN
numero_SN <- function(tw) {
  tw <- gsub("[[:punct:]]", "", tw) # Símbolos de puntuación
  tw <- gsub("[ |\t]{2,}", "", tw)  # Tabuladores
  tw <- gsub("[[:space:]]", "", tw) # Quita espacios
  toupper(tw) == "SN"
}

# Funciones para homologar abreviaciones y términos comunes
abrev_mun <- function(tw) {
  tw <- toupper(tw)
  tw <- gsub("[ |\t]{2,}", " ", tw)  # Tabuladores
  tw <- gsub("'", " ", tw)  # Comillas
  tw <- gsub("Á", "A", tw)
  tw <- gsub("É", "E", tw)
  tw <- gsub("Í", "I", tw)
  tw <- gsub("Ó", "O", tw)
  tw <- gsub("Ú", "U", tw)
  tw <- gsub("Ü", "U", tw)
  tw <- gsub("\\.", ". ", tw)

  tw <- gsub("FCO\\. ", "FRANCISCO ", tw) # abreviaciones principales
  tw <- gsub("STA\\. ", "SANTA ", tw)

  tw <- gsub("[[:punct:]]", " ", tw) # Quita otros símbolos de puntuación
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final
  
  if(tw == "SILAO") tw <- "SILAO DE LA VICTORIA"
  if(tw == "ALLENDE") tw <- "SAN MIGUEL DE ALLENDE"
  if(tw == "SAN MIGUEL") tw <- "SAN MIGUEL DE ALLENDE"
  tw
}

abrev_loc <- function(tw) {
  tw <- toupper(tw)
  tw <- gsub("[ |\t]{2,}", " ", tw)  # Tabuladores
  tw <- gsub("'", " ", tw)  # Comillas
  tw <- gsub("Á", "A", tw)
  tw <- gsub("É", "E", tw)
  tw <- gsub("Í", "I", tw)
  tw <- gsub("Ó", "O", tw)
  tw <- gsub("Ú", "U", tw)
  tw <- gsub("Ü", "U", tw)
  tw <- gsub("\\.", ". ", tw)

  tw <- gsub("FCO\\. ", "FRANCISCO ", tw) # abreviaciones principales
  tw <- gsub("STA\\. ", "SANTA ", tw)
  tw <- gsub("NVO\\. ", "NUEVO ", tw)
  tw <- gsub("RCHO\\. ", "RANCHO ", tw)
  tw <- gsub("S\\. ", "SAN ", tw)

  tw <- gsub("[[:punct:]]", " ", tw) # Quita otros símbolos de puntuación
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final

  if(tw == "LEON") tw <- "LEON DE LOS ALDAMA"
  if(tw == "SILAO") tw <- "SILAO DE LA VICTORIA"

  tw
}

abrev_snt <- function(tw) {
  tw <- toupper(tw)
  tw <- gsub("[ |\t]{2,}", " ", tw)  # Tabuladores
  tw <- gsub("'", " ", tw)  # Comillas
  tw <- gsub("Á", "A", tw)
  tw <- gsub("É", "E", tw)
  tw <- gsub("Í", "I", tw)
  tw <- gsub("Ó", "O", tw)
  tw <- gsub("Ú", "U", tw)
  tw <- gsub("Ü", "U", tw)
  tw <- gsub("\\.", ". ", tw)
  
  tw <- gsub("COL\\. ", "COLONIA ", tw) # abreviaciones principales
  tw <- gsub("FRACC\\. ", "FRACCIONAMIENTO ", tw)
  tw <- gsub("HAB\\. ", "HABITACIONAL ", tw)
  tw <- gsub("AMPL\\. ", "AMPLIACION ", tw)
  
  tw <- gsub("OTE\\. ", "ORIENTE ", tw)
  tw <- gsub("STA\\. ", "SANTA ", tw)
  tw <- gsub("HDA\\. ", "HACIENDA ", tw)
  tw <- gsub("BARR\\. ", "BARRIO ", tw)

  tw <- gsub("FCO\\. ", "FRANCISCO ", tw)

  tw <- gsub("[[:punct:]]", " ", tw) # Quita otros símbolos de puntuación
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final
  tw
}

abrev_vld <- function(tw) {
  tw <- toupper(tw)
  tw <- gsub("[ |\t]{2,}", " ", tw)  # Tabuladores
  tw <- gsub("'", " ", tw)  # Comillas
  tw <- gsub("Á", "A", tw)
  tw <- gsub("É", "E", tw)
  tw <- gsub("Í", "I", tw)
  tw <- gsub("Ó", "O", tw)
  tw <- gsub("Ú", "U", tw)
  tw <- gsub("Ü", "U", tw)
  tw <- gsub("\\.", ". ", tw)
  tw <- gsub("AND\\. ", "ANDADADOR ", tw) # abreviaciones principales
  tw <- gsub("AVE\\. ", "AVENIDA ", tw)
  tw <- gsub("AV\\. ", "AVENIDA ", tw)
  tw <- gsub("C\\. ", "CALLE ", tw)
  tw <- gsub("CJON\\. ", "CALLEJON ", tw)
  tw <- gsub("CTO\\. ", "CIRCUITO ", tw)
  tw <- gsub("CALZ\\. ", "CALZADA ", tw)
  tw <- gsub("BLVD\\. ", "BOULEVARD ", tw)
  tw <- gsub("BULEVARD ", "BOULEVARD ", tw)
  tw <- gsub("PROL\\. ", "PROLONGACION ", tw)
  tw <- gsub("PRIV\\. ", "PRIVADA ", tw)
  tw <- gsub("CDA\\. ", "CERRADA ", tw)
  tw <- gsub("CARR\\. ", "CARRETERA ", tw)
  tw <- gsub("2DA\\. ", "SEGUNDA ", tw)

  tw <- gsub("L\\. CARDENAS ", "LAZARO CARDENAS ", tw)
  tw <- gsub("FCO\\. ", "FRANCISCO ", tw)
  tw <- gsub("HDEZ\\. ", "HERNANDEZ ", tw)
  tw <- gsub("HEROES DE LA IND\\. ", "HEROES DE LA INDEPENDENCIA ", tw)
  tw <- gsub("MA\\. ", "MARIA ", tw)
  tw <- gsub("STA\\. ", "SANTA ", tw)
  
  tw <- gsub("NTE\\. ", "NORTE ", tw)

  tw <- gsub("[[:punct:]]", " ", tw) # Quita otros símbolos de puntuación
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final

  if(tw == "CONOCIDO") tw <- ""

  tw
}

# Para adaptar la función Soundex
rsna <- function(tw = "") {
  tw <- gsub("Ñ", "NY", tw)
  tw
}
#And yet other rules eliminate certain languages “kie”: not Spanish

# Función recurrente, creada por comodidad
limp_abrev <- function(dom, abrevx) {
  if(dom != "")
    dom <- abrevx(dom)
  dom <- limpieza(dom)
  return(dom)
}

# Quita los espacios extras de una cadena
quita_espacios <- function(tw) {
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final
  tw
}

# Separar localidades con más acepciones en sus elementos
localidad_partir_p <- function(tw) {
  m <- regexec("([[:alnum:][:space:][[:punct:]]+)+([\\(]+)([[:alnum:][:space:][[:punct:]]+)+([\\)]+)", tw)
  do.call(rbind, lapply(regmatches(tw, m), `[`, c(2L,4L)))
}

localidad_partir_c <- function(tw) {
  m <- regexec("([[:alnum:][:space:][[:punct:]]+)+([[]+)([[:alnum:][:space:][[:punct:]]+)+([]]+)", tw)
  do.call(rbind, lapply(regmatches(tw, m), `[`, c(2L,4L)))
}
