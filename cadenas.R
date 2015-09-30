# Limpieza de textos
limpieza <- function(tw = "") {
  tw <- toupper(tw)
  #  tw <- gsub("[PRIV/.]","PRIVADA",tw) # abreviaciones principales
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
  tw <- gsub("[[:punct:]]", "", tw) # Quita puntuación
  tw <- gsub("[ |\t]{2,}", "", tw)  # Tabuladores
  tw <- gsub("[[:space:]]", "", tw) # Quita espacios
  tw
}

# Separa calle y número en una misma cadena
calle_partir <- function(tw) {
  tw <- toupper(tw)
  tw <- gsub(" NO. ", " # ", tw)  # Caso: CANAL NO. 2
  m <- regexec("([[:alpha:][:space:]]+)+([[:blank:]]+)([#])?([[:blank:]]+)?([[:digit:]]+)([[:blank:]]+)?([[:alpha:]]+)?", tw)
  parts <- do.call(rbind, lapply(regmatches(tw, m), `[`, c(2L, 6L, 8L)))
  colnames(parts) <- c("Calle","Numero","Interior")
  parts
}
# @todo
# Faltan el caso:
# PRIVADA 4 (El 4 es el nombre de la privada)

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
  tw <- gsub(".", ". ", tw)

  tw <- gsub("FCO. ", "FRANCISCO ", tw) # abreviaciones principales
  tw <- gsub("STA. ", "SANTA ", tw)

  tw <- gsub("[[:punct:]]", " ", tw) # Quita otros símbolos de puntuación
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final
  
  if(tw == "SILAO") tw <- "SILAO DE LA VICTORIA"
  
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
  tw <- gsub(".", ". ", tw)

  tw <- gsub("FCO. ", "FRANCISCO ", tw) # abreviaciones principales
  
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
  tw <- gsub(".", ". ", tw)
  
  tw <- gsub("COL. ", "COLONIA ", tw) # abreviaciones principales
  tw <- gsub("FRACC. ", "FRACCIONAMIENTO ", tw)
  tw <- gsub("HAB. ", "HABITACIONAL ", tw)
  
  tw <- gsub("OTE. ", "ORIENTE ", tw)
  tw <- gsub("STA. ", "SANTA ", tw)
  tw <- gsub("HDA. ", "HACIENDA ", tw)

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
  tw <- gsub(".", ". ", tw)
  tw <- gsub("AND. ", "ANDADADOR ", tw) # abreviaciones principales
  tw <- gsub("AVE. ", "AVENIDA ", tw)
  tw <- gsub("AV. ", "AVENIDA ", tw)
  tw <- gsub("C. ", "CALLE ", tw)
  tw <- gsub("CTO. ", "CIRCUITO ", tw)
  tw <- gsub("BLVD. ", "BOULEVARD ", tw)
  tw <- gsub("BULEVARD ", "BOULEVARD ", tw)
  tw <- gsub("PROL. ", "PROLONGACION ", tw)
  tw <- gsub("PRIV. ", "PRIVADA ", tw)

  tw <- gsub("L.CARDENAS ", "LAZARO CARDENAS ", tw)
  tw <- gsub("FCO. ", "FRANCISCO ", tw)
  tw <- gsub("HDEZ. ", "HERNANDEZ ", tw)
  tw <- gsub("HEROES DE LA IND. ", "HEROES DE LA INDEPENDENCIA ", tw)
  tw <- gsub("MA. ", "MARIA ", tw)
  
  tw <- gsub("NTE. ", "NORTE ", tw)

  tw <- gsub("[[:punct:]]", " ", tw) # Quita otros símbolos de puntuación
  tw <- gsub("[[:space:]]+", " ", tw) # Espacios dobles generados
  tw <- gsub("^ ", "", tw)  # Quita espacio al principio
  tw <- gsub(" $", "", tw)  # Quita espacio al final
  tw
}
