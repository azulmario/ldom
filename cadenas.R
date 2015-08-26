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