# The MIT License (MIT)
# Copyright (c) 2015-2016 Mario Hernández Morales
#
# Librería que implementa la búsqueda inteligente de
# domicilios geográficos mediante árbol de decisión.

library(stringr)    # Para la conversión de tipos numéricos a cadenas
library(stringdist) # Para las comparaciones de nombres topográficos
library(geosphere)  # Cálculo de distancias geográficas
source("cadenas.R")   # Para la conversión de tipos numéricos a cadenas
source("conectadb.R") # Para obtener la información de los servidores


# Variables de control
# Para mostrar el mapa durante el diseño, inhabilitar en producción
map.is.visible = FALSE
mapa = NULL

# Variable auxiliar usada en común para toda la lista
# definida en lee() y utilizada en identifica()
alcance0 <- NULL

#-------------------------------------------------------------------
# Mostrar el resultado en un mapa
# Útil para probar, depurar y mostrar el código
hace_mapa <- function(r, r_zoom = NULL) {
  require(leaflet)
  mapa <<- leaflet(data = r) %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    addMarkers(~lon[], ~lat[], popup=~nombre)
  if(!is.null(r_zoom)) {
    mapa <<- setView(mapa, lng = r$lon[1], lat = r$lat[1], zoom = r_zoom)
  }
  print(mapa)
}

# Para realizar las búsquedas de cada componente de la dirección
# Utiliza la métrica Jaro-Winkler.

#-------------------------------------------------------------------
# Primero identifica el municipio
Cachelist_mun <- NULL
identifica_mun <- function(dom.mun) {
  if(is.null(Cachelist_mun)) {
    origen <- mun.php()
    origen$nombre <- limpieza(as.character(origen$nombre))
    origen$cve <- mapply(str_pad, origen$cve, 3, pad = "0")
    d <- mapply(gloc.php, origen$cve)
    lat <- as.numeric(d[1,])
    lon <- as.numeric(d[2,])
    origen <- data.frame(origen, lat, lon)
    Cachelist_mun <<- origen
  } else {
    origen <- Cachelist_mun
  }

  # Calcula distancias
  # 0 <= p <= 0.25
  origen$BM <- stringdistmatrix(origen$nombre, dom.mun, method="jw", p = 0.1)
  origen <- origen[origen$BM <= 0.1 + min(origen$BM),]
  origen$BM <- stringdistmatrix(rsna(origen$nombre), rsna(dom.mun), method = "cosine")
  r_mun <- origen[origen$BM <= 0.1 | origen$BM <=  0.01 + min(origen$BM),] # Obtiene el mínimo

  if(map.is.visible) {
    hace_mapa(r_mun, 11)
  }
  r_mun$niv <- 5
  return(r_mun[,c("BM","cve","nombre","lat","lon","niv")])
}

# Manejo de cache para el listado de localidades de cada municipio
Cachelist_loc <- vector(mode = "list", length = 46)
# Regresa el listado de localidades, considerando el uso de dos nombres
# Incluye casos A〔B〕 duplicando claves
list_loc <- function(cve_mun = "001") {
  if(is.null(Cachelist_loc[[as.numeric(cve_mun)]])) {
    origen1 <- loc.php(cve_mun)
    # Localidades con formato de paréntesis
    origen2 <- loc2.php(cve_mun)
    origen3 <- cbind(origen2, as.data.frame(localidad_partir_p(as.vector(origen2$nombre))))
    origen4 <- origen5 <- NULL
    if (length(origen3$cve) > 0) {
      origen4 <- data.frame(origen3$cve, origen3$V1, origen3$lat, origen3$lon)
      origen5 <- data.frame(origen3$cve, origen3$V2, origen3$lat, origen3$lon)
      colnames(origen5) <- colnames(origen4) <- c("cve", "nombre", "lat", "lon")
    }
    # Localidades con formato de corchetes
    origen2 <- loc3.php(cve_mun)
    origen3 <- cbind(origen2, as.data.frame(localidad_partir_c(as.vector(origen2$nombre))))
    origen6 <- NULL
    if(length(origen3$cve) > 0) {
      origen6 <- data.frame(origen3$cve, paste(origen3$V2, "", origen3$V1), origen3$lat, origen3$lon)
      colnames(origen6) <- c("cve", "nombre", "lat", "lon")
    }
    # Pega la información en una única lista
    origen <- rbind(origen1, origen4, origen5, origen6)
    origen$nombre <- limpieza(as.character(origen$nombre))
    origen$cve <- str_pad(origen$cve, 4, pad = "0")

    Cachelist_loc[[as.numeric(cve_mun)]] <<- unique(origen[complete.cases(origen),])
  }
  return(Cachelist_loc[[as.numeric(cve_mun)]])
}

#-------------------------------------------------------------------
# Segundo identifica la localidad
# Nota: Considerar permuta con colonia, principalmente en zona rural
identifica_loc <- function(dom.loc, r_mun) {
  if(is.na(dom.loc) || dom.loc == '') {
    return(NULL)
  }

  origen <- list_loc(r_mun$cve)

  if(length(origen$nombre) > 0) {
    origen$BM <- stringdistmatrix(origen$nombre, dom.loc, method="jw", p = 0.1)
    origen <- origen[origen$BM <= 0.1 + min(origen$BM),]
    origen$BM <- stringdistmatrix(rsna(origen$nombre), rsna(dom.loc), method = "cosine")
    r_loc <- origen[origen$BM <= 0.1 | (origen$BM <= 0.25 & origen$BM <=  0.01 + min(origen$BM)),] # Obtiene el mínimo

    if(length(r_loc$BM) == 0)
      return(NULL)
    
    if(map.is.visible & length(r_loc$lat) > 0) {
      hace_mapa(r_loc, 13)
    }
    r_loc$niv <- 4
    r_loc$BM <- 1 - (1 - r_loc$BM) * (1 - as.numeric(r_mun$BM)) # Teorema de Bayes
    r_loc$nombre <- paste (r_mun$nombre, r_loc$nombre, sep = ", ") # Nombre de localidad
    r_loc$cve <- paste0(r_mun$cve, r_loc$cve) # Clave de localidad

    return(r_loc[,c("BM","cve","nombre","lat","lon","niv")])
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------
# En el caso de no declarar la localidad, y debido a su importancia
# se busca en las localidades urbanas, descartando el resto.
# Se asegura que se busque, pero sin exagerar.
identifica_locurb <- function(r_mun) {
  origen <- loc.php(m = r_mun$cve)
  if(length(origen$nombre) > 0) {
    zurb <- switch(as.numeric(r_mun$cve),
                  c(1,117),
                  c(1,17,27,47),
                  c(1,67,192,236,244,306,591),
                  c(1,65,71),
                  c(1,3,8,21,30,33,36,45,47,51,298),
                  c(1),
                  c(1,91,109,118,130,132,135,143,146,153,154,156,159,
                   161,165,169,174,178,185,245,250,256,258,414,415),
                  c(1),
                  c(1,10,18,25,33,49),
                  c(1),
                  c(1,19,22,54),
                  c(1),
                  c(1),
                  c(1,132,255,742),
                  c(1,31,34,39,67,91,102,111,115,126),
                  c(1),
                  c(1,59,62,67,68,69,80,81,82,95,99,101,102,
                   113,138,148,161,163,165,176,177,181,359),
                  c(1,4,30,35),
                  c(1,27,68),
                  c(1,263,267,277,317,335,340,345,347,358,401,413,429,435,
                   439,445,451,452,458,464,703,785,786,869,885,975,1269,1294),
                  c(1),
                  c(1,85),
                  c(1,93,104,148,177,192,210,288),
                  c(1),
                  c(1,34,155),
                  c(1,23),
                  c(1,43,46,59,87,110,118,124,133,135,161),
                  c(1,31,48,58,60,64),
                  c(1,103),
                  c(1,47,71,110,120,211,251,265),
                  c(1,32,61,74),
                  c(1,15,86),
                  c(1,110,149,203,428,778),
                  c(1),
                  c(1,47,50,53,75),
                  c(1),
                  c(1,4,6,19,22,24,31,62,66,84,91,191),
                  c(1),
                  c(1,22,26),
                  c(1),
                  c(1),
                  c(1,86,117),
                  c(1),
                  c(1,12,23,42,43,44),
                  c(1),
                  c(1,12,44,75,76))

    r_loc <- NULL
    for(i in zurb)
      r_loc <- rbind(r_loc, origen[origen$cve == i,])

    m <- mapply(str_pad, r_mun$cve, 3, pad = "0") 
    l <- mapply(str_pad, r_loc$cve, 4, pad = "0")

    if(map.is.visible && length(r_loc$lat) > 0) {
      hace_mapa(r_loc, 13)
    }
    r_loc$niv <- 4
    r_loc$BM <- 1.0 - (1.0 - as.numeric(r_mun$BM)) * (0.9999999) 
    r_loc$cve <- paste0(m, l) # Clave de localidad
    r_loc$nombre <- paste (r_mun$nombre, r_loc$nombre, sep = ", ") # Nombre de localidad

    return(r_loc[c("BM", "cve", "nombre", "lat", "lon", "niv")])
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------
# Tercero identifica la colonia
identifica_snt <- function(dom.snt, r_loc) {
  if(is.na(dom.snt) || dom.snt == "" || dom.snt == "." || dom.snt == "..") {
    return(NULL)
  }

  origen <- col.php(m = substr(r_loc$cve, 1, 3), l = substr(r_loc$cve, 4, 7))
  origen <- origen[complete.cases(origen),]

  if(length(origen$nom_asen) > 0) {
    origen$nom_asen <- limpieza(as.character(origen$nom_asen))

    origen$BM <- stringdistmatrix(origen$nom_asen, dom.snt, method="jw", p = 0.1)
    origen <- origen[origen$BM <= 0.1 + min(origen$BM),]
    origen$BM <- stringdistmatrix(rsna(origen$nom_asen), rsna(dom.snt), method = "cosine")
    r_snt <- origen[origen$BM <= 0.1 | (origen$BM <= 0.25 & origen$BM <=  0.01 + min(origen$BM)),] # Limita los nombres no parecidos

    if(length(r_snt$cve_asen) == 0)
      return(NULL)

    # Obtiene las coordenadas y el radio del subconjunto
    d <- sapply(str_pad(as.character(r_snt$cve_asen), 4, pad = "0"), gcol.php)
    lat <- as.numeric(d[1,])
    lon <- as.numeric(d[2,])
    radio <- round(as.numeric(d[3,]), digits = 2)
    r_snt <- data.frame(r_snt, lat, lon)
    colnames(r_snt)[1] <- "cve"
    colnames(r_snt)[2] <- "nombre"
    if(map.is.visible & length(lat) > 0) {
      hace_mapa(r_snt, 15)
    }
    r_snt$cve <- paste(r_loc$cve, r_snt$cve, radio, sep = ",") # Clave de asentamiento
    r_snt$niv <- 3
    r_snt$BM <- 1 - (1 - r_snt$BM) * (1 - as.numeric(r_loc$BM))
    r_snt$nombre <- paste(r_loc$nombre, r_snt$nombre, sep = ", ") # Nombre de asentamiento
    return(r_snt[c("BM", "cve", "nombre", "lat", "lon", "niv")])
  }
  return(NULL)
}

#-------------------------------------------------------------------
# Cuarto identifica la vialidad

# Una colonia tiene en promedio un radio de 502 m, con una desviación
# estándar de 343 metros.
# La estrategia es buscar dentro de un radio de 3 sigmas desde el centro
# interno de la colonia. Hay que considerar que algunas calles por su longitud
# se referencia puede salir del rango, por lo que se considera como un método
# de refinamiento.
identifica_vld <- function(dom.vld, r_loc, r_snt = NULL, tipo = TRUE) {
  if(is.na(dom.vld) || dom.vld == "" || dom.vld == "." || dom.vld == ".." || dom.vld == "...") {
    return(NULL)
  }
  origen <- cal2.php(m = substr(r_loc$cve, 1, 3), l = substr(r_loc$cve, 4, 7))

  if(length(origen$nom_via) > 0) {
    origen$apx <- ""
    if(!is.null(r_snt)) { # Si busca en asentamientos,
      origen <- origen[!(is.na(origen$lat)|is.na(origen$lon)),] # que todos tengan coordenadas
      if(length(origen$nom_via) == 0)
        return(NULL)
    } else { # Aquellos que no tengan coordenadas las hereden de su localidad
      for(i in 1:length(origen$lat)) {
        if(is.na(origen[i,]$lat) ||  is.na(origen[i,]$lon)) {
          origen[i,]$lat <- r_loc$lat
          origen[i,]$lon <- r_loc$lon
          origen[i,]$apx <- " [aprox.]"
        }
      }
    }

    # Si se cuenta con información de las colonias, calcula las distancia
    origen$e <- ""
    origen$d <- NA
    if(!is.null(r_snt)) {
      cs <- length(r_snt$lat)
      while(cs > 0) {
        lat2 <- r_snt[cs,]$lat ; long2 <- r_snt[cs,]$lon
        # Calcula la distancia con el centro de la colonia
        cv <- length(origen$lat)
        while(cv > 0) {
          lat1 <- origen[cv,]$lat ; long1 <- origen[cv,]$lon
          d0 <- distGeo(c(long1,lat1), c(long2,lat2))
          r0 <- as.numeric(strsplit(r_snt$cve[cs], split=",")[[1]][3]) # Utiliza el radio como influencia
          if((d0 <= 4.0 * r0) && (is.na(origen[cv,]$d) || (d0 < origen[cv,]$d))) {
            origen[cv,]$d <- d0
            origen[cv,]$e <- strsplit(r_snt[cs,]$nombre, split=",")[[1]][3]
          }
          cv <- cv - 1
        }
        cs <- cs - 1
      }
      origen <- origen[!is.na(origen$d),]
    }

    # Hace limpieza de los candidatos de nombres
    origen$nom_via <- limpieza(origen$nom_via)
    origen$nom_via0 <- limpieza(origen$nom_via0)

    # Calcula las distancias con cada uno de los candidatos
    origen$B <- stringdistmatrix(origen$nom_via, dom.vld, method="jw", p = 0.1)
    origen$M <- stringdistmatrix(origen$nom_via0, dom.vld, method="jw", p = 0.1)
    origen <- origen[origen$B <= 0.1 + min(origen$B) | origen$M <= 0.1 + min(origen$M),]
    origen$B <- stringdistmatrix(rsna(origen$nom_via), rsna(dom.vld), method="cosine")
    origen$M <- stringdistmatrix(rsna(origen$nom_via0), rsna(dom.vld), method="cosine")
    origen$BM <- pmin(origen$B, 0.001 + origen$M)
    r_vld <- origen[origen$BM <= 0.1 | (origen$BM <= 0.25 & origen$BM <=  0.01 + min(origen$BM)),]

    if(length(r_vld$BM) == 0)
      return(NULL)
    
    colnames(r_vld)[1] <- "cve"
    colnames(r_vld)[2] <- "nombre"
    if(map.is.visible & length(r_vld$lat) > 0 && !is.na(r_vld$lat)) {
      hace_mapa(r_vld, 17)
    }
    r_vld$niv <- 2
    
    if(tipo || is.null(r_snt)) { # @error
      r_vld$BM <- 1 - (1 - r_vld$BM) * (1 - as.numeric(r_loc$BM))
    } else {
      u_ <- r_snt$BM
      u_ <- u_[ complete.cases(u_) ]
      u_ <- max(u_)
      r_vld$BM <- 1 - (1 - r_vld$BM) * (1 - u_)
    }
    r_vld$nombre <- paste(r_loc$nombre, r_vld$e, r_vld$nombre, sep = ", ") # Nombre de vialidad
    r_vld$nombre <- gsub(", , ", ", ", r_vld$nombre)
    r_vld$nombre <- paste0(r_vld$nombre, r_vld$apx)
    r_vld$cve <- mapply(str_pad, r_vld$cve, 12, pad = "0")

    return(r_vld[,c("BM","cve","nombre","lat","lon","niv")])
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------
# Quinto identifica la entrecalle
identifica_ref <- function(dom.ref, r_vld) {
  destino <- limpieza(dom.ref)
  if(is.na(dom.ref) || destino == "" || destino == "." || destino == ".." || destino == "...") {
    return(NULL)
  }

  origen <- ecal.php(c = r_vld$cve)

  if(length(origen$nom_via) > 0) {
    origen$nom_via <- limpieza(as.character(origen$nom_via))

    origen$BM <- stringdistmatrix(origen$nom_via, destino, method="jw", p = 0.1)
    origen <- origen[origen$BM <= 0.1 + min(origen$BM),]
    origen$BM <- stringdistmatrix(rsna(origen$nom_via), rsna(destino), method = "cosine")
    r_ref <- origen[origen$BM <= 0.1 | (origen$BM <= 0.25 & origen$BM <=  0.01 + min(origen$BM)),]

    if(length(r_ref$cve) == 0)
      return(NULL)

    d <- sapply(r_ref$cve, gecal.php, e = r_vld$cve) # Obtiene las coordenadas de la esquina
    lat <- as.numeric(d[1,])
    lon <- as.numeric(d[2,])
    r_ref <- data.frame(r_ref, lat, lon)
    colnames(r_ref)[2] <- "nombre"
    if(map.is.visible & length(r_ref$lat) > 0) {
      hace_mapa(r_ref, 17)
    }
    r_ref$niv <- 1
    r_ref$BM <- 1 - (1 - r_ref$BM) * (1 - as.numeric(r_vld$BM))
    r_ref$nombre <- paste(r_vld$nombre, r_ref$nombre, sep = " Y ")
    r_ref$cve <- paste(r_vld$cve, r_ref$cve, sep = "x")
    return(r_ref[,c("BM","cve","nombre","lat","lon","niv")])
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------
# Sexto identifica el número exterior.
# Cumple con el requisito de pintar varios puntos.
# Se cambia la comparación textual por numérica.
identifica_num <- function (dom.num, r_vld) {
  if(is.null(dom.num) || is.na(dom.num)) {
    return(NULL)
  }

  origen <- num.php(c = r_vld$cve)
  origen$num <- limpieza0(origen$num)
  origen <- origen[complete.cases(origen),]

  if(length(origen$num) > 0) {
    origen$BM <- 2*pnorm(sqrt(2)*(abs(origen$num-dom.num)/50))-1
    r_num <- origen[origen$BM <= 0.12 & origen$BM ==  min(origen$BM),]
    
    if(length(r_num$BM) == 0)
      return(NULL)

    # Regrese el más cercano
    # o pares cuando no sea exácto
    # excluye a los números muy alejados

    r_num$cve <- r_vld$cve
    r_num$num <- as.character(r_num$num)
    colnames(r_num)[3] <- "nombre"
    if(map.is.visible & length(r_num$lat) > 0) {
      hace_mapa(r_num, 18)
    }
    r_num$niv <- 0
    r_num$BM <- 1 - (1 - r_num$BM) * (1 - as.numeric(r_vld$BM))
    r_num$cve <- paste(r_num$cve, r_num$nombre, sep = "#") # Clave
    r_num$nombre <- paste(r_vld$nombre, r_num$nombre, sep = " #") # Dirección

    return(r_num[,c("BM","cve","nombre","lat","lon","niv")])
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------
# Identifica el árbol de decisión

# Método ID3
# Ross Quinlan (1986) Induction Decision Tree. Machine Learning [1979]
# Breiman et al. (1984) Classification and Regression Trees. Belmont, Calif.: Wadsworth.
# Técnica de aprendizaje automático
# Inducción de árboles de decisión
# Estrategia top-down

# Es un algoritmo voraz para la construcción automática de árboles
# de decisión, que selecciona en cada paso el mejor atributo.
# El mejor es el más discrimina (potencialmente más útil).
# Está basado en el principio de entropía informática (Shannon 1951),
# que mide el grado de aleatoriedad e incertidumbre
# de los datos, también puede verse como una medida del desorden.

# El proceso de construcción es iterativo:
# 1- Se selecciona un subconjunto de ejemplos del conjunto
#    disponible para el nodo i (nivel geográfico).
# 2- Se construye (induce) el árbol de decisión que permita discriminar
#    el conjunto de ejemplos para el siguiente nodo i+1 de decisión.

# Seleccionar en cada paso el atributo que discrimina más, así
# permite reducir el tamaño del árbol de decisión.
# La selección se hace maximizando una cierta función G, que representa
# la ganancia de información.

IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

# Entropía (aleatoriedad del árbol)
#   E = sum_{i=1}^n -p_i log_2(p_i)
# El valor p_i es la probabilidad de éxito
entropia <- function (Tt) {
  vls <- 1 - Tt$BM
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

# De la lista co2, regresa aquellas no consideradas en ad
norep <- function(co2, ad) {
  co <- NULL
  for(k in 1:length(co2$cve)) {
    if(!(any(co2$cve[k] == ad$cve))){
      co <- rbind(co, co2[k,])
    }
  }
  return(co)
}

# Esquema de trabajo con listas tratadas como árboles de decisiones
identifica <- function (dom, map = FALSE) {
  alcance <- NULL
  if(is.null(alcance0)){
    alcance <- list(mun = "mun"  %in% colnames(dom), loc = "loc" %in% colnames(dom),
                    tsnt = "tsnt" %in% colnames(dom), snt = "snt" %in% colnames(dom), 
                    tvld = "tvld" %in% colnames(dom), vld = "vld" %in% colnames(dom),
                    num = "num"  %in% colnames(dom), int = "int" %in% colnames(dom),
                    ref1 = "ref1" %in% colnames(dom), ref2 = "ref2" %in% colnames(dom),
                    CP = "CP"   %in% colnames(dom))
  } else {
    alcance <- alcance0
  }
  # Identifica abreviaciones y las sustituye
  dom$mun <- limp_abrev(dom$mun, abrev_mun)
  dom$loc <- limp_abrev(dom$loc, abrev_loc)
  dom$snt <- limp_abrev(dom$snt, abrev_snt)
  dom$vld <- limp_abrev(dom$vld, abrev_vld)
  c_SN <- numero_SN(dom$num)
  if(c_SN){
    dom$num <- NA
  } else {
    dom$num <- limpieza0(dom$num)
  }

  # Si no hay número exterior, verifica si no está incluido en la vialidad,
  # cuando se especifica sin número no aplica.
  # también separar los casos de vialidad con entrecalles.
  if(is.na(dom$num) && !c_SN && dom$vld != "") {
    q <- calle_partir(dom$vld)
    if(!is.na(q[1])) {
      dom$vld <- q[1]
    }
    if(!is.na(q[2])) {
      dom$num <- limpieza0(q[2]) # Se crea el número
      alcance$num <- TRUE # Se declara en alcance
    }
    if(!is.na(q[4])) {
      dom$ref1 <- q[5]
      dom$ref2 <- q[4]
      alcance$ref1 <- TRUE
      alcance$ref2 <- TRUE
    } else if(!is.na(q[5])) {
      dom$ref1 <- q[5]
      alcance$ref1 <- TRUE
    }
  }

  r_mun <- identifica_mun(dom$mun)
  ad <- r_mun
  #r_mun$BM <- 0 # Solo prueba con un municipio

  if(dom$loc == "" && dom$snt == "" && dom$vld == "")
    return(ad[c(6, 1, 2, 3, 4, 5)])

  i <- length(r_mun$cve)
  while(i > 0) {
    if(dom$loc == "") {
      r_loc <- identifica_locurb(r_mun[i,])
    } else {
      r_loc <- identifica_loc(dom$loc, r_mun[i,])
    }
    ad <- rbind(ad, r_loc)

    if(dom$snt == "" && dom$vld == "") {
      i <- i - 1 # @error: evita que se permuten o hagan las comprobaciones de conurbación de localidad
      next
    }

    j <- length(r_loc$cve)
    while(j > 0) {
      if(alcance$tsnt && dom$tsnt != "") {
        dom.snt <- paste(dom$tsnt, dom$snt)
        r_snt <- identifica_snt(dom.snt, r_loc[j,])
        ad <- rbind(ad, r_snt)
      } else {
        r_snt <- identifica_snt(dom$snt, r_loc[j,])
        ad <- rbind(ad, r_snt)
      }
      if(dom$vld != "") {
        # Si tenemos multiplicidad de asentamientos, incluir cada uno
        # y además el caso sin considerarlo.
        r_vld <- identifica_vld(dom$vld, r_loc[j,], r_snt)
        if(!is.null(r_snt) && (is.null(r_vld) || min(r_vld$BM) != 0)) {
          r_vld0 <- norep(identifica_vld(dom$vld, r_loc[j,]), r_vld)
          r_vld <- rbind(r_vld, r_vld0)
        }
        ad <- rbind(ad, r_vld)

        k <- length(r_vld$cve)
        while(k > 0) {
          if(alcance$ref1 && dom$ref1 != "" && limpieza(dom$ref1) != "") {
            r_ref1 <- identifica_ref(dom$ref1, r_vld[k,])
            ad <- rbind(ad, r_ref1)
          }
          if(alcance$ref2 && dom$ref2 != "" && limpieza(dom$ref2) != "") {
            r_ref2 <- identifica_ref(dom$ref2, r_vld[k,])
            ad <- rbind(ad, r_ref2)
          }
          if(alcance$num && !is.na(dom$num)) {
            r_num <- identifica_num(dom$num, r_vld[k,])
            ad <- rbind(ad, r_num)
          }
          k <- k - 1
        }
      }
      # Verifica si ya encontró un mínimo al nivel cero
      if(length(ad[ad$niv == 0,]$BM) > 0 && min(ad[ad$niv == 0,]$BM) == 0) {
        if(map)
          hace_mapa(ad)
        return(ad[c(6, 1, 2, 3, 4, 5)])
      }

      # Alternativas de localidad en misma conurbación
      # puede alterar el municipio
      co <- norep(conurbacion(r_loc[j,]$cve), ad)

      # Agregar a la lista tales localidades adicionales
      ad <- rbind(ad, co)

      cj <- length(co$cve)
      while(cj > 0) {
        co_cj <- co[cj,]
        co_cj$BM <- r_loc[j,]$BM
        if(alcance$tsnt && dom$tsnt != "") {
          dom.snt <- paste(dom$tsnt, dom$snt)
          r_snt <- norep(identifica_snt(dom.snt, co_cj), ad)
          ad <- rbind(ad, r_snt)
        } else {
          r_snt <- norep(identifica_snt(dom$snt, co_cj), ad)
          ad <- rbind(ad, r_snt)
        }

        if(dom$vld != "") {
          # Similar al anterior, probando con localidades hermanas
          r_vld <- identifica_vld(dom$vld, co[cj,], r_snt)
          if(!is.null(r_snt) && (is.null(r_vld) || min(r_vld$BM) != 0)) {
            r_vld0 <- identifica_vld(dom$vld, co[cj,])
            r_vld <- rbind(r_vld, r_vld0)
          }
          ad <- rbind(ad, r_vld)
          
          k <- length(r_vld$cve)
          while(k > 0) {
            if(alcance$ref1 && dom$ref1 != "" && limpieza(dom$ref1) != "") {
              r_ref1 <- identifica_ref(dom$ref1, r_vld[k,])
              ad <- rbind(ad, r_ref1)
            }
            if(alcance$ref2 && dom$ref2 != "" && limpieza(dom$ref2) != "") {
              r_ref2 <- identifica_ref(dom$ref2, r_vld[k,])
              ad <- rbind(ad, r_ref2)
            }
            if(alcance$num && !is.na(dom$num)) {
              r_num <- identifica_num(dom$num, r_vld[k,])
              ad <- rbind(ad, r_num)
            }
            k <- k - 1
          }
        }
        cj <- cj - 1
      }

      # Continúa...
      j <- j - 1
    }
    # Intercambia localidad y colonia, solo si son de nombre distinto.
    destino <- limpieza(dom$snt)
    if (dom$snt != "" && destino != "" && destino != "." && destino != ".." &&
        destino != limpieza(dom$loc) ) {
      r_mun_i <- r_mun[i,]
      r_mun_i$BM <- 1.0 - (1.0 - r_mun[i,]$BM) * (1.0 - 1e-7)
      r_loc <-identifica_loc(dom$snt, r_mun_i)
      ad <- rbind(ad, r_loc)
      j <- length(r_loc$cve)
      while(j > 0) {
        r_snt <- identifica_snt(dom$loc, r_loc[j,])
        ad <- rbind(ad, r_snt)
        r_snt <- identifica_snt(dom$snt, r_loc[j,])
        ad <- rbind(ad, r_snt)
        if(dom$vld != "") {
          # Hay dos candidatos de asentamiento, por eso no se usa
          r_vld <- identifica_vld(dom$vld, r_loc[j,])
          ad <- rbind(ad, r_vld)
          k <- length(r_vld$cve)
          while(k > 0) {
            if(alcance$ref1 && dom$ref1 != "" && limpieza(dom$ref1) != "") {
              r_ref1 <- identifica_ref(dom$ref1, r_vld[k,])
              ad <- rbind(ad, r_ref1)
            }
            if(alcance$ref2 && dom$ref2 != "" && limpieza(dom$ref2) != "") {
              r_ref2 <- identifica_ref(dom$ref2, r_vld[k,])
              ad <- rbind(ad, r_ref2)
            }
            if(alcance$num && !is.na(dom$num)) {
              r_num <- identifica_num(dom$num, r_vld[k,])
              ad <- rbind(ad, r_num)
            }
            k <- k - 1
          }
        }
        #
        # Alternativas de localidad en misma conurbación; puede alterar el municipio
        co <- norep(conurbacion(r_loc[j,]$cve), ad)

        # Agregar a la lista tales localidades
        ad <- rbind(ad, co)

        cj <- length(co$cve)
        while(cj > 0) {
          co_cj <- co[cj,]
          co_cj$BM <- r_loc[j,]$BM
          r_snt <- identifica_snt(dom$snt, co_cj)
          ad <- rbind(ad, r_snt)
          if(alcance$tsnt && dom$tsnt != "") {
            dom.snt <- paste(dom$tsnt, dom$snt)
            r_snt <- identifica_snt(dom.snt, co_cj)
            ad <- rbind(ad, r_snt)
          }

          if(dom$vld != "") {
            # Idéntico al primer caso
            r_vld <- identifica_vld(dom$vld, co[cj,], r_snt)
            if(!is.null(r_snt) && (is.null(r_vld) || min(r_vld$BM) != 0)) {
              r_vld0 <- identifica_vld(dom$vld, co[cj,])
              r_vld <- rbind(r_vld, r_vld0)
            }
            ad <- rbind(ad, r_vld)

            k <- length(r_vld$cve)
            while(k > 0) {
              if(alcance$ref1 && dom$ref1 != "" && limpieza(dom$ref1) != "") {
                r_ref1 <- identifica_ref(dom$ref1, r_vld[k,])
                ad <- rbind(ad, r_ref1)
              }
              if(alcance$ref2 && dom$ref2 != "" && limpieza(dom$ref2) != "") {
                r_ref2 <- identifica_ref(dom$ref2, r_vld[k,])
                ad <- rbind(ad, r_ref2)
              }
              if(alcance$num && !is.na(dom$num)) {
                r_num <- identifica_num(dom$num, r_vld[k,])
                ad <- rbind(ad, r_num)
              }
              k <- k - 1
            }
          }
          cj <- cj - 1
        }
        #
        j <- j - 1
      }
    }
    # Si no hay colonia, pero la localidad debería ser la colonia
    destino <- limpieza(dom$loc)
    if(dom$snt == "" && destino != "" && destino != "." && destino != "..") {
      r_mun_i <- r_mun[i,]
      r_mun_i$BM <- 1.0 - (1.0 - r_mun[i,]$BM) * (1.0 - 0.05)
      r_loc <-identifica_locurb(r_mun_i)
      ad <- rbind(ad, r_loc)
      j <- length(r_loc$cve)
      while(j > 0) {
        r_snt <- identifica_snt(destino, r_loc[j,])
        ad <- rbind(ad, r_snt)
        if(dom$vld != "") {
          r_vld <- identifica_vld(dom$vld, r_loc[j,], r_snt, tipo = FALSE)
          ad <- rbind(ad, r_vld)
          
          k <- length(r_vld$cve)
          while(k > 0) {
            if(alcance$ref1 && dom$ref1 != "" && limpieza(dom$ref1) != "") {
              r_ref1 <- identifica_ref(dom$ref1, r_vld[k,])
              ad <- rbind(ad, r_ref1)
            }
            if(alcance$ref2 && dom$ref2 != "" && limpieza(dom$ref2) != "") {
              r_ref2 <- identifica_ref(dom$ref2, r_vld[k,])
              ad <- rbind(ad, r_ref2)
            }
            if(alcance$num && !is.na(dom$num)) {
              r_num <- identifica_num(dom$num, r_vld[k,])
              ad <- rbind(ad, r_num)
            }
            k <- k - 1
          }
        }
        j <- j - 1
      }
    }

    i <- i - 1
  }
  if(map)
    hace_mapa(ad)
  ad[c(6, 1, 2, 3, 4, 5)]
}

#-------------------------------------------------------------------
# Podar el árbol de decisión
# en cada nivel tener el más probable

# Ganancia de información:
# Una rama con entropía cero se convierte en hojas
# Si no es así, la rama debe seguir dividiéndose,
# para poder clasificar mejor sus nodos
# El algoritmo ID3 es recursivo en nodos,
# que no son hojas, hasta que se llegue a las hojas
# (resultado de la decisión)
podar <- function (Ts, map = FALSE) {
  # Separa por niveles
  T0 <- Ts[which(Ts$niv == 0),]
  T1 <- Ts[which(Ts$niv == 1),]
  T2 <- Ts[which(Ts$niv == 2),]
  T3 <- Ts[which(Ts$niv == 3),]
  T4 <- Ts[which(Ts$niv == 4),]
  T5 <- Ts[which(Ts$niv == 5),]
  # Elige la mejor opción de cada nivel
  if(length(T0$BM) > 0) {
    # @error: ningún argumento finito para min; retornando Inf
    M0 <- T0[which(T0$BM == min(T0$BM, na.rm = TRUE)),]
  } else {
    M0 <- NULL
  }
  if(length(T1$BM) > 0) {
    M1 <- T1[which(T1$BM == min(T1$BM, na.rm = TRUE)),]
  } else {
    M1 <- NULL
  }
  if(length(T2$BM) > 0) {
    M2 <- T2[which(T2$BM == min(T2$BM, na.rm = TRUE)),]
  } else {
    M2 <- NULL
  }
  if(length(T3$BM) > 0) {
    M3 <- T3[which(T3$BM == min(T3$BM, na.rm = TRUE)),]
  } else {
    M3 <- NULL
  }
  if(length(T4$BM) > 0) {
    M4 <- T4[which(T4$BM == min(T4$BM, na.rm = TRUE)),]
  } else {
    M4 <- NULL
  }
  if(length(T5$BM) > 0) {
    M5 <- T5[which(T5$BM == min(T5$BM, na.rm = TRUE)),]
  } else {
    M5 <- NULL
  }
  # Asegurar que se incluya la rama completa
  C0 <- unique(M0$cve)
  C2 <- unique(M2$cve)
  C4 <- unique(M4$cve)
  
  i <- length(C0)
  R02 <- NULL
  R04 <- NULL
  R05 <- NULL
  while(i > 0) { #@todo revisar con la homologación de claves
    R02 <- rbind(R02, T2[ which( T2$cve == C0[i] ),] )
    R04 <- rbind(R04, T4[ which( substr(T4$cve, 2, 7) == substr(C0[i], 1, 6) ),] )
    R05 <- rbind(R05, T5[ which( T5$cve == substr(C0[i], 1, 2) ),] )
    i <- i -1
  }

  i <- length(C2)
  R24 <- NULL
  R25 <- NULL
  while(i > 0) {
    R24 <- rbind(R04, T4[ which( substr(T4$cve, 2, 7) == substr(C2[i], 1, 6) ),] )
    R25 <- rbind(R05, T5[ which( T5$cve == substr(C2[i], 1, 2) ),] )
    i <- i -1
  }

  i <- length(C4)
  R45 <- NULL
  while(i > 0) {
    R45 <- rbind(R05, T5[ which( T5$cve == substr(C4[i], 1, 2) ),] )
    i <- i -1
  }
  # Pega la información de la mejor opción
  Us <- rbind(M0, M1, R02, R04, R05, M2, R24, R25, M3, M4, R45, M5)
  # Eliminar renglones sin coordenadas
  # Nota: Por esto es importante que todos los rasgos tengan asociada
  # una coordenada geográfica, al menos en la base de datos; principalmente
  # en el caso de las vialidades.
  Us <- Us[which(!is.na(Us$lat)),]
  Us <- Us[which(!is.na(Us$lon)),]

  # Elimina en nivel cero, si no hay ninguna probabilidad de éxito
  Us <- Us[which(!(Us$niv == 0 & Us$BM == 1)),]

  # Eliminar renglones repetidos
  Us <- unique(Us)
  
  if(map)
    hace_mapa(Us)
  Us
}

# Atomiza un árbol podado
# Obtiene la hoja más probable
atomizar <- function (Ts, map = FALSE) {
  T0 <- Ts[which(Ts$niv == 0),]
  T1 <- Ts[which(Ts$niv == 1),]
  T2 <- Ts[which(Ts$niv == 2),]
  T3 <- Ts[which(Ts$niv == 3),]
  T4 <- Ts[which(Ts$niv == 4),]
  T5 <- Ts[which(Ts$niv == 5),]
  Ta <- NULL
  if(length(T0$BM) > 0 && any(T0$BM < 0.002)) {
    # Número exterior exacto
    Ta <- T0[which(T0$BM == min(T0$BM, na.rm = TRUE)),]
    Ta[1,]$lat <- mean(Ta$lat)
    Ta[1,]$lon <- mean(Ta$lon)
    Ta <- Ta[1,]
  } else if(length(T1$BM) > 0 && any(T1$BM < 0.002)) {
    # Promedia las coordenadas si proporciona las dos entrecalles correctamente
    Ta <- T1[which(T1$BM == min(T1$BM, na.rm = TRUE)),]
    Ta[1,]$lat <- mean(Ta$lat)
    Ta[1,]$lon <- mean(Ta$lon)
    Ta <- Ta[1,]
  } else if(length(T2$BM) > 0 && any(T2$BM < 0.002)) { # Cuando hay una calle correcta
    if(length(T0$BM) > 0 && any(T0$BM < 0.12)) {
      # Promedia las coordenadas, si hay más de 1
      Ta <- T0[which(T0$BM == min(T0$BM, na.rm = TRUE)),]
      Ta[1,]$lat <- mean(Ta$lat)
      Ta[1,]$lon <- mean(Ta$lon)
      Ta <- Ta[1,]
    } else {
      # Proporciona la calle
      # [order(T2$cve),] último Ta[length(Ta),]
      Ta <- T2[which(T2$BM == min(T2$BM, na.rm = TRUE)),][1,]
    }
  } else if(length(T3$BM) > 0 && any(T3$BM < 0.002)) { # Cuando la colonia es correcta
    if(length(T2$BM) > 0 && any(T2$BM < 0.1)){
      if(length(T0$BM) > 0 && any(T0$BM < 0.1)) {
        # Promedia las coordenadas, si hay más de 1
        Ta <- T0[which(T0$BM == min(T0$BM, na.rm = TRUE)),]
        Ta[1,]$lat <- mean(Ta$lat)
        Ta[1,]$lon <- mean(Ta$lon)
        Ta <- Ta[1,]
      } else {
        Ta <- T2[which(T2$BM == min(T2$BM, na.rm = TRUE)),][1,]
      }
    } else {
      # Se queda con la colonia
      Ta <- T3[which(T3$BM == min(T3$BM, na.rm = TRUE)),][1,]
    }
  } else if(length(T4$BM) > 0 && any(T4$BM < 0.15)) { # Si ninguna de las anteriores.
    if(length(T2$BM) > 0 && any(T2$BM < 0.1)) {
      if(length(T0$BM) > 0 && any(T0$BM < 0.12)) {
        Ta <- T0[which(T0$BM == min(T0$BM, na.rm = TRUE)),]
        Ta[1,]$lat <- mean(Ta$lat)
        Ta[1,]$lon <- mean(Ta$lon)
        Ta <- Ta[1,]
      } else {
        Ta <- T2[which(T2$BM == min(T2$BM, na.rm = TRUE)),][1,]
      }
    } else if(length(T3$BM) > 0 && any(T3$BM < 0.1)) {
      Ta <- T3[which(T3$BM == min(T3$BM, na.rm = TRUE)),][1,]
    } else {
      Ta <- T4[which(T4$BM == min(T4$BM, na.rm = TRUE)),][1,]
    }
  }
  if(is.null(Ta)) {
    Ta <- data.frame(niv = -1, BM = 0, cve = 0, nombre = 0, lat = 0, lon = 0)
  } else if(map) {
    hace_mapa(Ta)
  }
  Ta
}

#-------------------------------------------------------------------
# Lee el archivo de trabajo y realiza la normalización de los datos
lee <- function(path, sheet = 1, iforder = TRUE, ifcompact = TRUE) {
  if(ifcompact) {
    require(openxlsx)
    matricula <- openxlsx::read.xlsx(path, sheet, rows = c(1:6))
  } else{
    require(readxl)
    matricula <- readxl::read_excel(path, sheet) #@warning: No lee documentos de libreoffice
  }

  # Normaliza la tabla para que contenga los campos requeridos
  alcance0 <<- list(mun = "mun" %in% colnames(matricula), loc = "loc" %in% colnames(matricula),
                    tsnt = "tsnt" %in% colnames(matricula), snt = "snt" %in% colnames(matricula), 
                    tvld = "tvld" %in% colnames(matricula), vld = "vld" %in% colnames(matricula),
                    num = "num" %in% colnames(matricula), int = "int" %in% colnames(matricula),
                    ref1 = "ref1" %in% colnames(matricula), ref2 = "ref2" %in% colnames(matricula),
                    CP = "CP" %in% colnames(matricula))

  # De la clave de municipio obtiene el nombre de municipio y lo asigna
  if(!alcance0$mun && "id_mun" %in% colnames(matricula)) {
    alcance0$mun <- TRUE
    ca <- mun.php()
    matricula$mun <- ""
    for(n in 1:nrow(matricula)) {
      if(substring(matricula[n, ]$id_mun, 1, 2) == "11") {
        matricula[n,]$mun <- as.character(ca[which(ca$cve == as.numeric(substring(matricula[n, ]$id_mun, 3, 5))), ]$nombre)
      }
    }
  }

  # De la clave de localidad obtiene el nombre de localidad
  if(!alcance0$loc && "id_loc" %in% colnames(matricula)) {
    alcance0$loc <- TRUE
    matricula$loc <- as.character(mapply(nloc.php, substring(matricula$id_loc, 3, 5), substring(matricula$id_loc, 6)))
    
    if(!alcance0$mun) {
      alcance0$mun <- TRUE
      ca <- mun.php()
      matricula$mun <- ""
      for(n in 1:nrow(matricula))
        if(substring(matricula[n, ]$id_mun, 1, 2) == "11")
          matricula[n,]$mun <- as.character(ca[which(ca$cve == as.numeric(k[n])), ]$nombre )
    }
  }
  
  # Completa con las columnas vacías, donde no se tenga información
  if(!alcance0$mun)
    return
  if(!alcance0$loc)
    matricula$loc <- ""
  if(!alcance0$snt)
    matricula$snt <- ""
  if(!alcance0$vld)
    matricula$vld <- ""
  if(!alcance0$num)
    matricula$num <- ""
  
  # Crea un índice, si no tiene
  if(!("n" %in% colnames(matricula))) {
    matricula$n <- c(1:nrow(matricula))
  }

  # Construye una base basado en las columnas con las que se cuentan
  # minimiza la carga de memoria durante el procesamiento
  if(ifcompact) {
    # Listado de variables disponibles
    vars <- c("n","mun","loc", "snt", "vld", "num")
    if(alcance0$tsnt)
      vars <- c(vars, "tsnt")
    if(alcance0$tvld)
      vars <- c(vars, "tvld")
    if(alcance0$int)
      vars <- c(vars, "int")
    if(alcance0$ref1)
      vars <- c(vars, "ref1")
    if(alcance0$ref2)
      vars <- c(vars, "ref2")
    if(alcance0$CP)
      vars <- c(vars, "CP")
    
    matricula <- matricula[,vars]
  }
  # Quita los renglones vacíos, basado en el campo municipio
  matricula <- matricula[which(!is.na(matricula$mun)),]
  matricula$mun <- limpieza(matricula$mun)
  matricula <- matricula[which(matricula$mun != ""),]
  
  # Remplaza los resultados NA por cadena vacía
  # Simplifica el código al no tener que hacer doble verificación
  matricula[is.na(matricula)] <- ""
  
  # Ordena por municipio y localidad para optimizar el cache inicial
  if(iforder) {
    matricula <- matricula[with(matricula, order(mun, loc)),]
  }
  return(matricula) 
}

#-------------------------------------------------------------------
# Ejecuta el procedimiento sobre el archivo de direcciones y lo
# guarda en un archivo separado. Incluye seguimiento del proceso.
# En este caso siempre se procesa en paralelo.
main <- function (path, sheet = 1, file, id = 0, paralelo = TRUE, seguimiento = TRUE) {
  # Almacena el Process ID
  if(seguimiento) pid.ldom.php(id, Sys.getpid())

  # Oculta los mensajes de alertas, habilitar para depuración
  assign("last.warning", NULL, envir = baseenv())
  options(show.error.messages = FALSE)
  map.is.visible <<- FALSE

  # Lee el archivo base de trabajo y lo estandariza
  padron <- lee(path, sheet, iforder = FALSE, ifcompact = FALSE)

  # Guarda padron
  require(feather)
  feather::write_feather(as.data.frame(padron), paste0(path, ".dat"))

  # Listado de variables disponibles
  vars <- c("n","mun","loc", "snt", "vld", "num")
  if(alcance0$tsnt)
    vars <- c(vars, "tsnt")
  if(alcance0$tvld)
    vars <- c(vars, "tvld")
  if(alcance0$int)
    vars <- c(vars, "int")
  if(alcance0$ref1)
    vars <- c(vars, "ref1")
  if(alcance0$ref2)
    vars <- c(vars, "ref2")
  if(alcance0$CP)
    vars <- c(vars, "CP")
  
  # Construye una base basado en las columnas con las que se cuentan
  # minimiza la carga de memoria durante el procesamiento
  matricula <- padron[,vars]
  rm(vars)

  # Reserva la memoria
  rm(padron)

  ll <- length(matricula$n)
  if(seguimiento) size.ldom.php(id, ll)

  # Procesamiento por lote en paralelo y secuencial
  require(plyr)
  require(parallel)
  ndC <- detectCores()
  if(paralelo && ll > ndC) {
    # Ordena por municipio y localidad para optimizar el cache inicial
    matricula <- matricula[with(matricula, order(mun, loc)),]
    
    require(doParallel)
    registerDoParallel(cores = max(ndC - 1, 1)) # Recomendado mínimo 2 procesadores
    options(sd_num_thread = 1) # Evita conflicto con el uso de procesadores
  } else {
    options(sd_num_thread = ndC)
    paralelo <- FALSE
  }
  rm(ndC)

  res <- ldply(1:ll, function(n) {
    # Procesa la dirección y obtiene la coordenada geográfica probable
    c <- try(atomizar(podar(identifica(matricula[n,]))), silent = TRUE)
    if(class(c) == "try-error") {
      c <- data.frame(niv=-2, BM=0, cve=0, nombre=0, lat=0, lon = 0)
    }
    # Reporta el avance a la base de datos
    if(seguimiento) avance.ldom.php(id)
    # Agrega el número de renglón
    c$n <-matricula[n,]$n
    return(c)
  }, .parallel = paralelo, .progress = "time")
  
  # Limpieza
  rm(matricula)
  
  # Cambia el campo 'nombre' por 'ntd', para no interferir con otro
  colnames(res)[4] <- "ntd"  

  # Carda padron
  padron <- NULL
  while (is.null(padron)) try(padron <- feather::read_feather(paste0(path, ".dat")), TRUE)
  
  # Guarda en el sentido en el que fue generado (orden lógico)
  # Intercambia el orden de las columnas
  padron <- merge(padron, res[c(7, 1:6)], by = "n")

  feather::write_feather(as.data.frame(padron), paste0(file, ".dat"))

  # Desprotege el archivo de solo lectura
  rm(padron)
  system(paste0( "rm -f ", path, ".dat"))

  # Reporta que se concluyó el proceso
  if(seguimiento) fin.ldom.php(id)

  #warnings()
  return(0)
}

# Referencias:
# Sean C. Anderson, plyr: Split-Apply-Combine for Mortals, 2013
# http://seananderson.ca/2013/12/01/plyr.html

# La estrategia genérica no es práctica.
# ¿Cuántos árboles de decisión distintos se pueden formar con n atributos booleanos?
# 2^(2^n), con n = 8, tenemos 2 ^ (225) = 1.157920892×10⁷⁷
# ¿Cuántos árboles de profundidad unitaria (capa de decisión)?
# 4n, con n = 8, tenemos 4 * (8) = 32
# Solución: La estrategia utilizada fue diseñar un árbol de decisión en papel, y programar
# cada nodo por separado.