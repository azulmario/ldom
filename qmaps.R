library(stringr)    # Para la conversión de tipos numéricos a cadenas
library(stringdist) # Para las comparaciones de nombres topográficos
source("cadenas.R")   # Para la conversión de tipos numéricos a cadenas
source("conectadb.R") # Para obtener la información de los servidores

# Variables de control
# Para mostrar el mapa durante el diseño, inhabilitar en producción
map.is.visible <<- FALSE
mapa <<- NULL

#-------------------------------------------------------------------
# Mostrar el resultado en un mapa
# Útil para probar y depurar el código
hace_mapa <- function(r, r_zoom = NULL) {
  require(leaflet)
  mapa <<- leaflet(data = r) %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    addMarkers(~lon[], ~lat[], popup=~nombre)
  if(!is.null(r_zoom) ) {
    mapa <<- setView(mapa, lng = r$lon[1], lat = r$lat[1], zoom = r_zoom)
  }
  print(mapa)
}

# Para realizar las búsquedas de cada componente de la dirección
# Utiliza la métrica Jaro-Winkler.

#-------------------------------------------------------------------
# Primero identifica el municipio
identifica_mun <- function(dom.mun) {
  origen <- mun.php()
  origen$nombre <- limpieza(as.character(origen$nombre))
  destino <- limpieza(dom.mun)

  BM <- stringdistmatrix(origen$nombre, destino, method="jw", p = 0.1)
  BM <- cbind(BM,origen)
  BM <- BM[with(BM, order(BM)), ]

  r_mun <- BM[BM[,1] <= 0.05+min(BM[,1]),]
  m <- mapply(str_pad, r_mun$cve_mun, 3, pad = "0")
  d <- mapply(gloc.php, m)
  lat <- as.numeric(d[1,])
  lon <- as.numeric(d[2,])
  r_mun <- data.frame(r_mun, lat, lon)  
  colnames(r_mun)[2] <- "cve"
  if(map.is.visible) {
    hace_mapa(r_mun, 11)
  }
  r_mun$niv <- 5
  r_mun
}

#-------------------------------------------------------------------
# Segundo identifica la localidad
# Nota: Considerar permuta con colonia, principalmente en zona rural
identifica_loc <- function(dom.loc, r_mun.cve_mun, r_mun.BM) {
  origen <- loc.php(m = str_pad(r_mun.cve_mun, 3, pad = "0"))
  if(length(origen$nombre) > 0  && ! is.na(dom.loc)) {
    origen$nombre <- limpieza(as.character(origen$nombre))
    destino = limpieza(dom.loc)

    BM <- stringdistmatrix(origen$nombre, destino, method="jw", p = 0.1)
    BM <- cbind(BM,origen)
    BM <- BM[with(BM, order(BM)), ]

    r_loc <- BM[BM[,1] <= 0.05+min(BM[,1]),]
    m <- mapply(str_pad, r_mun.cve_mun, 3, pad = "0") # str_pad(r_mun.cve_mun, 3, pad = "0")
    l <- mapply(str_pad, r_loc$cve_loc, 4, pad = "0") # str_pad(r_loc.cve_loc, 4, pad = "0"))
    d <- mapply(gloc.php, m, l)
    lat <- as.numeric(d[1,])
    lon <- as.numeric(d[2,])
    r_loc <- data.frame(r_loc, lat, lon)  
    colnames(r_loc)[2] <- "cve"
    colnames(r_loc)[3] <- "nombre"
    if(map.is.visible & length(lat) > 0) {
      hace_mapa(r_loc, 13)
    }
    r_loc$niv <- 4
    r_loc$BM <- 1.0 - (1.0 - r_loc$BM) * (1.0 - r_mun.BM)
    r_loc$cve <- paste (m, l, sep = "")
    r_loc
  } else {
    NULL
  }
}

#-------------------------------------------------------------------
# Tercero identifica la colonia
identifica_snt <- function(dom.snt, r_loc.cve_loc, r_loc.BM) {
  origen <- col.php(m = substr(r_loc.cve_loc, 1, 3), l = substr(r_loc.cve_loc, 4, 7))
  destino = limpieza(dom.snt)

  if(length(origen$nom_asen) > 0  && ! is.na(dom.snt) && destino != "" && destino != "." && destino != "..") {
    origen$nom_asen <- limpieza(as.character(origen$nom_asen))
  
    BM <- stringdistmatrix(origen$nom_asen, destino, method="jw", p = 0.1)
    BM <- cbind(BM,origen)
    BM <- BM[with(BM, order(BM)), ]

    r_snt <- BM[BM[,1] <= 0.05+min(BM[,1]),]
    d <- sapply(mapply(str_pad, as.character(r_snt$cve_asen), 4, pad = "0"), gcol.php)
    lat <- as.numeric(d[1,])
    lon <- as.numeric(d[2,])
    r_snt <- data.frame(r_snt, lat, lon)
    colnames(r_snt)[2] <- "cve"
    colnames(r_snt)[3] <- "nombre"
    if(map.is.visible & length(lat) > 0) {
      hace_mapa(r_snt, 15)
    }
    r_snt$niv <- 3
    r_snt$BM <- 1.0 - (1.0 - r_snt$BM) * (1.0 - r_loc.BM)
    r_snt
  } else {
    NULL
  }
}

#-------------------------------------------------------------------
# Cuarto identifica la vialidad
identifica_vld <- function(dom.vld, r_loc.cve_loc, r_loc.BM) {
  origen <- cal.php(m = substr(r_loc.cve_loc, 1, 3), l = substr(r_loc.cve_loc, 4, 7))

  destino = limpieza(dom.vld)
  if(length(origen$nom_via) > 0 && ! is.na(dom.vld) && destino != "" && destino != "." && destino != ".." && destino != "...") {
    origen$nom_via <- limpieza(as.character(origen$nom_via))

    BM <- stringdistmatrix(origen$nom_via, destino, method="jw", p = 0.1)
    BM <- cbind(BM,origen)
    BM <- BM[with(BM, order(BM)), ]

    r_vld <- BM[BM[,1] <= 0.05+min(BM[,1]),]
    d <- sapply(r_vld$cve_via, gcal.php)
    lat <- as.numeric(d[1,])
    lon <- as.numeric(d[2,])
    r_vld <- data.frame(r_vld, lat, lon)
    colnames(r_vld)[2] <- "cve"
    colnames(r_vld)[3] <- "nombre"
    if(map.is.visible & length(lat) > 0) {
      hace_mapa(r_vld, 17)
    }
    r_vld$niv <- 2
    r_vld$BM <- 1.0 - (1.0 - r_vld$BM) * (1.0 - r_loc.BM)
    r_vld
  } else {
    NULL
  }
}

#-------------------------------------------------------------------
# Quinto identifica la entrecalle
identifica_ref <- function(dom.ref, r_vld.cve_via, r_vld.BM) {
  origen <- ecal.php(c = r_vld.cve_via)

  destino = limpieza(dom.ref)
  if(length(origen$nom_via) > 0 && ! is.na(dom.ref) && destino != "" && destino != "." && destino != ".." && destino != "...") {
    origen$nom_via <- limpieza(as.character(origen$nom_via))

    BM <- stringdistmatrix(origen$nom_via, destino, method="jw", p = 0.1)
    BM <- cbind(BM,origen)
    BM <- BM[with(BM, order(BM)), ]

    r_ref <- BM[BM[,1] <= 0.05+min(BM[,1]),]
    d <- sapply(r_ref$cve, gecal.php, e = r_vld.cve_via) # Obtiene las coordenadas de la esquina
    lat <- as.numeric(d[1,])
    lon <- as.numeric(d[2,])
    r_ref <- data.frame(r_ref, lat, lon)
    colnames(r_ref)[2] <- "cve"
    colnames(r_ref)[3] <- "nombre"
    if(map.is.visible & length(lat) > 0) {
      hace_mapa(r_ref, 17)
    }
    r_ref$niv <- 1
    r_ref$BM <- 1.0 - (1.0 - r_ref$BM) * (1.0 - r_vld.BM)
    r_ref
  } else {
    NULL
  }
}

#-------------------------------------------------------------------
# Sexto identifica el número exterior.
# Cumple con el requisito de pintar varios puntos.
# Se cambia la comparación string por numeric.
identifica_num <- function (dom.num, r_vld.cve_via, r_vld.BM) {
  origen <- num.php (c = r_vld.cve_via)
  destino = as.numeric(limpieza0(dom.num))
  if(length(origen$num) > 0 && ! is.na(dom.num) && ! is.na(destino)) {
    origen.num <- as.numeric(limpieza0(as.character(origen$num)))

    BM <- 2*pnorm(sqrt(2)*(abs(origen.num-destino)/50))-1
    BM <- cbind(BM, origen)
    BM <- BM[with(BM, order(BM)), ]

    r_num <- BM[BM[,1] <= 0.05+min(BM[,1]),]
    r_num <- cbind(r_vld.cve_via, r_num)
    r_num$num <- as.character(r_num$num)
    colnames(r_num)[1] <- "cve"
    colnames(r_num)[5] <- "nombre"
    if(map.is.visible & length(r_num$lat) > 0) {
      hace_mapa(r_num, 18)
    }
    r_num$niv <- 0
    r_num$BM <- 1.0 - (1.0 - r_num$BM) * (1.0 - r_vld.BM)
    r_num[c(2, 1, 5, 3, 4, 6)]
    } else {
    NULL
  }
}

#-------------------------------------------------------------------
# Identifica el arbol de desición

# Método ID3 (Induction Decision Tree [Quinlan, 1979, 1986])
# Técnica de aprendizaje automático
# Inducción de árboles de decisión
# Estrategia top-down

# Es un algoritmo voraz para la construcción automática de árboles
# de decisión, que selecciona en cada paso el mejor atributo.
# El mejor es el más discriminante (potencialmente más útil).

# El proceso de construcción es iterativo:
# 1- Se selecciona un subconjunto de ejemplos del conjunto
#    disponible para el nodo i (nivel geográfico).
# 2- Se construye (induce) el árbol de decisión que permita discriminar
#    el conjunto de ejemplos para el siguiente nodo i+1 de decisión.

# Seleccionar en cada paso el atributo que discrimina más, así
# permite reducir el tamaño del árbol de decisión.
# La selección se hace maximizando una cierta función G, que representa
# la ganancia de información.

# Entropía (aleatoriedad del árbol)
#   E = sum -p log_2(p)
# El valor p es la probabilidad de éxito
entriopia <- function (Tt) {
  sum((Tt$BM - 1)*log2(1 - Tt$BM))
}

# Esquema de trabajo con listas tratadas como árboles de decisiones
identifica <- function (dom, map = FALSE) {
  # Identifica las abreviaciones y las sustituye
  dom$mun <- abrev_loc(dom$mun)
  dom$loc <- abrev_loc(dom$loc)
  dom$snt <- abrev_snt(dom$snt)
  dom$vld <- abrev_vld(dom$vld)
  # Si no hay número exterior, verifica si no está incluido en la vialidad,
  # cuando se especifica sin número no aplica.
  if(!"num" %in% colnames(dom) || (limpieza0(dom$num) == "" && !numero_SN(dom$num))) {
    q <- calle_partir(dom$vld)
    if(!is.na(q[1])) {
      dom$vld <- q[1]
      dom$num <- q[2]
    }
  }

  r_mun <- identifica_mun(dom$mun)
  ad <- r_mun
  i <- length(r_mun$cve)
  while(i > 0) {
    r_loc <- identifica_loc(dom$loc, r_mun[i,]$cve, r_mun[i,]$BM)
    ad <- rbind(ad, r_loc)
    j <- length(r_loc$cve)
    while(j > 0) {
      dom.snt <- dom$snt
      r_snt <- identifica_snt(dom.snt, r_loc[j,]$cve, r_loc[j,]$BM)
      ad <- rbind(ad, r_snt)
      if("tsnt" %in% colnames(dom)) {
        dom.snt <- paste(dom$tsnt, dom.snt)
      }
      r_snt <- identifica_snt(dom.snt, r_loc[j,]$cve, r_loc[j,]$BM)
      ad <- rbind(ad, r_snt)

      r_vld <- identifica_vld(dom$vld, r_loc[j,]$cve, r_loc[j,]$BM)
      ad <- rbind(ad, r_vld)
      k <- length(r_vld$cve)
      while(k > 0) {
        if("ref1" %in% colnames(dom)) {
          r_ref1 <- identifica_ref(dom$ref1, r_vld[k,]$cve, r_vld[k,]$BM)
          ad <- rbind(ad, r_ref1)
        }
        if("ref2" %in% colnames(dom)) {
          r_ref2 <- identifica_ref(dom$ref2, r_vld[k,]$cve, r_vld[k,]$BM)
          ad <- rbind(ad, r_ref2)
        }
        if("num" %in% colnames(dom)) {
          r_num <- identifica_num(dom$num, r_vld[k,]$cve, r_vld[k,]$BM)
          ad <- rbind(ad, r_num)
        }
        k <- k - 1
      }
      
      #
      # Alternativas de localidad en misma zona conurbada
      # puede alterar el municipio
      co <- conurbación(r_loc[j,]$cve)
      # Agregar a la lista tales municipios
      ad <- rbind(ad, co)

      cj <- length(co$cve)
      while(cj > 0) {
        dom.snt <- dom$snt
        r_snt <- identifica_snt(dom.snt, r_loc[j,]$cve, r_loc[j,]$BM)
        ad <- rbind(ad, r_snt)
        if("tsnt" %in% colnames(dom)) {
          dom.snt <- paste(dom$tsnt, dom.snt)
        }
        r_snt <- identifica_snt(dom.snt, co[cj,]$cve, r_loc[j,]$BM)
        ad <- rbind(ad, r_snt)
        r_vld <- identifica_vld(dom$vld, co[cj,]$cve, r_loc[j,]$BM)
        ad <- rbind(ad, r_vld)
        k <- length(r_vld$cve)
        while(k > 0) {
          if("ref1" %in% colnames(dom)) {
            r_ref1 <- identifica_ref(dom$ref1, r_vld[k,]$cve, r_vld[k,]$BM)
            ad <- rbind(ad, r_ref1)
          }
          if("ref2" %in% colnames(dom)) {
            r_ref2 <- identifica_ref(dom$ref2, r_vld[k,]$cve, r_vld[k,]$BM)
            ad <- rbind(ad, r_ref2)
          }
          if("num" %in% colnames(dom)) {
            r_num <- identifica_num(dom$num, r_vld[k,]$cve, r_vld[k,]$BM)
            ad <- rbind(ad, r_num)
          }
          k <- k - 1
        }
        cj <- cj - 1
      }
      # Continúa ...
      j <- j - 1
    }
    # Intercambia localidad y colonia,
    # solo sí son de nombre distinto.
    destino = limpieza(dom$snt)
    if (! is.na(dom$snt) && destino != "" && destino != "." && destino != ".." &&
        destino != limpieza(dom$loc) ) {
      r_loc <- identifica_loc(dom$snt, r_mun[i,]$cve, r_mun[i,]$BM)
      ad <- rbind(ad, r_loc)
      j <- length(r_loc$cve)
      while(j > 0) {
        r_snt <- identifica_snt(dom$loc, r_loc[j,]$cve, r_loc[j,]$BM)
        ad <- rbind(ad, r_snt)
        r_vld <- identifica_vld(dom$vld, r_loc[j,]$cve, r_loc[j,]$BM)
        ad <- rbind(ad, r_vld)
        k <- length(r_vld$cve)
        while(k > 0) {
          if("ref1" %in% colnames(dom)) {
            r_ref1 <- identifica_ref(dom$ref1, r_vld[k,]$cve, r_vld[k,]$BM)
            ad <- rbind(ad, r_ref1)
          }
          if("ref2" %in% colnames(dom)) {
            r_ref2 <- identifica_ref(dom$ref2, r_vld[k,]$cve, r_vld[k,]$BM)
            ad <- rbind(ad, r_ref2)
          }
          if("num" %in% colnames(dom)) {
            r_num <- identifica_num(dom$num, r_vld[k,]$cve, r_vld[k,]$BM)
            ad <- rbind(ad, r_num)
          }
          k <- k - 1
        }
        #
        # Alternativas de localidad en misma zona conurbada
        # puede alterar el municipio
        co <- conurbación(r_loc[j,]$cve) 
        # Agregar a la lista tales municipios
        ad <- rbind(ad, co)
        
        cj <- length(co$cve)
        while(cj > 0) {
          dom.snt <- dom$snt
          r_snt <- identifica_snt(dom.snt, r_loc[j,]$cve, r_loc[j,]$BM)
          ad <- rbind(ad, r_snt)
          if("tsnt" %in% colnames(dom)) {
            dom.snt <- paste(dom$tsnt, dom.snt)
          }
          r_snt <- identifica_snt(dom.snt, co[cj,]$cve, r_loc[j,]$BM)
          ad <- rbind(ad, r_snt)
          r_vld <- identifica_vld(dom$vld, co[cj,]$cve, r_loc[j,]$BM)
          ad <- rbind(ad, r_vld)
          k <- length(r_vld$cve)
          while(k > 0) {
            if("ref1" %in% colnames(dom)) {
              r_ref1 <- identifica_ref(dom$ref1, r_vld[k,]$cve, r_vld[k,]$BM)
              ad <- rbind(ad, r_ref1)
            }
            if("ref2" %in% colnames(dom)) {
              r_ref2 <- identifica_ref(dom$ref2, r_vld[k,]$cve, r_vld[k,]$BM)
              ad <- rbind(ad, r_ref2)
            }
            if("num" %in% colnames(dom)) {
              r_num <- identifica_num(dom$num, r_vld[k,]$cve, r_vld[k,]$BM)
              ad <- rbind(ad, r_num)
            }
            k <- k - 1
          }
          cj <- cj - 1
        }
        #
        j <- j - 1
      }
    }
    #
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
# Una rama con entriopía cero se convierte en hojas
# Si no es así, la rama debe seguir dividiéndose,
# para poder clasificar mejor sus nodos
# El algoritmo ID3 se ejecuta recursivamente en nodos
# que no son hojas, hasta que se llegue a nodos - hoja
# (resultado de la decisión)
podar <- function (Ts, map = FALSE) {
  # Separa por niveles
  T0 <- Ts[which(Ts$niv == 0), ]
  T1 <- Ts[which(Ts$niv == 1), ]
  T2 <- Ts[which(Ts$niv == 2), ]
  T3 <- Ts[which(Ts$niv == 3), ]
  T4 <- Ts[which(Ts$niv == 4), ]
  T5 <- Ts[which(Ts$niv == 5), ]
  # Elige la mejor opción de cada nivel
  if(length(T0$BM) > 0) {
    # @error: ningún argumento finito para min; retornando Inf
    M0 <- T0[which(T0$BM == min(T0$BM, na.rm = TRUE)), ]
  } else {
    M0 <- NULL
  }
  if(length(T1$BM) > 0) {
    M1 <- T1[which(T1$BM == min(T1$BM, na.rm = TRUE)), ]
  } else {
    M1 <- NULL
  }
  if(length(T2$BM) > 0) {
    M2 <- T2[which(T2$BM == min(T2$BM, na.rm = TRUE)), ]
  } else {
    M2 <- NULL
  }
  if(length(T3$BM) > 0) {
    M3 <- T3[which(T3$BM == min(T3$BM, na.rm = TRUE)), ]
  } else {
    M3 <- NULL
  }
  if(length(T4$BM) > 0) {
    M4 <- T4[which(T4$BM == min(T4$BM, na.rm = TRUE)), ]
  } else {
    M4 <- NULL
  }
  if(length(T5$BM) > 0) {
    M5 <- T5[which(T5$BM == min(T5$BM, na.rm = TRUE)), ]
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
  while(i > 0) {
    R02 <- rbind(R02, T2[ which( T2$cve == C0[i] ), ] )
    R04 <- rbind(R04, T4[ which( substr(T4$cve, 2, 7) == substr(C0[i], 1, 6) ), ] )
    R05 <- rbind(R05, T5[ which( T5$cve == substr(C0[i], 1, 2) ), ] )
    i <- i -1
  }

  i <- length(C2)
  R24 <- NULL
  R25 <- NULL
  while(i > 0) {
    R24 <- rbind(R04, T4[ which( substr(T4$cve, 2, 7) == substr(C2[i], 1, 6) ), ] )
    R25 <- rbind(R05, T5[ which( T5$cve == substr(C2[i], 1, 2) ), ] )
    i <- i -1
  }

  i <- length(C4)
  R45 <- NULL
  while(i > 0) {
    R45 <- rbind(R05, T5[ which( T5$cve == substr(C4[i], 1, 2) ), ] )
    i <- i -1
  }
  # Pega la información de la mejor opción
  Us <- rbind(M0, M1, R02, R04, R05, M2, R24, R25, M3, M4, R45, M5)
  # Eliminar renglones sin coordenadas
  Us <- Us[which(! is.na(Us$lat)), ]
  Us <- Us[which(! is.na(Us$lon)), ]

  # Elimina en nivel cero, si no hay ninguna probabilidad de éxito
  Us <- Us[which(!(Us$niv == 0 & Us$BM == 1)), ]

  # Eliminar renglones repetidos 
  Us <- unique(Us)
  
  if(map)
    hace_mapa(Us)
  Us
}

# Atomiza un árbol podado
# Obtiene la hoja más probable
atomizar <- function (Ts, map = FALSE) {
  T0 <- Ts[which(Ts$niv == 0), ]
  T1 <- Ts[which(Ts$niv == 1), ]
  T2 <- Ts[which(Ts$niv == 2), ]
  T3 <- Ts[which(Ts$niv == 3), ]
  T4 <- Ts[which(Ts$niv == 4), ]
  T5 <- Ts[which(Ts$niv == 5), ]
  Ta <- NULL
  if(length(T0$BM) > 0 && T0$BM == 0) {
    Ta <- T0[which(T0$BM == 0), ][1, ]
  } else if(length(T1$BM) > 0 && T1$BM == 0) {
    # Promedia las coordenadas si proporciona las dos entrecalles correctamente
    Ta <- T1[which(T1$BM == 0), ]
    Ta[1,]$lat <- mean(Ta$lat)
    Ta[1,]$lon <- mean(Ta$lon)
    Ta <- Ta[1,]
    
  } else if(length(T2$BM) > 0 && T2$BM == 0) {
    if(length(T0$BM) > 0 && T0$BM < 0.12) {
      # Promedia las coordenadas si hay más de 1
      Ta <- T0[which(T0$BM < 0.12), ]
      Ta[1,]$lat <- mean(Ta$lat)
      Ta[1,]$lon <- mean(Ta$lon)
      Ta <- Ta[1,]
    } else {
      Ta <- T2[which(T2$BM == 0), ][1, ]
    }
  } else if(length(T3$BM) > 0 && T3$BM == 0) {
    Ta <- T3[which(T3$BM == 0), ][1, ]
  } else if(length(T4$BM) > 0 && T4$BM < 0.1) {
    if(length(T2$BM) > 0 && T2$BM < 0.1) {
      if(length(T0$BM) > 0 && T0$BM < 0.12) {
        Ta <- T0[which(T0$BM < 0.12), ]
        Ta[1,]$lat <- mean(Ta$lat)
        Ta[1,]$lon <- mean(Ta$lon)
        Ta <- Ta[1,]
      } else {
        Ta <- T2[which(T2$BM < 0.1), ][1, ]
      }
    } else if(length(T3$BM) > 0 && T3$BM < 0.1) {
      Ta <- T3[which(T3$BM < 0.1), ][1, ]
    } else {
      Ta <- T4[which(T4$BM < 0.1), ][1, ]
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
# Ejecuta el procedimiento sobre el archivo de direcciones y lo
# guarda en un archivo separado.
main <- function (path, sheet = 1, file, paralelo = FALSE) {
  map.is.visible <<- FALSE
  # Procesamiento por lote
  require(readxl)
  matricula <- read_excel(path, sheet)

  if(paralelo) {
    require(doParallel)
    registerDoParallel(cores = (detectCores() - 1))
  }
  require(plyr)
  res <- ldply(1:length(matricula$mun), function(n) {
    # Procesa la dirección y obtiene la coordenada geográfica probable
    c <- atomizar(podar(identifica(matricula[n,])))
    c$n <- n #Agrega el número de renglón
    c
  }, .parallel = paralelo, .progress = "time")

  # Guarda en el sentido inverso en el que fue generado (orden lógico)
  # Intercambia el orden de las columnas
  require(openxlsx)
  write.xlsx(res[c(7, 1:6)], file)
}

#
# http://seananderson.ca/2013/12/01/plyr.html
