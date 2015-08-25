#-------------------------------------------------------------------
# Ejemplo demostrativo de la potencialidad de localización
# automatizada de domicilios geográficos.
# 17 de agosto de 2015.

#-------------------------------------------------------------------
# Notas:
# Para su desarrollo se implementa una técnica de inteligencia
# artificial denominada árbol de decisión. 

#! Remover todos los objetos
rm(list=ls(all=TRUE))

#! Carga los procedimientos
source("qmaps.R")

# Primer ejemplo
dom <- list("mun"="Salamanca",
            "loc"="ZALAMANNCA",
            "snt"="AMPLIACION RINCONADA SAN JAVIER",
            "vld"="CALLE DE LA PARRóQUIA",
            "num"="123")
map.is.visible <- TRUE
r_mun <- identifica_mun(dom$mun)
r_loc <- identifica_loc(dom$loc, r_mun$cve, r_mun$BM)
r_snt <- identifica_snt(dom$snt, r_loc$cve, r_loc$BM)
r_vld <- identifica_vld(dom$vld, r_loc$cve, r_loc$BM)
r_num <- identifica_num(dom$num, r_vld[1,]$cve, r_vld[1,]$BM)

# Segundo ejemplo
dom <- list("mun"="Salamanca",
            "loc"="SALAMANCA",
            "snt"="AMPLIACION BELLAVISTA",
            "vld"="CALLE 2",
            "num"="12")
# Crea el arbol de desición
Tt <- identifica(dom, TRUE)
entriopia(Tt)
# Podar el arbol
Tt <- podar(Tt, TRUE)
entriopia(Tt)

# El procedimiento completo se realiza en dos horas.
rm(list=ls(all=TRUE))
source("qmaps.R")
#main(path = "SEGdemo.xlsx", file = "res.xlsx")

path = "dilectu/Salamanca_Sec.xlsx"
file = "dilectu/res.xlsx"
sheet = 1
require(readxl)
matricula <- read_excel(path, sheet)

n <- length(matricula$mun)
n <- sample(1:n, 1)
n <- 2462
res <- NULL
dom <-matricula[n,]
map <- TRUE

# Crea el arbol de desición
Tt <- identifica(dom, TRUE)
entriopia(Tt)

# Podar el arbol
Tt <- podar(Tt, TRUE)
entriopia(Tt)

# Atomiza el arbol
Tt <- atomizar(Tt, TRUE)
entriopia(Tt)

## Bibliografía
# Mitchell, T.M. Machine Learning, McGraw-Hill, 1997.
# Cap. 3: Decision tree learning
# Quinlan, Induction Decision Tree, 1979, 1986
# https://es.wikipedia.org/wiki/%C3%81rbol_de_decisi%C3%B3n_%28modelo_de_clasificaci%C3%B3n_ID3%29