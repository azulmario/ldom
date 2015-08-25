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

#! Archivo de trabajo
path = "dilectu/Salamanca_Sec.xlsx"
require(readxl)
matricula <- read_excel(path)

n <- 4016
#n <- 1653
#n <- 2462
#n <- sample(1:length(matricula$mun), 1)
dom <-matricula[n,]

#! Carga los procedimientos
source("qmaps.R")

# Primer ejemplo
dom <- matricula[1086,]
map.is.visible <- TRUE
r_mun <- identifica_mun(dom$mun)
r_loc <- identifica_loc(dom$loc, r_mun$cve, r_mun$BM)
r_snt <- identifica_snt(dom$snt, r_loc$cve, r_loc$BM)
r_vld <- identifica_vld(dom$vld, r_loc$cve, r_loc$BM)
r_num <- identifica_num(dom$num, r_vld[1,]$cve, r_vld[1,]$BM)

# Segundo ejemplo
dom <- matricula[1572,]
# Crea el arbol de desición
Tt <- identifica(dom, TRUE)
entriopia(Tt)
# Podar el arbol
Tt <- podar(Tt, TRUE)
entriopia(Tt)
# Atomiza el arbol
Tt <- atomizar(Tt, TRUE)
entriopia(Tt)

# Tercer ejemplo
# El procedimiento completo se realiza en dos horas.
rm(list=ls(all=TRUE))
source("qmaps.R")
main(path = "dilectu/Salamanca_Sec.xlsx", file = "dilectu/res.xlsx")


## Bibliografía
# Mitchell, T.M. Machine Learning, McGraw-Hill, 1997.
# Cap. 3: Decision tree learning
# Quinlan, Induction Decision Tree, 1979, 1986
# https://es.wikipedia.org/wiki/%C3%81rbol_de_decisi%C3%B3n_%28modelo_de_clasificaci%C3%B3n_ID3%29