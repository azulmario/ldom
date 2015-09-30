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
path = "dilectu/Salamanca_Prim.xlsx"
require(readxl)
matricula <- read_excel(path)

n <- sample(1:length(matricula$mun), 1)
dom <-matricula[n,]

#! Carga los procedimientos
source("qmaps.R")

# Primer ejemplo
dom <- matricula[1065,]
map.is.visible <- TRUE
r_mun <- identifica_mun(dom$mun)
r_loc <- identifica_loc(dom$loc, r_mun$cve, r_mun$BM)
r_snt <- identifica_snt(dom$snt, r_loc$cve, r_loc$BM)
r_vld <- identifica_vld(dom$vld, r_loc$cve, r_loc$BM)

r_ref1 <- identifica_ref(dom$ref1, r_vld[1,]$cve, r_vld[1,]$BM)
r_ref2 <- identifica_ref(dom$ref2, r_vld[1,]$cve, r_vld[1,]$BM)

r_num <- identifica_num(dom$num, r_vld[1,]$cve, r_vld[1,]$BM)

# Segundo ejemplo
source("qmaps.R")
dom$vld <- "BLVD. JUAN JOSÉ TORRES LANDA 1701"
dom$num <- ""
dom$snt <- "PREDIO EL TLACUACHE"
dom$loc <- "LEON DE LOS ALDAMA"
dom$mun <- "León"

dom <- matricula[1151,]
# Crea el arbol de desición
Tt <- identifica(dom, TRUE)
entriopia(Tt)
mapa
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
main(path = "dilectu/Demo.xlsx", file = "dilectu/temp00.xlsx", paralelo = TRUE)

# Análisis estadístico de los resultados
require(readxl)
res <- read_excel("dilectu/res.xlsx")
hace_mapa(res)
hist(res$BM)
hist(res[which(res$niv == 0), ]$BM)
hist(res[which(res$niv == 1), ]$BM)
hist(res[which(res$niv == 3), ]$BM)
hist(res[which(res$niv == 4), ]$BM)

hist(res[which(res$niv == 0 & res$BM > 0), ]$BM)
hist(res[which(res$niv == 1 & res$BM > 0), ]$BM)
hist(res[which(res$niv == 3 & res$BM > 0), ]$BM)
hist(res[which(res$niv == 4 & res$BM > 0), ]$BM)

# Cuarto ejemplo
# Niños de prescolar a primaria.
rm(list=ls(all=TRUE))
source("qmaps.R")
main(path = "dilectu/Demo.xlsx", file = "dilectu/temp00.xlsx")

## Bibliografía
# Mitchell, T.M. Machine Learning, McGraw-Hill, 1997.
# Cap. 3: Decision tree learning
# Quinlan, Induction Decision Tree, 1979, 1986
# https://es.wikipedia.org/wiki/%C3%81rbol_de_decisi%C3%B3n_%28modelo_de_clasificaci%C3%B3n_ID3%29
# Fernando Caparrini, Arboles decision id3, 5 de enero de 2013
# http://es.slideshare.net/FernandoCaparrini/arboles-decision-id3

# Sin éxito
# 115  128  131  133  241  437  609  611  675  681  685  686  692  695  699  707  709  710  711  714  717  734  789  791  792  793  794  798
# 799  802  803  806  807  814  815  817  818  819  822  824  829  832  833  838  841  910  938  989 1100 1110 1132 1186 1523 1586 1604 1865
#2406 2408 2414 2533 2534 2537 2579 2596 2670 2780 2826 3086 3178 3611 3732 3949 3978 3979 3980 4240 5076 5275
# 587  799 1040 1476 1652 2009 2012 2333 2336 2338 2699 2968 3171 3622 4173 4282
#4307 4333 4336 4339 4367 4368 4373 4391

#Warning messages:
#1:13 In min(T0$BM, na.rm = TRUE) : ningún argumento finito para min; retornando Inf