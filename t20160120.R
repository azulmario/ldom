rm(list=ls(all=TRUE))
source("qmaps.R")
main(path = "FALTAS.xlsx", file = "tempD.xlsx", paralelo = TRUE)

#@todo
# Cuando hay varias opciones a elegir, optar por la que sea más acertada

# /usr/bin/Rscript t20160120.R --vanilla &

path = "padron15.xls"
file = "tempA.xlsx"
paralelo = FALSE
sheet = 1

matricula <- lee(path, sheet)

n = 1
dom = matricula[n,]
map = TRUE

dom.mun = dom$mun

a <- identifica(dom)
b <- podar(a)
c <- atomizar(b)

# Ejecutar como:
# /usr/bin/Rscript t20151208.R --vanilla &
#
#  ~159 h

# n = 11
# elige una calle de otra localidad!