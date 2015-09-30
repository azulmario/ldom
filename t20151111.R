rm(list=ls(all=TRUE))
source("qmaps.R")
main(path = "dilectu/Salamanca_Sec.xlsx", file = "dilectu/Sec.xlsx") # Completado después de 6 h
main(path = "dilectu/Salamanca_Prim.xlsx", file = "dilectu/Prim.xlsx") # Completado después de 3 h

# Advertencias:
# - In min(T0$BM, na.rm = TRUE) : ningún argumento finito para min; retornando Inf
# - In `[<-.factor`(`*tmp*`, ri, value = c(8L, 2L, 3L, 7L,  ... : invalid factor level, NA generated
# - In identifica_ref(dom$ref1, r_vld[k, ]$cve, r_vld[k, ]$BM) : NAs introducidos por coerción

require(readxl)
res <- read_excel("dilectu/Sec.xlsx")
hace_mapa(res)
hist(res$BM)
hist(res$niv)
hist(res[which(res$niv == 0), ]$BM)
hist(res[which(res$niv == 1), ]$BM)
hist(res[which(res$niv == 2), ]$BM)
hist(res[which(res$niv == 3), ]$BM)
hist(res[which(res$niv == 4), ]$BM)

hist(res[which(res$niv == 0 & res$BM > 0), ]$BM)
hist(res[which(res$niv == 1 & res$BM > 0), ]$BM)
hist(res[which(res$niv == 2 & res$BM > 0), ]$BM)
hist(res[which(res$niv == 3 & res$BM > 0), ]$BM)
hist(res[which(res$niv == 4 & res$BM > 0), ]$BM)

n <- length(res$niv)
res$t <- -1
for(i in 1:n) {
  if(res[i,]$niv == 0 & res[i,]$BM == 0) res[i,]$t <- 0 else {
  if(res[i,]$niv == 0 & res[i,]$BM >  0) res[i,]$t <- 1 else {
  if(res[i,]$niv == 1 & res[i,]$BM == 0) res[i,]$t <- 2 else {
  if(res[i,]$niv == 1 & res[i,]$BM >  0) res[i,]$t <- 3 else {
  if(res[i,]$niv == 2 & res[i,]$BM == 0) res[i,]$t <- 4 else {
  if(res[i,]$niv == 2 & res[i,]$BM >  0) res[i,]$t <- 5 else {
  if(res[i,]$niv == 3 & res[i,]$BM == 0) res[i,]$t <- 6 else {
  if(res[i,]$niv == 3 & res[i,]$BM >  0) res[i,]$t <- 7 else {
  if(res[i,]$niv == 4 & res[i,]$BM == 0) res[i,]$t <- 8 else {
  if(res[i,]$niv == 4 & res[i,]$BM >  0) res[i,]$t <- 9
  }}}}}}}}}
}
hist(res$t)

mytable <- table(res$t)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of Species\n (with sample sizes)") 

