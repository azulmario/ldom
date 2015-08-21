# Habilitar primero la tarjeta
#sudo prime-select nvidia
# 
# https://github.com/cran/gputools

library("gputools")
gpu.matmult <- function(n) {
  A <- matrix(runif(n * n), n ,n)
  B <- matrix(runif(n * n), n ,n)
  tic <- Sys.time()
  C <- A %*% B
  toc <- Sys.time()
  comp.time <- toc - tic
  cat("CPU: ", comp.time, "\n")
  tic <- Sys.time()
  C <- gpuMatMult(A, B)
  toc <- Sys.time()
  comp.time <- toc - tic
  cat("GPU: ", comp.time, "\n")
}

gpu.matmult(10)
gpu.matmult(100)
gpu.matmult(1000)
gpu.matmult(2000)

#gpuDist
N <- 1e3
m <- matrix(sample(100, size = N*N, replace = T), nrow = N)
system.time(dist(m))
system.time(gpuDist(m))



## Instalación
#sudo prime-select nvidia
#http://rstudio-pubs-static.s3.amazonaws.com/76831_0710fb114a704200886af1a4b8b8a020.html#/4
#sudo ln -s /usr/local/cuda/lib64/libcublas.so.7.0 /usr/lib/libcublas.so.7.0

## Pruebas de variables de entorno
#Sys.setenv(PATH="/usr/local/heroku/bin:/usr/local/cuda-7.0/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games")
#Sys.setenv(LD_LIBRARY_PATH="/usr/lib/R/lib:/usr/lib/x86_64-linux-gnu:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64/server:/usr/local/cuda-7.0/lib64:")
#Sys.setenv(CUDA_LIB_PATH="/usr/local/cuda-7.0/lib64:")
#Sys.setenv(CUDA_HOME="/usr/local/cuda-7.0")
#Sys.setenv(R_INC_PATH="/usr/lib/R/include:/usr/local/cuda-7.0/lib64:")
#Sys.setenv(R_LIBS_SITE="/usr/local/lib/R/site-library:/usr/lib/R/site-library:/usr/lib/R/library")
#Sys.unsetenv(LD_LIBRARY_PATH)
#Sys.unsetenv(CUDA_LIB_PATH)
#Sys.unsetenv(CUDA_HOME)
#Sys.unsetenv(R_INC_PATH)
#Sys.unsetenv(R_LIBS_SITE)
#library("gputools")
# Así estaba al inicio
#Sys.setenv(PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games")

