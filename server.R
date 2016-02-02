library(shiny)
library(readxl)
library(utils)
library(tools)
library(plyr)

source("qmaps.R")
matricula <- NULL
res <- NULL

options(shiny.maxRequestSize = 9*1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$matricula <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #Renombrar
    #system
    jj <- file.path(dirname(inFile$datapath), inFile$name)
    kk <- system(paste('mv', inFile$datapath, jj))
    #Leer
    matricula <<- read_excel(path = jj, sheet = input$sheet)
    
    #Borrar
    system(paste('rm', jj))
  
    data.frame(matricula[1:10,])
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('res-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(merge(matricula, res[c(7, 1:6)]), file, row.names = FALSE)
    }
  )

  ntext <- eventReactive(input$goButton, {
    options(show.error.messages = FALSE)
    numera <- "n" %in% colnames(matricula)
    res <<- ldply(1:length(matricula$mun), function(n) {
      # Procesa la dirección y obtiene la coordenada geográfica probable
      c <- try(atomizar(podar(identifica(matricula[n,]))), silent = TRUE)
      if(class(c) == "try-error") {
        c <- data.frame(niv=-2, BM=0, cve=0, nombre=0, lat=0, lon = 0)
      }
      #Agrega el número de renglón
      c$n <-ifelse(numera, matricula[n,]$n, n)
      c
    }, .parallel = FALSE, .progress = "time")
    options(show.error.messages = TRUE)
    "Terminado!"
  })
  
  output$nText <- renderText({
    ntext()
  })
})
