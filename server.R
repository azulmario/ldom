library(shiny)
library(utils)
library(tools)
library(readxl)
source("qmaps.R")

options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input, output) {
  jj <- ""
  kk <- ""
  
  output$matricula <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #Renombrar
    jj <<- file.path("/srv/shiny-server/docs/out", inFile$name)
    system(paste('cp -f', inFile$datapath, jj))
    #Leer
    matricula <- lee(jj, input$sheet)

    data.frame(matricula[1:(min(10,length(matricula$n))),])
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('res-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      matricula <- read_excel(kk)
      write.csv(matricula, file, row.names = FALSE)
    }
  )

  ntext <- eventReactive(input$goButton, {
    options(show.error.messages = FALSE)
    kk <<- paste('/srv/shiny-server/docs/out/', 'r', Sys.time(), '.xlsx', sep='')
    opetus <- paste("/usr/bin/Rscript -e \"source('/srv/shiny-server/ldom/qmaps.R'); main(path = '",jj,"', sheet = ",input$sheet,", file = '",kk,"', paralelo = TRUE);\" --vanilla &", sep='') 
    system(opetus)
    options(show.error.messages = TRUE)
    paste("¡Espere a que termine el proceso! Al finalizar, utilice la carpeta de trabajo o el siguiente vínculo:")
  })

  output$nText <- renderText({
    ntext()
  })
})
