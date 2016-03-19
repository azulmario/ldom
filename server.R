library(shiny)
library(utils)
library(tools)
library(readxl)
source("qmaps.R")

options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input, output) {
  jj <- ""
  kk <- ""
  ll <- 0
  maksimi <- 3
  
  output$matricula <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)

    #Renombrar
    jj <<- file.path("/srv/shiny-server/docs/in", inFile$name)
    system(paste('cp -f', inFile$datapath, jj))
    #Número de pestañas
    sheets <- readxl::excel_sheets(jj)
    maksimi <<- length(sheets)

    output$slider <<- renderUI({
      sliderInput("sheet", 
                  label = p("Hoja a leer, la posición de la hoja. Por defecto es la primera hoja."), 
                  value = input$sheet, min = 1, max = maksimi, step = 1)
    })
    
    #Leer
    matricula <- lee(jj, input$sheet)
    ll <<- length(matricula$n)
    if(ll == 1) {
      return(data.frame(matricula))
    } 
    data.frame(matricula[c(1:min(9,ll-1),ll),])
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
    kk <<- paste('/srv/shiny-server/docs/out/', 'r', Sys.time(), '.xlsx', sep='')
    id <- ldom.php(file_in = jj, sheet = input$sheet, file_out = kk, size = ll)
    opetus <- paste("/usr/bin/Rscript -e \"source('/srv/shiny-server/ldom/qmaps.R'); main2(path = '",jj,"', sheet = ",input$sheet,", file = '",kk,"', id = ",id,");\" --vanilla &", sep='') 
    system(opetus)
    paste("¡Espere a que termine el proceso! Al finalizar, utilice la bitácora de trabajo de trabajo. Número de trabajo:", id)
  })

  output$nText <- renderText({
    ntext()
  })
  
})
