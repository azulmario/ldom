library(shiny)
library(DT)
library(xtable)
library(utils)
library(tools)
library(readxl)
source("qmaps.R")

options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input, output, session) {
  jj <- ""
  ll <- 0
  libre <- 0
  tam <- 0
  tamc <- 0
  tam0 <- 0
  tamp <- 0
  tamZ <- 1
  progress <- NULL

  output$matricula <- renderTable({
    ll <<- 0
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)

    #Renombrar
    jj <<- file.path("/srv/shiny-server/docs/in", inFile$name)
    system(paste('cp -f', inFile$datapath, jj))
    #Número de pestañas
    sheets <- readxl::excel_sheets(jj)
    maksimi <- length(sheets)
    if(maksimi > 1) {
      output$slider <<- renderUI({
        sliderInput("sheet", 
                    label = p("Elija la posición de la hoja a leer:"), 
                    value = input$sheet, min = 1, max = maksimi, step = 1)
      })
    } else {
      output$slider <<- renderUI({
        numericInput("sheet", 
                    label = p("El archivo solo contiene una hoja."), 
                    value = input$sheet, min = 1, max = 1)
      })
    }
    #Leer
    matricula <- lee(jj, input$sheet)
    ll <<- length(matricula$n)
    if(ll == 1) {
      return(xtable(matricula))
    }
    xtable(matricula[c(1:min(5,ll-1),ll),])
  },
  caption = "Vista previa de los datos:",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  ntext <- eventReactive(input$goButton, {
    ldom <- lee.ldom.php()
    if(length(ldom$size) == 0 || !is.na(ldom[1,]$time_end) ) {
      libre <<- 0      
    } else {
      libre <<- 1
    }
    if(jj == "") {
      return("Aún no ha subido el archivo de trabajo.")
    }
    if(ll == 0) {
      return("Lote sin direcciones por procesar.")
    }
    if(libre) {
      return("Servidor ocupado.")
    }

    kk <- paste('/srv/shiny-server/docs/out/r', format(Sys.time()), '.xlsx', sep='')
    id <- ldom.php(file_in = jj, sheet = input$sheet, file_out = kk, size = ll)
    opetus <- paste("/usr/bin/Rscript -e \"source('/srv/shiny-server/ldom/qmaps.R'); main2(path = '",jj,"', sheet = ",input$sheet,", file = '",kk,"', id = ",id,");\" --vanilla &", sep='') 
    system(opetus)
    libre <<- 1
    paste("¡Espere a que termine el proceso! Al finalizar, busqué en la bitácora de trabajo el número ", id, ".", sep ="")
  })

  output$nText <- renderText({
    ntext()
  })

  output$currentTime <- renderText({
    invalidateLater(as.integer(1000), session)
    ldom <- lee.ldom.php()
    imprime <- 0
    if(length(ldom$biased) > 0) {
      if(is.na(ldom[1,]$time_end)) {
        if(tamp == 0) {
          tamp <<- 1
          progress <<- shiny::Progress$new(min=0, max=ldom[1,]$size)
          progress$set(value = ldom[1,]$biased+1, message = 'Búsquedas en proceso', detail = paste(ldom[1,]$biased, '/', ldom[1,]$size, sep=''))
          imprime <- 1
        }
        libre <<- 1
      } else {
        if(tamp == 1) {
          tamp <<- 0
          progress$close()
          imprime <- 1
        }
        libre <<- 0
      }
      if(tamc != length(ldom$id)) {
        tamc  <<- length(ldom$id)
        imprime <- 1
      }
      if(tam != ldom[1,]$size) {
        tam  <<- ldom[1,]$size
        imprime <- 1
      }
      if(tam0 != ldom[1,]$biased) {
        tam0  <<- ldom[1,]$biased
        if(tamp) {
          progress$set(value = ldom[1,]$biased+1, message = 'Búsquedas en proceso', detail = paste(ldom[1,]$biased, '/', ldom[1,]$size, sep=''))
        }
      }
      tamZ <<- 1
    } else {
      if(tamZ) {
        tamZ <<- 0
        imprime <- 1
      }
    }

    if(imprime) {
      # Personaliza tabla
      row.names(ldom) <- ldom[,1] # prefix = "Núm." 
      ldom[c(1,2,5,9)] <- list(NULL)
      names(ldom)[1] <- "Archivo"
      names(ldom)[2] <- "Hoja"
      names(ldom)[3] <- "Inició"
      names(ldom)[4] <- "Terminó"
      names(ldom)[5] <- "Tamaño"
      ldom[,1] <- str_sub(ldom[,1], start = 27)
      ldom[,3] <- format(ldom[,3])
      ldom[,4] <- format(ldom[,4])
      output$ldom <- DT::renderDataTable(DT::datatable({ldom}, selection = 'single', options = list(
        language = list(
          sProcessing='Por favor, espere...',
          sLengthMenu='Mostrar _MENU_ registros',
          sZeroRecords='No hay coincidencias de registros',
          sEmptyTable= 'Bitácora de trabajo vacía',
          sInfo='Mostrando _START_ a _END_ de _TOTAL_ registros',
          sInfoEmpty='Ningún registro mostrado',
          sInfoFiltered='(filtrado de _MAX_ registros)',
          sInfoPostFix='',
          sSearch='Buscar:',
          sUrl='',
          sInfoThousands=',',
          sLoadingRecords='Cargando...',
          oPaginate= list(
            sFirst='Primera',
            sLast='Última',
            sNext='Siguiente',
            sPrevious='Anterior'
          ),
          oAria=list(
            sSortAscending=': Activar para ordenar la columna de manera ascendente',
            sSortDescending=': Activar para ordenar la columna de manera descendente'
          )
        ),
        lengthMenu = list(c(1, 5, 25, -1),c('1', '5', '25', 'Todos')), pageLength = 1, orderClasses = TRUE, searching = TRUE)),
        server = FALSE, selection = list(target = 'row'))
    }
    return('')
  })

  output$downloadData1 <- downloadHandler(
    filename = function() {
      s = input$ldom_rows_selected
      if (length(s)) {
        return(paste(str_sub(lee.ldom.php()[s[1],]$file_out, start = 28, end=47), '.csv', sep = ''))
      }
      'error'
    },
    content = function(file) {
      s = input$ldom_rows_selected
      if (length(s)) {
        require(readxl)
        res <- read_excel(as.character(lee.ldom.php()[s[1],]$file_out))
        write.csv(res, file, row.names = FALSE)
      }
    }
  )

  output$downloadData2 <- downloadHandler(
    filename = function() {
      s = input$ldom_rows_selected
      if (length(s)) {
        return(paste(str_sub(lee.ldom.php()[s[1],]$file_out, start = 28, end=47), '.xlsx', sep = ''))
      }
      'error'
    },
    content = function(file) {
      s = input$ldom_rows_selected
      if (length(s)) {
        file.copy(as.character(lee.ldom.php()[s[1],]$file_out), file)
      }
    }
  )

  output$downloadData3 <- downloadHandler(
    filename = function() {
      s = input$ldom_rows_selected
      if (length(s)) {
        return(paste(str_sub(lee.ldom.php()[s[1],]$file_out, start = 28, end=47), '.zip', sep = ''))
      }
      'error'
    },
    content = function(file) {
      s = input$ldom_rows_selected
      if (length(s)) {
        require(readxl)
        ffi <- as.character(lee.ldom.php()[s[1],]$file_out)
        res <- read_excel(ffi)
        res <- as.data.frame(res)
        require(sp)
        coordinates(res)<-~lon+lat
        proj4string(res)<-CRS("+proj=longlat +datum=WGS84 +no_defs")
        require(rgdal)
        writeOGR(res, dsn=paste('/srv/shiny-server/docs/shp/', str_sub(ffi, start = 28, end=47),'.shp',sep=''), layer="ldom", driver="ESRI Shapefile")
        zip(zipfile='/srv/shiny-server/docs/shp/fbCrawlExport.zip', files="/srv/shiny-server/docs/shp", flags = "-r9Xj")
        file.copy("/srv/shiny-server/docs/shp/fbCrawlExport.zip", file)
        system("rm -f /srv/shiny-server/docs/shp/*.*")
      }
    }
  )

  observeEvent(input$deleteData, {
    s <- input$ldom_rows_selected
    if (length(s)) {
      r <- lee.ldom.php()[s[1],]
      if(is.na(r$time_end)) {
        system(paste("pkill -TERM -P ", r$pid, sep = ""))
        progress$close()
        libre <<- 0
        tam <<- 0
        tamc <<- 0
        tam0 <<- 0
        tamp <<- 0
        tamZ <<- 1
      }
      system(paste("rm -f '", r$file_out, "'", sep = ""))
      remove.ldom.php(r$id)
    }
  })

})
