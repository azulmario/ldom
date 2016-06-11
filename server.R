# The MIT License (MIT)
# Copyright (c) 2016 Mario Hernández Morales
#    @error: SET_STRING_ELT() can only be applied to a 'character vector', not a 'raw'
library(shiny)
library(shinyBS)
library(leaflet)
library(DT)
library(xtable)
library(utils)
library(tools)
source("qmaps.R")

vals <- reactiveValues(count=0)

options(shiny.maxRequestSize = -1) # ¡Alerta! 25*1024^2
shinyServer(function(input, output, session) {
  isolate(vals$count <- vals$count + 1)
  jj <- "" # Nombre del archivo subido
  ll <- 0 # Número de elementos de pestaña seleccionada
  tamc <- 0 # Número de lotes de bitácora (entero)
  tame <- 0 # Número de elementos del lote procesado (entero)
  tam0 <- 0 # Número de elementos ya procesados del lote (entero menor)
  tamp <- FALSE # Indica si se está ejecutando un lote (boleano), pero sin la intervención de otras funciones
  tamZ <- TRUE # Asegura que se imprima la bitácora vacía
  progress <- NULL
  progrest <- NULL

  output$matricula <- renderTable({
    ll <<- 0
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)

    # Renombrar
    jj <<- file.path("/srv/shiny-server/docs/in", inFile$name)
    jj <<- gsub("[[:space:]]+", "_", jj)
    jj <<- gsub("\\(", "", jj)
    jj <<- gsub("\\)", "", jj)
    system(paste('cp -f', inFile$datapath, jj))
    # Número de pestañas
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
    matricula <- lee(jj, input$sheet, iforder = FALSE, ifcompact = TRUE)
    ll <<- nrow(matricula)
    if (ll < 7) {
      return(xtable(matricula))
    }
    # Regresa una versión corta para visualizar
    xtable(matricula[c(1:min(5,ll-1),ll),])
  },
  caption = "Vista previa de los datos:",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  ntext <- eventReactive(input$goButton, {
    ldom <- lee.ldom.php()
    if(jj == "") {
      return("Aún no ha subido el archivo de trabajo.")
    }
    if(ll == 0) {
      return("Lote sin direcciones por procesar.")
    }
    if(!(length(ldom$size) == 0 || !is.na(ldom[1,]$time_end))) {
      return("Servidor ocupado.")
    }

    kk <- paste0('/srv/shiny-server/docs/out/r', format(Sys.time()), '.xlsx')
    id <- ldom.php(file_in = jj, sheet = input$sheet, file_out = kk, size = ll)
    opetus <- paste0("/usr/bin/Rscript -e \"source('/srv/shiny-server/ldom/qmaps.R'); main(path = '",jj,"', sheet = ",input$sheet,", file = '",kk,"', id = ",id,");\" --vanilla &") 
    system(opetus)
    paste0("¡Espere a que termine el proceso! Al finalizar, busqué en la bitácora de trabajo el número ", id, ".")
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
        if(tamp == FALSE) {
          tamp <<- TRUE
          progress <<- shiny::Progress$new(min=1, max=ldom[1,]$size)
          imprime <- 1
        }
      } else {
        if(tamp == TRUE) {
          tamp <<- FALSE
          progress$close()
          imprime <- 1
        }
      }
      if(tamc != length(ldom$id)) {
        tamc  <<- length(ldom$id)
        imprime <- 1
      }
      if(tame != ldom[1,]$size) {
        tame  <<- ldom[1,]$size
        imprime <- 1
      }
      if(tam0 != ldom[1,]$biased) {
        tam0  <<- ldom[1,]$biased
        if(tamp) {
          A1 <-ldom[1,]$size
          A2 <- ldom[1,]$biased
          B1 <- strptime(ldom[1,6], "%Y-%m-%d %H:%M:%S")
          B2 <- Sys.time()
          E <- B1+((B2-B1)*A1/A2)
          if(A2 == 0) {
            progress$set(value = 1, message = "Preparando")
          } else if (4 > A2) {
            progress$set(value = A2+1, message = paste0("Procesando", str_dup('.', A2)))
          } else if (A1 / 2 > A2 && 400 > A2) {
            progress$set(value = A2+1, message = "Procesando...", detail = paste0("(", A2, '/', A1, ")"))
          } else {
            progress$set(value = A2+1, message = paste("Faltan:", format(E - B2, digits = 3)), detail = paste0("(", A2, '/', A1, ")"))
          }
        }
      }
      tamZ <<- TRUE
    } else {
      if(tamZ) {
        tamZ <<- FALSE
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

  # Descarga en formato CSV
  output$downloadData1 <- downloadHandler(
    filename = function() {
      s <- input$ldom_rows_selected
      if (length(s)) {
        return(paste0(str_sub(lee.ldom.php()[s[1],]$file_out, start = 28, end=47), '.csv'))
      }
      'error'
    },
    content = function(file) {
      s <- input$ldom_rows_selected
      if (length(s)) {
        require(feather)
        res <- read_feather(paste0(as.character(lee.ldom.php()[s[1],]$file_out), ".dat"))
        write.csv(res, file, row.names = FALSE)
      }
    }
  )

  # Descarga en formato XLSX
  output$downloadData2 <- downloadHandler(
    filename = function() {
      s <- input$ldom_rows_selected
      if (length(s)) {
        return(paste0(str_sub(lee.ldom.php()[s[1],]$file_out, start = 28, end=47), '.xlsx'))
      }
      'error'
    },
    content = function(file) {
      s <- input$ldom_rows_selected
      if (length(s)) {
        if(!file.exists(as.character(lee.ldom.php()[s[1],]$file_out))) {
          require(feather)
          res <- read_feather(paste0(as.character(lee.ldom.php()[s[1],]$file_out), ".dat"))
          require(openxlsx)
          openxlsx::write.xlsx(res, as.character(lee.ldom.php()[s[1],]$file_out))
        }
        file.copy(as.character(lee.ldom.php()[s[1],]$file_out), file)
      }
    }
  )

  # Descarga en formato SHP
  output$downloadData3 <- downloadHandler(
    filename = function() {
      s <- input$ldom_rows_selected
      if (length(s)) {
        return(paste0(str_sub(lee.ldom.php()[s[1],]$file_out, start = 28, end=47), '.zip'))
      }
      'error'
    },
    content = function(file) {
      s <- input$ldom_rows_selected
      if (length(s)) {
        ffi <- as.character(lee.ldom.php()[s[1],]$file_out)
        require(feather)
        res <- read_feather(paste0(ffi, ".dat"))
        res <- as.data.frame(res[which(res$lon != 0),])
        require(sp)
        coordinates(res)<-~lon+lat
        proj4string(res)<-CRS("+proj=longlat +datum=WGS84 +no_defs")
        require(rgdal)
        writeOGR(res, dsn=paste0('/srv/shiny-server/docs/shp/', str_sub(ffi, start = 28, end=47),'.shp'), layer="ldom", driver="ESRI Shapefile")
        zip(zipfile='/srv/shiny-server/docs/shp/fbCrawlExport.zip', files="/srv/shiny-server/docs/shp", flags = "-r9Xj")
        file.copy("/srv/shiny-server/docs/shp/fbCrawlExport.zip", file)
        system("rm -f /srv/shiny-server/docs/shp/*.*")
      }
    }
  )

  # Visualiza en mapa
  output$mymap <- renderLeaflet({
    s <- input$ldom_rows_selected 
    if (length(s)) {
      require(feather)
      res <- read_feather(paste0(as.character(lee.ldom.php()[s[1],]$file_out), ".dat"))
      res <- res[which(res$lon != 0),]
      if (length(res$lon) > 0) {
        leaflet() %>%
          addProviderTiles("Esri.WorldStreetMap",
            options = providerTileOptions(noWrap = TRUE, detectRetina = TRUE, reuseTiles = TRUE, minZoom = 8, maxZoom = 19)) %>%
          addMarkers(lng = res$lon, lat = res$lat, popup = res$ntd, layerId = res$n, clusterOptions = markerClusterOptions(), clusterId = 'clstr')
      }
    }
  })

  output$button_ui <- renderUI({
    tagList(
      bsModal("modal1", "Ubicación de domicilios localizados", trigger = "a1", size = "large", leafletOutput("mymap", height = 512), renderText({paste0("Nota: Deslice el cursor sobre un grupo para ver los límites de sus puntos y haga clic para ampliar sus límites.")})),
      actionButton("a1", "Visor")
    )
  })

  # Visualiza el rendimiento
  output$distPlot <- renderPlot({
    s <- input$ldom_rows_selected
    if (length(s)) {
      require(feather)
      res <- read_feather(paste0(as.character(lee.ldom.php()[s[1],]$file_out), ".dat"))
      if (length(res$n) > 0) {
        res$Nivel <- factor(res$niv, levels = c(0, 1, 2, 3, 4, -1, -2), labels = c("Número exterior", "Entrecalle", "Calle", "Colonia", "Localidad", "No localizada", "Bug"))
        res$Direcciones <- 1.0
        res$CBM <- res$BM
        res$CBM[res$BM == 0] <- "a"
        res$CBM[res$BM > 0.000 & res$BM <= 0.025] <- "b"
        res$CBM[res$BM > 0.025 & res$BM <= 0.050] <- "c"
        res$CBM[res$BM > 0.050 & res$BM <= 0.075] <- "d"
        res$CBM[res$BM > 0.075 & res$BM <= 0.100] <- "e"
        res$CBM[res$BM > 0.100] <- "f"
        res <- res[with(res, order(Nivel, BM)),]
        res$CBM <- factor(res$CBM, levels = c("f", "e", "d", "c", "b", "a"), labels = c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo", "Sin error") )
        ppd <- table(res$CBM) > 0
        cbbPalette <- c()
        if(ppd[1]) cbbPalette <- c(cbbPalette, "#ff6634")
        if(ppd[2]) cbbPalette <- c(cbbPalette, "#fecf07") 
        if(ppd[3]) cbbPalette <- c(cbbPalette, "#66cc9a") 
        if(ppd[4]) cbbPalette <- c(cbbPalette, "#339832") 
        if(ppd[5]) cbbPalette <- c(cbbPalette, "#080c9f") 
        if(ppd[6]) cbbPalette <- c(cbbPalette, "#046394")
        require(ggplot2)
        ggplot() + theme_bw()  +
          geom_bar(aes(y = Direcciones, x = Nivel, fill = CBM), data = res, stat="identity") +
          theme(legend.title = element_blank()) + 
          ggtitle("Nivel y error") + labs(x="Nivel", y="Cantidad de direcciones") +
          scale_fill_manual(values = cbbPalette)
      }
    }
  })
  
  output$button_pt <- renderUI({
    tagList(
      bsModal("modal2", "Calidad y precisión de domicilios localizados", trigger = "a2", size = "large", plotOutput("distPlot")),
      actionButton("a2", "Plot")
    )
  })

  # Elimina entrada, incluye interrumpir proceso
  observeEvent(input$deleteData, {
    s <- input$ldom_rows_selected
    if (length(s)) {
      r <- lee.ldom.php()[s[1],]
      if(is.na(r$time_end)) {
        system(paste0("pkill -TERM -P ", r$pid, " ; kill -s TERM ", r$pid))
        progress$close()
        tame <<- 0
        tamc <<- 0
        tam0 <<- 0
        tamp <<- FALSE
        tamZ <<- TRUE
      }
      system(paste0("rm -f '", r$file_out, "'"))
      system(paste0("rm -f '", r$file_out, ".dat'"))
      remove.ldom.php(r$id)

      if(length(lee.ldom.php()$id) == 0) {
        restart.ldom.php()
        system("rm -f /srv/shiny-server/docs/in/*")
      }
    }
  })

  session$onSessionEnded(function() {
    isolate(vals$count <- vals$count - 1)
    ldom <- lee.ldom.php()
    # Si no hay aplicaciones abiertas y ninguna tarea está en proceso
    if(isolate(vals$count) == 0 && (nrow(ldom) == 0 || is.na(ldom[1,]$time_end))) {
      # Borra archivos antiguos para no saturar el directorio
      system("find /srv/shiny-server/docs/in -mtime +7 -type f -exec rm -f {} \\;")
      system("find /srv/shiny-server/docs/zip -mtime +7 -type f -exec rm -f {} \\;")
      # Realiza la actualización cada semana
      if(!file.exists("/srv/shiny-server/docs/zip/master.zip")) {
        download.file("https://github.com/azulmario/ldom/archive/master.zip", "/srv/shiny-server/docs/zip/master.zip", method = "auto", quiet = TRUE)
        unzip("/srv/shiny-server/docs/zip/master.zip", exdir = "/srv/shiny-server/docs/zip/", junkpaths = TRUE)
        # Actualiza solo la aplicación principal
        file.copy("/srv/shiny-server/docs/zip/qmaps.R", "/srv/shiny-server/ldom/qmaps.R", overwrite = TRUE)
        file.copy("/srv/shiny-server/docs/zip/cadenas.R", "/srv/shiny-server/ldom/cadenas.R", overwrite = TRUE)
        file.copy("/srv/shiny-server/docs/zip/conectadb.R", "/srv/shiny-server/ldom/conectadb.R", overwrite = TRUE)
        file.copy("/srv/shiny-server/docs/zip/server.R", "/srv/shiny-server/ldom/server.R", overwrite = TRUE)
        file.copy("/srv/shiny-server/docs/zip/ui.R", "/srv/shiny-server/ldom/ui.R", overwrite = TRUE)
      }
    }
  })

  output$count <- renderText({
    vals$count
  })

})
