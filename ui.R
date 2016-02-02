library(shiny)

shinyUI(fluidPage(
  titlePanel("LDom"),
  sidebarLayout(
    sidebarPanel(
      h3("Localización automatizada de domicilios geográficos"),
      p("Ejemplo demostrativo de la potencialidad de localización automatizada de domicilios geográficos. Para su desarrollo se implementa una técnica de inteligencia artificial denominada árbol de decisión."),
      p('Si quieres un archivo muestra para subir,',
        'puede descargar la plantilla',
        a(href = 'demo.xlsx', 'demo.xlsx'),
        'y utilizarla para sus datos.'
      ),
      tags$hr(),
      numericInput("sheet", 
                   label = p("Hoja a leer, un número entero (la posición de la hoja). Por defecto es la primera hoja."), 
                   value = 1, min = 1, step = 1),
      tags$hr(),
      fileInput('file1', 'Elija el archivo a subir',
                accept = c(
                  'application/vnd.ms-excel',
                  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                  '.xls',
                  '.xlsx'
                )
      ),
      tags$hr(),
      actionButton('goButton','Procesamiento por lote'),
      tags$hr(),
      verbatimTextOutput("nText"),
      tags$hr(),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      h2("Primer paso"),
      p("El primer paso es cargar el archivo de direcciones."),
      tableOutput('matricula')
    )
  )
))