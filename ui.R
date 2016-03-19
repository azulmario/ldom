library(shiny)

shinyUI(fluidPage(
  titlePanel("Localización automatizada de domicilios geográficos"),
  sidebarLayout(
    sidebarPanel(
      p("Ejemplo demostrativo de la potencialidad de localización automatizada de domicilios geográficos. Para su desarrollo se implementa una técnica de inteligencia artificial denominada árbol de decisión."),
      tags$hr(),
      h4("Notas"),
      p("Los archivos de entrada deben tener un formato específico, primero se deben identificar los campos necesarios, según la norma técnica de domicilios geográficos de Inegi, e identificarlos con etiquetas preestablecida."),
      p('Puedes utilizar el archivo muestra, tomarlo como referencia, aquí puedes descargar la plantilla',
        a(href = '../docs/demo.xlsx', 'demo.xlsx', target='_blank'), 'para utilizarla para sus datos.'),
      p("La velocidad depende de las características de la máquina virtual, principalmente en el número de procesadores asignados."),
      tags$hr(),
      h4("Referencias"),
      p('Programa de captura de domicilios con adecuaciones para visualizarlo en un mapa:',
        a(href = '../dom', '/dom', target='_blank'), '.'),
      p('Reporte de características del programa automatizado:',
        a(href = '../reporte', '/reporte', target='_blank'), '.'),
      p('Carpeta de trabajo:',
        a(href = '../docs/out', '/out', target='_blank'), '.')
    ),
    mainPanel(
      h3("Primer paso"),
      p("El primer paso es cargar el archivo de direcciones."),
      fileInput('file1', 'Elija el archivo a subir:',
                accept = c(
                  'application/vnd.ms-excel',
                  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                  '.xls',
                  '.xlsx'
                )
      ),
      uiOutput("slider"),
      tableOutput('matricula'),
      tags$hr(),
      h3("Segundo paso"),
      p("Cuando esté listo, presione el siguiente botón para ejecutar el procesamiento de todas las direcciones del lote definido en el paso anterior:"),
      actionButton('goButton','Procesamiento por lote'),
      textOutput("nText"),
      tags$hr(),
      h3("Tercer paso"),
      downloadButton('downloadData', 'Download')
    )
  )
))