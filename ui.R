# The MIT License (MIT)
# Copyright (c) 2016 Mario Hernández Morales
# 
library(shiny)
library(shinyBS)
library(DT)

shinyUI(fluidPage(
  tags$head(
    tags$style(HTML(
"
.shiny-progress .bar {
  background-color: #FF0000;
  .opacity = 0.8;
}
.shiny-progress .progress {
  height:18px;
}
.shiny-progress .progress-text {
	border-color:rgb(235, 204, 209);
	border:1px solid transparent;
	border-radius:4px;
  font-size:14p;
  font-weight:700;
	color:rgb(169, 68, 66);
	background-color:rgb(242, 222, 222);
  line-height:20px;
  box-sizing:border-box;
  padding:15px;
  .opacity = 0.5;
  -ms-display: flex;
  display: flex;
  align-items: center;
  justify-content: center;
}
"))
  ),
  titlePanel("Localización automatizada de domicilios geográficos"),
  sidebarLayout(
    sidebarPanel(
      p("Ejemplo demostrativo de la potencialidad de localización automatizada de domicilios geográficos. Para su desarrollo se implementa una técnica de inteligencia artificial denominada árbol de decisión."),
      tags$hr(),
      h4("Notas"),
      p("Los archivos de entrada deben tener un formato específico, primero se deben identificar los campos necesarios, según la ",a(href = '../docs/dof_ntdg.pdf', 'norma técnica de domicilios geográficos', target='_blank')," de Inegi, e identificarlos con etiquetas preestablecida. También puede consultar su ", a(href = '../docs/manual_ntdg2012.pdf', 'Manual.', target='_blank')),
      p('Recomendamos utilizar el archivo muestra, tomarlo como referencia, aquí puedes descargar la plantilla',
        a(href = '../docs/demo.xlsx', 'demo.xlsx', target='_blank'), 'para utilizarla para sus datos. Además vea el ', a(href = '../docs/demo.xlsx', 'Diccionario de Datos', target='_blank'), ' para conocer todas las posibilidades disponibles.'),
      p("La velocidad depende de las características de la máquina virtual, principalmente en el número de procesadores asignados."),
      tags$hr(),
      h4("Referencias"),
      p('Programa de captura de domicilios con adecuaciones para visualizarlo en un mapa:',
        a(href = '../dom', '/dom.', target='_blank')),
      p('Reporte de características del programa automatizado:',
        a(href = '../reporte', '/reporte.', target='_blank')),
      "Sesiones actualmente abiertas de esta aplicación:", 
      verbatimTextOutput("count"),
      p('Versión 20160708'),
      textOutput("currentTime")
    ),
    mainPanel(
      h3("Primer paso"),
      p("El primer paso es cargar el archivo de direcciones."),
      fileInput('file1', 'Elija el archivo a subir:', accept = c('.xlsx')),
      uiOutput("slider"),
      tableOutput('matricula'),
      tags$hr(),
      h3("Segundo paso"),
      p("Cuando esté listo, presione el siguiente botón para ejecutar el procesamiento de todas las direcciones del lote definido en el paso anterior."),
      actionButton('goButton','Procesamiento por lote'),
      textOutput("nText"),
      tags$hr(),
      h3("Tercer paso"),
      p("Administración de resultados. Seleccione en la siguiente lista un lote y elija un botón, más abajo, para ejecutar la acción indicada."),
      fluidRow(
        column(width = 12, DT::dataTableOutput('ldom')),
        column(width = 12, tags$hr()),
        column(width = 2, downloadButton('downloadData1', 'CSV')),
        column(width = 2, downloadButton('downloadData2', 'XLS')),
        column(width = 2, downloadButton('downloadData3', 'SHP')),
        column(width = 2, uiOutput("button_ui")),
        column(width = 2, uiOutput("button_pt")),
        column(width = 2, actionButton('deleteData', 'DEL', icon =icon('erase', lib = "glyphicon")))
      ),
      tags$hr()
    )
  )
))