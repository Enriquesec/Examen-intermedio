library(shiny)
library(sparklyr)
library(leaflet)
library(tidyverse)
library(lubridate)
library(DT)

config = spark_config()
config$`sparklyr.shell.driver-memory` <- "4G"
config$`sparklyr.shell.executor-memory` <- "4G"
config$`spark.yarn.executor.memoryOverhead` <- "512"
#Realizamos la conección y cargamos los datos en Spark.
sc = spark_connect(master = "local", config = config)
setwd("~/Documentos/Servicio_social/Examen intermedio/")
guanajuato<-spark_read_csv(sc,"guajuato.csv/")
mi<-summarise(guanajuato,min(fechaRegistro))%>%collect()
mi<-as.Date(mi$`min(fechaRegistro)`)
ma<-summarise(guanajuato,max(fechaRegistro))%>%collect()
ma<-as.Date(ma$`max(fechaRegistro)`)
cate<-guanajuato%>%distinct(categoria)%>%arrange(categoria)%>%collect()
cate<-cate$categoria
ui <- fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  fluidRow(column(12,
                  leafletOutput("my_leaf"),
                  absolutePanel(top=10,right = 10,draggable = TRUE,
                                selectInput("cat", "Categorías",cate,selected = "AZUCAR"),
                                sliderInput("range","Time of data collection:",
                                            min = mi,
                                            max = ma,
                                            value = ma,
                                            step = 30,
                                            animate=animationOptions(interval = 3000))
                  ),
                  wellPanel(
                    span("Tabla de los objetos:"),  dataTableOutput("table")
                  )
                  ))
)

server <- function(input, output, session){
  output$my_leaf <- renderLeaflet({
      leaflet()%>%
      addTiles()%>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)
  })

  observe({
    aa<-input$cat
    tt<-input$range
    isolate({
      huevos<-guanajuato%>%select(categoria,longitud,latitud,precio,fechaRegistro)%>%filter(categoria==aa&fechaRegistro<tt)%>%group_by(longitud,latitud)%>%arrange(desc(fechaRegistro))%>%top_n(1)%>%collect()
      co<-huevos%>%distinct(precio)%>%collect()
      pal<-colorNumeric("viridis",c(co$precio))
      })
  huevos<-as.data.frame(huevos)
  leafletProxy("my_leaf",data=huevos)%>%
  clearShapes()%>%
  addCircles(lng=~as.numeric(longitud),lat=~as.numeric(latitud),
             weight = 1, color = "#777777",fillColor = ~pal(as.numeric(precio)), 
             fillOpacity = 0.7, popup = ~paste(as.numeric(precio)),radius=~100)%>%
  fitBounds(~min(as.numeric(longitud)), ~min(as.numeric(latitud)), ~max(as.numeric(longitud)), ~max(as.numeric(latitud)))
  })
  observe({
    aa<-input$cat
    tt<-input$range
    isolate({
      huevos<-guanajuato%>%select(categoria,longitud,latitud,precio,fechaRegistro)%>%filter(categoria==aa&fechaRegistro<tt)%>%group_by(longitud,latitud)%>%arrange(desc(fechaRegistro))%>%top_n(1)%>%collect()
      co<-huevos%>%distinct(precio)%>%collect()
      pal<-colorNumeric("viridis",c(co$precio))
    })
    huevos<-as.data.frame(huevos)
    proxy<-leafletProxy("my_leaf",data=huevos)
    proxy%>%clearControls()
    if(input$range||input$car){
      proxy%>%addLegend(title = "Rango de Precios.",position = "bottomright",pal = pal, values = ~as.numeric(precio))
    }
  })
  observe({
    aa<-input$cat
    tt<-input$range
    isolate({
      huevos<-guanajuato%>%select(categoria,longitud,latitud,precio,fechaRegistro)%>%filter(categoria==aa&fechaRegistro<tt)%>%group_by(longitud,latitud)%>%arrange(desc(fechaRegistro))%>%top_n(1)%>%collect()
    })
    huevos<-as.data.frame(huevos)
    output$table<-renderDataTable(huevos) 
  })
  
}

shinyApp(ui, server)