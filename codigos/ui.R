library(shiny)
library(leaflet)
library(DT)

shinyUI(fluidPage(
  
  navbarPage("",
             tabPanel("Mapa y Tabla dinamicas en R y shiny",
                      titlePanel("Mapa de Variables Climaticas y Grafica de Valores"),
                      sidebarLayout(
                        sidebarPanel(
                          # Select year
                          sliderInput("year",
                                      "Año:",
                                      min = 2004,
                                      max = 2016,
                                      value = 2004,
                                      step = 1,
                                      sep = "",
                                      animate=TRUE
                          ),
                          radioButtons('rad', label = 'SELECCIONA LA VARIABLE CLIMÁTICA:',  
                                       choices = list("Precipitacion total anual" = 1, "Temperatura minima promedio anual" = 2, 
                                                      'Temperatura promedio anual' = 3, "Temperatura maxima promedio anual" = 4), 
                                       selected = 1), 
                          uiOutput("estadosSelect")
                        ),
                        mainPanel(
                          # Show  SD plots
                          plotOutput("sdPlot"),
                          plotOutput("ggplot")
                        )
                      )
             ),
             tabPanel("LISA dinamica",
                      titlePanel("Comportamiento de las variables dinámicas (Precipitacion - Temperatura) 2004 - 2016"),
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons('radio', label = 'SELECCIONA EL TIPO DE VECINDAD:',  
                                       choices = list("Tipo reina (queen)" = 1, "Tipo torre (rook)" = 2, 
                                                      'Vecinos mas cercanos (kNN)' = 3, 'Distancia' = 4), 
                                       selected = 1), 
                          radioButtons('radiodos', label = 'SELECCIONA LA VARIABLE CLIMÁTICA:',  
                                       choices = list("Precipitacion total anual" = 1, "Temperatura minima promedio anual" = 2, 
                                                      'Temperatura promedio anual' = 3, "Temperatura maxima promedio anual" = 4), 
                                       selected = 1), 
                          conditionalPanel(
                            condition = "input.radio == 3", 
                            sliderInput("knn_slider", 'SELECCIONA EL NÚMERO DE VECINOS', 
                                        min = 1, max = 31, value = 6)
                          ), 
                          conditionalPanel(
                            condition = "input.radio == 4", 
                            sliderInput("dist_slider", "SELECCIONA EL UMBRAL DE DISTANCIA EN KM", 
                                        min = 440, max = 2000, step = 10, value = 500)
                          ),
                          # Select year
                          sliderInput("yearLISA",
                                      "AÑO DE INTERÉS:",
                                      min = 2004,
                                      max = 2016,
                                      value = 2004,
                                      step = 1,
                                      sep = "",
                                      animate=TRUE
                          )
                        ),
                        mainPanel(
                          plotOutput("moranPlot", height = 400, brush = brushOpts(id = "plot_brush")),
                          DT::dataTableOutput("table", width = '100%'),
                          leafletOutput("LISAmap", width = '100%')
                        )
                      )
             )
  )
))

