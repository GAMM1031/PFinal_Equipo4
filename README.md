# Poyecto Final    Equipo4     
## Objetivos :
Este proyecto está enfocado en realizar un estudio del comportamiento de las variables climáticas de Precipitación y Temperatura a partir de una serie histórica que comprende el periodo 2004-2016, las variables son estudiadas a partir de la obtención de:

-	Los mapas del comportamiento Anual de las variables Precipitaciones, Temperatura Media, Temperatura Mínima, Temperatura Máxima.

-	Comportamiento Anual de las variables climáticas a partir de la evaluación de indicadores de Asociación Espacial (LISA), para diferentes tipos matrices de vecindades y distancias.


Fuente del Datos:

Los datos fueron extraídos del sitio oficial del Servicio Meteorológico Nacional (SMN), a partir del de sus reportes de resúmenes Mensuales de Temperaturas y Lluvia publicado y accedido a través del enlace: http://smn.cna.gob.mx/es/climatologia/temperaturas-y-lluvias/resumenes-mensuales-de-temperaturas-y-lluvias

## Materiales Utilizados:

Para la confección de estos mapas, fue utilizada R-Studio Versión 1.0.153 – © 2009-2017 RStudio, Inc.

# Resultados obtenidos:

#### Mapas de Comportamiento de las variables climáticas

En este caso se obtuvo mapas interactivos con shinny los cuales permiten ser filtrados por tiempo a través del uso de un slide por años y un panel de selección para cada tipo de variables, el resultado de la selección es mostrado en un mapa que representa el comportamiento de las diferentes variables utilizando una escala en función del tipo de variable cuya leyenda es mostrada en el mapa. Además, en una segunda parte de la página se presenta el resultado de las variables en una gráfica de barras, en función del estado seleccionado

A continuación, dos ejemplos de los resultados alcanzados:

![img](/img/img1.png)

Se muestran los datos de la variable (Temperatura Mínima promedio Anual) para el año 2012 en el estado de Baja California.

![img](/img/img2.png)

Se muestran los datos de la variable (Precipitación total anual) para el año 2008 en el estado de Baja California

![img](/img/img3.png)

Como resultado de este módulo del proyecto es posible obtener de manera dinámica los datos de las variables de estudio a partir de la selección del tipo de variable a estudiar dada la entidad federativa, así mismo se presentan como elementos de visualización, el mapa interactivo (representación espacial) y una gráfica de barras con los valores anuales de la variable.

#####	Comportamiento Anual de las variables climáticas a partir de la evaluación de indicadores de Asociación Espacial (LISA), para diferentes tipos matrices de vecindades y distancias

En este módulo del proyecto la intención es obtener un conjunto de mapas del comportamiento de las variables de estudio en función de parámetros de vecindad seleccionados por el usuario a partir de la utilización de la herramienta Autocorrelación espacial (I de Moran) la cual mide la autocorrelación espacial basada en las ubicaciones y los valores de las entidades simultáneamente, dado un conjunto de entidades y un atributo asociado, expresando con esto la evaluación de existencia de un patrón de agrupamiento, de dispersión o es un fenómeno aleatorio.  Específicamente para este proyecto se presentan los resultados del índice de Asociación Espacial (LISA) basado en el cálculo de la I de Moran el cual es evaluado para cada `región i` (Entidades Federativas) en cada año `t` del periodo estudiado (`2004-2016`)



Para la evaluación del comportamiento de las variables fueron utilizadas varios tipos de matrices (`wij`), además de considerar la distancia entre ellos mediante un umbral definido, en este sentido el usuario puede evaluar el indicador según:
-	Tipo reina (Queen) 
-	Tipo torre (rook)
-	Vecinos más cercanos (kNN), definiendo el número de vecinos a evaluar
-	Distancia (dada un umbral definido)
La siguiente figura muestra la pantalla inicial para el proceso de selección:
En este caso se obtuvo además los valores indicativos del comportamiento del I de Moran presentado en un diagrama de moran y el cual va cambiando en función de los datos y la selección previa realizada por el usuario.

![img](/img/img4.png)


![img](/img/img5.png)


Mapa en leaflet del resultado de la I de Moran según el tipo de autocorrelación, además se representa la etiqueta que resulta de seleccionar con el mouse uno de las entidades, mostrando la entidad federativa seguido de los valores de la variable seleccionada para el año evaluado.

![img](/img/img6.png)


![img](/img/img7.png)

La siguiente imagen representa un ejemplo del resultado obtenido de manera conjunta
En este módulo del proyecto es posible obtener dada la selección del tipo de vecindad que se quiera evaluar, y para la selección de la variable objeto de estudio, el valor del índice de moran y los niveles de agrupamiento (dispersión) de la misma dado el año seleccionado.

En el ejemplo de la figura se muestran los resultados para una vecindad de reina, la variable de estudio es la precipitación total anual para el año 2004, en los resultados es posible apreciar que se obtuvo un valor de I de Moran = 0.36, siendo significativa la formación de clúster alto-alto para los estados de Yucatán (32.82mm), Quintana Roo (31.89mm), Campeche (32.62mm) y Tabasco (32.53mm), para el caso de la formación de clúster bajo-bajo son significativos los resultados para el Estado de México (23.12mm) e Hidalgo (23,93mm), el resto de los estados sus valores son no-significativos. 

![img](/img/img8.png)


## METODOLOGÍA
Antes de comenzar, necesitamos cargar (o instalar primero, en caso de que no estén ya instaladas) las librerías que vamos a usar:


```` R
library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(spdep)
library(ggplot2)
library(GISTools)
library(RColorBrewer)
library(leaflet)
library(DT)
library(dplyr)
library(reshape2)
library(scales)
library(extrafont)
````


## Se leen los datos de entrada
clima <- readOGR("clima.shp", verbose = FALSE, stringsAsFactors = FALSE, GDAL1_integer64_policy = T)
###### #Los vecinos de todos los estados QUEEN
clima.nbq <- poly2nb(clima, queen = T)
clima.nbq.w <- nb2listw(clima.nbq)
###### #Los vecinos de todos los estados ROOK
clima.nbr <- poly2nb(clima, queen = F)
clima.nbr.w <- nb2listw(clima.nbr)
###### #Las coordenadas de los centroides de los estados
coords <- coordinates(clima)


## Se crea la primer pestaña 

Donde se hace uso de las condiciones `if` y `else if`, para poder desplegar la información de las cuatro variables climáticas, que son precipitación, temperatura media, temperatura máxima y temperatura minima:




```` R
shinyServer(function(input, output) {
  
  year <- reactive({ 
    as.character(input$year) 
  })
  
  data <- reactive({
    if (input$rad == 1) {
      data <- switch(year(), 
                     "2004" = clima$PR04,
                     "2005" = clima$PR05, 
                     "2006" = clima$PR06,
                     "2007" = clima$PR07,
                     "2008" = clima$PR08,
                     "2009" = clima$PR09,
                     "2010" = clima$PR10,
                     "2011" = clima$PR11,
                     "2012" = clima$PR12,
                     "2013" = clima$PR13,
                     "2014" = clima$PR14,
                     "2015" = clima$PR15,
                     "2016" = clima$PR16)
    } else if (input$rad == 2) {
      data <- switch(year(), 
                     "2004" = clima$TMIN04,
                     "2005" = clima$TMIN05, 
                     "2006" = clima$TMIN06,
                     "2007" = clima$TMIN07,
                     "2008" = clima$TMIN08,
                     "2009" = clima$TMIN09,
                     "2010" = clima$TMIN10,
                     "2011" = clima$TMIN11,
                     "2012" = clima$TMIN12,
                     "2013" = clima$TMIN13,
                     "2014" = clima$TMIN14,
                     "2015" = clima$TMIN15,
                     "2016" = clima$TMIN16)
    } else if (input$rad == 3) {
      data <- switch(year(), 
                     "2004" = clima$TMED04,
                     "2005" = clima$TMED05, 
                     "2006" = clima$TMED06,
                     "2007" = clima$TMED07,
                     "2008" = clima$TMED08,
                     "2009" = clima$TMED09,
                     "2010" = clima$TMED10,
                     "2011" = clima$TMED11,
                     "2012" = clima$TMED12,
                     "2013" = clima$TMED13,
                     "2014" = clima$TMED14,
                     "2015" = clima$TMED15,
                     "2016" = clima$TMED16)
    } else {
      data <- switch(year(), 
                     "2004" = clima$TMAX04,
                     "2005" = clima$TMAX05, 
                     "2006" = clima$TMAX06,
                     "2007" = clima$TMAX07,
                     "2008" = clima$TMAX08,
                     "2009" = clima$TMAX09,
                     "2010" = clima$TMAX10,
                     "2011" = clima$TMAX11,
                     "2012" = clima$TMAX12,
                     "2013" = clima$TMAX13,
                     "2014" = clima$TMAX14,
                     "2015" = clima$TMAX15,
                     "2016" = clima$TMAX16)
    }
    
  })
  
  etiq <- reactive({
    if (input$rad == 1) {
      etiq = "PrecipitaciÃ³n total registrada en el aÃ±o"
    } else if (input$rad == 2) {
      etiq = "Temperatura mÃ­nima promedio en el aÃ±o"
    } else if (input$rad == 3) {
      etiq = "Temperatura promedio en el aÃ±o"
    }  else {
      etiq = "Temperatura mÃ¡xima promedio en el aÃ±o"
    }
  })
  
  et <- reactive({
    if (input$rad == 1) {
      et = "en mm de lÃ¡mina de lluvia"
    }  else {
      et = "en Â°C"
    }
  })
  
  plt <- reactive({
    if (input$rad == 1) {
      plt = "Blues"
    }  else {
      plt = "YlOrRd"
    }
  })
  
````



### [ACCESO A LOS COGIGOS FUENTES]: https://github.com/GAMM1031/PFinal_Equipo4/blob/master/codigos%20/server
