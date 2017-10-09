![img](/img/Fondo.png)


# Proyecto Final    Equipo4   

# R Studio


## Objetivos :
Este proyecto está enfocado en realizar un estudio del comportamiento de las variables climáticas de Precipitación y Temperatura a partir de una serie histórica que comprende el periodo 2004-2016, las variables son estudiadas a partir de la obtención de:

-	Los mapas del comportamiento Anual de las variables Precipitaciones, Temperatura Media, Temperatura Mínima, Temperatura Máxima.

-	Comportamiento Anual de las variables climáticas a partir de la evaluación de indicadores de Asociación Espacial (LISA), para diferentes tipos matrices de vecindades y distancias.



## Materiales Utilizados:

Para la confección de estos mapas, fue utilizada R-Studio Versión 1.0.153 – © 2009-2017 RStudio, Inc.

Los datos fueron extraídos del sitio oficial del Servicio Meteorológico Nacional (SMN), a partir del de sus reportes de resúmenes Mensuales de Temperaturas y Lluvia publicado y accedido a través del enlace: http://smn.cna.gob.mx/es/climatologia/temperaturas-y-lluvias/resumenes-mensuales-de-temperaturas-y-lluvias



# Resultados obtenidos:


### Mapas de Comportamiento de las variables climáticas

En este caso se obtuvo mapas interactivos con shinny los cuales permiten ser filtrados por tiempo a través del uso de un slide por años y un panel de selección para cada tipo de variables, el resultado de la selección es mostrado en un mapa que representa el comportamiento de las diferentes variables utilizando una escala en función del tipo de variable cuya leyenda es mostrada en el mapa. Además, en una segunda parte de la página se presenta el resultado de las variables en una gráfica de barras, en función del estado seleccionado

A continuación, dos ejemplos de los resultados alcanzados:

![img](/img/img1.png)

Se muestran los datos de la variable (Temperatura Mínima promedio Anual) para el año 2012 en el estado de Baja California.

![img](/img/img2.png)

Se muestran los datos de la variable (Precipitación total anual) para el año 2008 en el estado de Baja California

![img](/img/img3.png)

Como resultado de este módulo del proyecto es posible obtener de manera dinámica los datos de las variables de estudio a partir de la selección del tipo de variable a estudiar dada la entidad federativa, así mismo se presentan como elementos de visualización, el mapa interactivo (representación espacial) y una gráfica de barras con los valores anuales de la variable.

####	Comportamiento Anual de las variables climáticas a partir de la evaluación de indicadores de Asociación Espacial (LISA), para diferentes tipos matrices de vecindades y distancias

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





## Impresión del mapa de la variable seleccionada 

Donde imprime el mapa de salida de acuerdo a la seleccón que se le haga click en el panel de opciones del lado izquierdo, tanto en la variable elegida como en el año.


```` R
output$sdPlot <- renderPlot({
    data()
    par(mar = c(0, 0, 1.5, 0))
    shades <- auto.shading(data(), cutter = rangeCuts, n = 7, cols = brewer.pal(7, plt()))
    choropleth(clima, data(), shades)
    choro.legend(-95, 32, shades, under = "<", over = ">", between = "a", box.lty = "blank", x.intersp = 0.5, y.intersp = 0.75, cex = 1)
    title(main = paste(etiq(), year(), et()), cex.main = 1.25)  
  })
  
  plot.data <- data.frame(clima)
  
  output$estadosSelect <- renderUI({
    selectInput("estado",
                "Estado:",
                choices = as.list(unique(clima$ADMIN_NAME)),
                selected = "Aguascalientes")
  })
  
  estado <- reactive({
    if (is.null(input$estado)) {
      return(NULL)
    }
    input$estado
  })
  
  output$ggplot <- renderPlot({
    if (is.null(input$estado)) {
      return()
    }
    
    plot.data.gg <- subset(clima, ADMIN_NAME==estado())
    
    if (input$rad == 1) {
      data.gg <- data.frame(plot.data.gg$ADMIN_NAME,plot.data.gg$PR04,  
                            plot.data.gg$PR05, plot.data.gg$PR06, plot.data.gg$PR07, 
                            plot.data.gg$PR08, plot.data.gg$PR09, plot.data.gg$PR10, 
                            plot.data.gg$PR11, plot.data.gg$PR12, plot.data.gg$PR13, 
                            plot.data.gg$PR14, plot.data.gg$PR15, plot.data.gg$PR16)
      colnames(data.gg) = c('ADMIN_NAME', '2004', '2005','2006', '2007', '2008', '2009', 
                            '2010', '2011', '2012', '2013', '2014', '2015', '2016')
      plot.total <- melt(data.gg, id = c("ADMIN_NAME"))
    } else if (input$rad == 2) {
      data.gg <- data.frame(plot.data.gg$ADMIN_NAME,plot.data.gg$TMIN04,  
                            plot.data.gg$TMIN05, plot.data.gg$TMIN06, plot.data.gg$TMIN07, 
                            plot.data.gg$TMIN08, plot.data.gg$TMIN09, plot.data.gg$TMIN10, 
                            plot.data.gg$TMIN11, plot.data.gg$TMIN12, plot.data.gg$TMIN13, 
                            plot.data.gg$TMIN14, plot.data.gg$TMIN15, plot.data.gg$TMIN16)
      colnames(data.gg) = c('ADMIN_NAME', '2004', '2005','2006', '2007', '2008', '2009', 
                            '2010', '2011', '2012', '2013', '2014', '2015', '2016')
      plot.total <- melt(data.gg, id = c("ADMIN_NAME"))
    } else if (input$rad == 3) {
      data.gg <- data.frame(plot.data.gg$ADMIN_NAME,plot.data.gg$TMED04,  
                            plot.data.gg$TMED05, plot.data.gg$TMED06, plot.data.gg$TMED07, 
                            plot.data.gg$TMED08, plot.data.gg$TMED09, plot.data.gg$TMED10, 
                            plot.data.gg$TMED11, plot.data.gg$TMED12, plot.data.gg$TMED13, 
                            plot.data.gg$TMED14, plot.data.gg$TMED15, plot.data.gg$TMED16)
      colnames(data.gg) = c('ADMIN_NAME', '2004', '2005','2006', '2007', '2008', '2009', 
                            '2010', '2011', '2012', '2013', '2014', '2015', '2016')
      plot.total <- melt(data.gg, id = c("ADMIN_NAME"))
    } else {
      data.gg <- data.frame(plot.data.gg$ADMIN_NAME,plot.data.gg$TMAX04,  
                            plot.data.gg$TMAX05, plot.data.gg$TMAX06, plot.data.gg$TMAX07, 
                            plot.data.gg$TMAX08, plot.data.gg$TMAX09, plot.data.gg$TMAX10, 
                            plot.data.gg$TMAX11, plot.data.gg$TMAX12, plot.data.gg$TMAX13, 
                            plot.data.gg$TMAX14, plot.data.gg$TMAX15, plot.data.gg$TMAX16)
      colnames(data.gg) = c('ADMIN_NAME', '2004', '2005','2006', '2007', '2008', '2009', 
                            '2010', '2011', '2012', '2013', '2014', '2015', '2016')
      plot.total <- melt(data.gg, id = c("ADMIN_NAME"))
    }
    
    a <- reactive({
      if (input$rad == 1) {
        a <- paste("Precipitacion total anual del estado de", plot.total$ADMIN_NAME)
      } else if (input$rad == 2) {
        a <- paste("Temperatura mÃ­nima promedio anual del estado de", plot.total$ADMIN_NAME)
      } else if (input$rad == 3) {
        a <- paste("Temperatura promedio anual del estado de", plot.total$ADMIN_NAME)
      } else {
        a <- paste("Temperatura mÃ¡xima promedio anual del estado de", plot.total$ADMIN_NAME)
      }
    })
    
    b <- reactive({
      if (input$rad == 1) {
        b <- "LÃ¡mina de lluvia en mm"
      } else {
        b <- "Temperatura en Â°C"
      }
    })
    
    dosmenos <- round(min(plot.total$value), digits = 2)
    unomenos <- round((min(plot.total$value) + mean(plot.total$value))/2, digits = 2)
    media <- round(mean(plot.total$value), digits = 2)
    unomas <- round((mean(plot.total$value) + max(plot.total$value))/2, digits = 2)
    dosmas <- round(max(plot.total$value), digits = 2)
    
    sdClass <- function(var){
      
      Clase = vector(mode = "character", length = length(var))
      
      Clase[var <= dosmenos] <- '1'
      Clase[var > dosmenos & var <= unomenos] <- '2'
      Clase[var > unomenos & var <= media] <- '3'
      Clase[var > media & var <= unomas] <- '4'
      Clase[var > unomas & var <= dosmas] <- '5'
      
      Clase <- factor(Clase, levels = c('1', '2', '3','4','5'))
      return(Clase)
    }
 ````



#### Se imprime la gráfica que corresponde a los datos anuales de precipitación de acuerdo al estado seleccionado



 ````R
   plot.total$'Clase' <- sdClass(plot.total$value)
    
    ggplot(plot.total, aes(x = variable, y = value, fill = Clase)) +
      ggtitle(a()) +
      xlab("Anios con datos") +
      ylab(b()) +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "Verdana")) +
      theme(axis.title.x = element_text(hjust = 0.5, family = "Verdana", face="bold", size=12)) +
      theme(axis.title.y = element_text(hjust = 0.5, family = "Verdana", face="bold", size=12)) +
      theme(axis.text.x = element_text(size = 12, family = "Verdana")) +
      theme(axis.text.y = element_text(size = 12, family = "Verdana")) +
      scale_x_discrete(limit = c('2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016'),
                       labels = c('2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016')) +
      scale_fill_brewer(palette = plt(), name = "Rangos",
                        breaks = c('1', '2', '3', '4','5'),
                        labels = c(dosmenos, unomenos, media, unomas, dosmas), drop = TRUE, guide = "legend") +
      geom_bar(stat="identity") +
      scale_y_continuous(breaks= pretty_breaks()) +
      guides(fill = guide_legend(ncol=1)) +  
      theme(legend.title = element_text(size = 12, family = "Verdana", face="italic"),
            legend.position = "left", legend.key.size = unit(8, "mm"), strip.text = element_text(size=8, face="bold"),
            legend.text = element_text(size = 10, family = "Verdana")) +
      geom_text(aes(label = value), position = position_stack(0.1), size = 5, color = "black", face="bold")
  })
 ````


## Se crea la segunda pestaña 
En este apartado, se crean la representación gráfica por medio de un mapa y gráfica del análisis de los indicadores locales de asociación espacial (LISA, por su acrónimo en inglés).

Para lo cual se vuelve hacer uso de las condiciones `if` y `else if`, para poder desplegar la información de las cuatro variables climáticas, que son precipitación, temperatura media, temperatura máxima y temperatura minima:



 ````R
 selected <- reactive({
    if (input$radiodos == 1) {
      if (input$yearLISA == 2004) {
        var <- clima$PR04
      } else if (input$yearLISA == 2005) {
        var <- clima$PR05  
      } else if (input$yearLISA == 2006) {
        var <- clima$PR06
      } else if (input$yearLISA == 2007) {
        var <- clima$PR07
      } else if (input$yearLISA == 2008) {
        var <- clima$PR08
      } else if (input$yearLISA == 2009) {
        var <- clima$PR09
      } else if (input$yearLISA == 2010) {
        var <- clima$PR10
      } else if (input$yearLISA == 2011) {
        var <- clima$PR11
      } else if (input$yearLISA == 2012) {
        var <- clima$PR12
      } else if (input$yearLISA == 2013) {
        var <- clima$PR13
      } else if (input$yearLISA == 2014) {
        var <- clima$PR14
      } else if (input$yearLISA == 2015) {
        var <- clima$PR15
      } else {
        var <- clima$PR16    
      }
    }
    if (input$radiodos == 2) {
      if (input$yearLISA == 2004) {
        var <- clima$TMIN04
      } else if (input$yearLISA == 2005) {
        var <- clima$TMIN05 
      } else if (input$yearLISA == 2006) {
        var <- clima$TMIN06
      } else if (input$yearLISA == 2007) {
        var <- clima$TMIN07
      } else if (input$yearLISA == 2008) {
        var <- clima$TMIN08
      } else if (input$yearLISA == 2009) {
        var <- clima$TMIN09
      } else if (input$yearLISA == 2010) {
        var <- clima$TMIN10
      } else if (input$yearLISA == 2011) {
        var <- clima$TMIN11
      } else if (input$yearLISA == 2012) {
        var <- clima$TMIN12
      } else if (input$yearLISA == 2013) {
        var <- clima$TMIN13
      } else if (input$yearLISA == 2014) {
        var <- clima$TMIN14
      } else if (input$yearLISA == 2015) {
        var <- clima$TMIN15
      } else {
        var <- clima$TMIN16    
      }
    }
    if (input$radiodos == 3) {
      if (input$yearLISA == 2004) {
        var <- clima$TMED04
      } else if (input$yearLISA == 2005) {
        var <- clima$TMED05 
      } else if (input$yearLISA == 2006) {
        var <- clima$TMED06
      } else if (input$yearLISA == 2007) {
        var <- clima$TMED07
      } else if (input$yearLISA == 2008) {
        var <- clima$TMED08
      } else if (input$yearLISA == 2009) {
        var <- clima$TMED09
      } else if (input$yearLISA == 2010) {
        var <- clima$TMED10
      } else if (input$yearLISA == 2011) {
        var <- clima$TMED11
      } else if (input$yearLISA == 2012) {
        var <- clima$TMED12
      } else if (input$yearLISA == 2013) {
        var <- clima$TMED13
      } else if (input$yearLISA == 2014) {
        var <- clima$TMED14
      } else if (input$yearLISA == 2015) {
        var <- clima$TMED15
      } else {
        var <- clima$TMED16    
      }
    }
    else {
      if (input$yearLISA == 2004) {
        var <- clima$TMAX04
      } else if (input$yearLISA == 2005) {
        var <- clima$TMAX05 
      } else if (input$yearLISA == 2006) {
        var <- clima$TMAX06
      } else if (input$yearLISA == 2007) {
        var <- clima$TMAX07
      } else if (input$yearLISA == 2008) {
        var <- clima$TMAX08
      } else if (input$yearLISA == 2009) {
        var <- clima$TMAX09
      } else if (input$yearLISA == 2010) {
        var <- clima$TMAX10
      } else if (input$yearLISA == 2011) {
        var <- clima$TMAX11
      } else if (input$yearLISA == 2012) {
        var <- clima$TMAX12
      } else if (input$yearLISA == 2013) {
        var <- clima$TMAX13
      } else if (input$yearLISA == 2014) {
        var <- clima$TMAX14
      } else if (input$yearLISA == 2015) {
        var <- clima$TMAX15
      } else {
        var <- clima$TMAX16    
      }
    }   
 ````
 
 
 
 #### También se crea la opción de tipos de vencindades que se utlizará para el análisis LISA, donde se considera distancia, vecinos más cercanos, queen (reyna) y torre, donde estos últimos dos fueron establecidos al inicio del cógido: 
 
 ````R
        # Distance-based spatial weights
    knn <- reactive({
      if (input$radio == 3) {
        k <- knearneigh(coordinates(clima), k = input$knn_slider, longlat = TRUE)
        return(k$nn)
      } else {
        return(NULL)
      }
    })
    
    dist <- reactive({
      if (input$radio == 4) {
        return(dnearneigh(coordinates(clima), 0, input$dist_slider, longlat = TRUE))
      } else {
        return(NULL)
      }
    })
 ````



#### Se crea una serie de condicionantes usando `if` y `else if`, esto para desplegar el mapa del análisis LISA de acuerdo con el tipo de vecindad seleccionadao


 ````R
    clima_weights <- reactive({
      if (input$radio == 1) {
        return(nb2listw(poly2nb(clima)))
      } else if (input$radio == 2) {
        return(nb2listw(poly2nb(clima, queen = FALSE)))
      } else if (input$radio == 3) {
        k <- knearneigh(coordinates(clima), k = input$knn_slider, longlat = TRUE)
        return(nb2listw(knn2nb(k)))
      } else if (input$radio == 4) {
        d <- dnearneigh(coordinates(clima), 0, input$dist_slider, longlat = TRUE)
        return(nb2listw(d))
      }
    })
 ````
 
#### Se realiza la impresión del mapa y elementos caracteristicos del análisis LISA
 
 ````R
     clima$var <- var
    clima$var_lag <- lag.listw(clima_weights(), var)
    
    varStd <- (var - mean(var))/sd(var)
    lagStd <- lag.listw(clima_weights(), scale(var))
    
    mean <- mean(var)
    lag_mean <- mean(clima$var_lag) 
    
    global_moran <- moran.test(var, clima_weights())
    statistic <- (global_moran$estimate)
    statistic <- round(statistic, 2)
    lisa <- localmoran(var, clima_weights())
    
    clima$cuadrante <- c(rep(0,length(var)))
    significance <- 0.05
    vec <- ifelse(lisa[,5] < significance, 1,0)
    
    clima$cuadrante[var >= mean & clima$var_lag >= lag_mean]  <- 1
    clima$cuadrante[var < mean & clima$var_lag < lag_mean]  <- 2
    clima$cuadrante[var < mean & clima$var_lag >= lag_mean]  <- 3
    clima$cuadrante[var >= mean & clima$var_lag < lag_mean]  <- 4
    clima$cuadrante.data <- clima$cuadrante*vec
    
    clima$cuadrante.col[clima$cuadrante.data==1] <- "Alto-Alto"
    clima$cuadrante.col[clima$cuadrante.data==2] <- "Bajo-Bajo"
    clima$cuadrante.col[clima$cuadrante.data==3] <- "Bajo-Alto"
    clima$cuadrante.col[clima$cuadrante.data==4] <- "Alto-Bajo"
    clima$cuadrante.col[clima$cuadrante.data==0] <- "No-signif"
    
    clima$fill <- factor(clima$cuadrante.data+1)
    clima$var_mean <- mean
    clima$var_lag_mean <- lag_mean
    clima$statistic <- statistic[1]
    
    clima.sel <- subset(clima, select = c(CVE_ENT, ADMIN_NAME, var, var_lag, cuadrante, cuadrante.data,
                                        cuadrante.col, fill, var_mean, var_lag_mean, statistic))
  })

  x <- reactive({
  if (input$radiodos == 1) {
    x <- "\nPrecipitación total promedio por Entidad Federativa"
  } else if (input$radiodos == 2) {
    x <- "\nTemperatura mínima promedio por Entidad Federativa"
  } else if (input$radiodos == 3) {
    x <- "\nTemperatura promedio por Entidad Federativa"
  } else {
    x <- "\nTemperatura máxima promedio por Entidad Federativa"
  }
  })

  y <- reactive({
  if (input$radiodos == 1) {
    y <- "\nRetraso espacial de la precipitación total promedio por Entidad Federativa"  
  } else if (input$radiodos == 2) {
    y <- "\nRetraso espacial de la temperatura mínima promedio por Entidad Federativa"  
  } else if (input$radiodos == 3) {
    y <- "\nRetraso espacial de la temperatura promedio por Entidad Federativa"
  } else {
    y <- "\nRetraso espacial de la temperatura máxima promedio por Entidad Federativa" 
  }
})

  output$moranPlot <- renderPlot({
    cColors <- c(rgb(0.74, 0.74, 0.74, alpha = 0.2), rgb(1, 0, 0, alpha = 0.75),
                 rgb(0, 0, 1, alpha = 0.75), rgb(0.58, 0.58, 1, alpha = 0.75), rgb(1, 0.58, 0.58, alpha = 0.75))
    ggplot(selected()@data, aes(x=var, y=var_lag)) +
      geom_point(aes(fill = selected()$fill), colour="black", size = 3, shape = 21)+
      scale_fill_manual(name="",
                        values = c("1" = cColors[1], "2" = cColors[2], "3" = cColors[3], "4" = cColors[4], "5" =cColors[5]),
                        labels=c("No-signif",
                                 paste0("Alto-Alto (", sum(selected()$cuadrante.data==1), ")"),  
                                 paste0("Bajo-Bajo (", sum(selected()$cuadrante.data==2), ")"),
                                 paste0("Bajo-Alto (", sum(selected()$cuadrante.data==3), ")"),
                                 paste0("Alto-Bajo (", sum(selected()$cuadrante.data==4), ")"))) +
      geom_vline(xintercept = unique(selected()$var_mean), colour = "grey", linetype = "longdash") +
      geom_hline(yintercept = unique(selected()$var_lag_mean), colour = "grey", linetype = "longdash") +
      stat_smooth(method="lm", se=FALSE, colour = "black", size = 0.5) +
      xlab(x()) +
      ylab(y()) +
      theme_bw() +
      ggtitle(paste0("I de Moran: ", unique(selected()$statistic),"\n")) +
      theme(plot.title = element_text(color = "darkorchid")) 
  })
    
 ````
 
 
 
#### Después de haber seleccionado la opción de vecindad y el tipo de variable, se hace la impresión del mapa de autocorrelación espacial usando un mapa base del servidor leaflet
 
 
 ````R
 
   output$LISAmap <- renderLeaflet({
    cColors <- c(rgb(0.74, 0.74, 0.74), rgb(1, 0, 0),
                 rgb(0, 0, 1), rgb(0.58, 0.58, 1), rgb(1, 0.58, 0.58))
    factpal <- colorFactor(cColors, 
                           domain = c("0", "1", "2", "3", "4"))
    
    popup <- paste(selected()$ADMIN_NAME,":", selected()$var, etiqu(), input$yearLISA)
    leaflet(clima) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(data = selected(), fillColor = ~factpal(cuadrante.data), 
                  fillOpacity = 0.7, color = "black", weight = 1, label = popup) %>%
      addLegend(position = "topright", colors = cColors,
                labels = c("No-signif", "Alto-Alto", "Bajo-Bajo", "Bajo-Alto", "Alto-Bajo"), opacity = 0.7)
  })
  
  brushed <- eventReactive(input$plot_brush, {
    brushedPoints(selected(), input$plot_brush)
  })

  output$table <- DT::renderDataTable({
    tbl <- brushed() %>%   
      as.data.frame() %>% 
      select("Entidad Federativa" = ADMIN_NAME, "Variable evaluada en mm o en °C" = var, "Tipo de Cluster" = cuadrante.col)
  },
  rownames = FALSE, options = list(pageLength = 5, dom = 'tip', autoWidth = FALSE))
  
  observe({
    req(brushed())
    popup <- paste(brushed()$ADMIN_NAME, ":", brushed()$var, etiqu(), input$yearLISA)
    leafletProxy('LISAmap') %>%
      clearGroup(group = 'brushed') %>%
      addPolygons(data = brushed(), fill = "#9cf", color = '#036', weight = 1.5,
                  opacity = 0.5, group = 'brushed', label = popup)
  })
  
})

````
 
 
### ACCESO A LOS CODIGOS FUENTES 
[server]

[ui]


[server]: https://github.com/GAMM1031/PFinal_Equipo4/blob/master/codigos/server.R

[ui]: https://github.com/GAMM1031/PFinal_Equipo4/blob/master/codigos/ui.R

:black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle::black_circle:


#   					 D3

D3 ( Data-Driven Documents o D3.js ) es una biblioteca de JavaScript para visualizar datos utilizando estándares web. Ayuda a llevar los datos usando SVG, CANVAS y HTML. También combina poderosas técnicas de visualización e interacción con un enfoque basado en datos para la manipulación DOM, ofreciéndole las capacidades completas de los navegadores modernos y la libertad de diseñar la interfaz visual adecuada para sus datos.



### Para comenzar la representación en D3, se esta utilizando el lenguaje HTML, para lo cual se definió una conjunto de paletas de colores que serviran para representar las variables de precipitación y temperatura (media, mínima y máxima).



````html

<!DOCTYPE html>
<head>
  <meta charset="UTF-8">
  <title>Final D3 - DGC, GMM, IJVC</title>
<style>
/*Los colores para las clases de población, rojos*/
  .q0 { fill:#fcc383; }
  .q1 { fill:#fc9f67; }
  .q2 { fill:#f4794e; }
  .q3 { fill:#e65338; }
  .q4 { fill:#ce2a1d; }

  .p0 { fill:#B6EDF0; }
  .p1 { fill:#74B4E8; }
  .p2 { fill:#1F83E0; }
  .p3 { fill:#1D44B8; }
  .p4 { fill:#090991; }


  #climatica{
    position: absolute;
    top: 125px;
    left: 810px;
  }

  #anho{
    position: absolute;
    top: 125px;
    left: 1110px;
  }

  .background {
    fill: none;
    pointer-events: all;
  }

  #estados .active {
    stroke: black;
  }

  #bars {
    position: absolute;
    margin-top: 150px;
    margin-left: 5px;
  }

  .bar text {
    fill: black;
    font: 13px verdana;
    font-weight: bold;
    text-anchor: right;
  }

  #titulo {
    position: absolute;
    top: 160px;
    left: 810px;
    font: 17px verdana;
    font-weight: bold;
  }


</style>
</head>
````

###  Se agrega un tema de fondo


````html
<body background="Fondo.png">
<h2 align="center"> <font size="5" color="black" face = "garamond">
````

### De igual manera se crean dos selectores que corresponden a la variable climática y al año, así tener mejor uso en la visualización. 

````html  
  MAPA DE REPRESENTACIÓN DE VARIABLES CLIMÁTICAS POR ESTADO POR AÑO</h2>
  <select id="climatica" class="select">
    <option value="PR" selected>Precipitación media anual (PR)</option>
    <option value="TMIN">Temperatura mínima promedio anual (TMIN)</option>
    <option value="TMED">Temperatura promedio anual (TMED)</option>
    <option value="TMAX">Temperatura máxima promedio anual (TMAX)</option>
  </select>
  <select id="anho", class="select">
    <option value="04" selected>2004</option>
    <option value="05">2005</option>
    <option value="06">2006</option>
    <option value="07">2007</option>
    <option value="08">2008</option>
    <option value="09">2009</option>
    <option value="10">2010</option>
    <option value="11">2011</option>
    <option value="12">2012</option>
    <option value="13">2013</option>
    <option value="14">2014</option>
    <option value="15">2015</option>
    <option value="16">2016</option>
  </select>
  <div id="titulo"></div>
</body>
````



### Para hacer un mapa en D3, se necesita dos librerías de JavaScrip, que son: D3 y topojson. La primera es la base de D3 y la segunda es una extensión para manejar datos de tipo topoJSON. 

- Se define una proyección, coordenadas del centro del mapa y el nivel de zoom.

- Lo primero que vamos a hacer es leer los datos y dibujar un mapa pintando todos los polígonos de un mismo color.

- Se desglosa los _features_ del topoJSON ` features = topojson.feature(datos, datos.objects.clima); `




````html

  <script src="https://d3js.org/d3.v4.min.js"></script>
  <script src="https://unpkg.com/topojson@3"></script>
  <script src="https://d3js.org/d3-format.v1.min.js"></script>
  <script>
   var format = d3.format(".2f"); </script>
  <script>

    var features, nest, bar;

    var width = 800,
        height = 600,
        active = d3.select(null);

    var projection = d3.geoMercator()
                       .scale(1400)
                       .center([-102.584065, 23.62755])
                       .translate([width/2, height/2]);

    var select = d3.selectAll(".select");

    var svg = d3.select("body").append("svg")
                .attr("width", width)
                .attr("height", height);

    svg.append("rect")
       .attr("class", "background")
       .attr("width", width)
       .attr("height", height)
       .on("click", reset);

    var g = svg.append("g")
               .attr("id", "estados");

    var barSvg = d3.select("body").append("svg")
               .attr("id", "bars")
               .attr("height", 200)
               .attr("width", 400);

    var path = d3.geoPath().projection(projection);

    d3.json('clima.json', function(error, datos) {

        features = topojson.feature(datos, datos.objects.clima);

        var propiedades = features.features.map(function(ADMIN_NAME) {return ADMIN_NAME.properties;});
        nest = d3.nest()
          .key(function(d) { return d.ADMIN_NAME; })
          .rollup(function(values) {
            return {
              PR04: +d3.values(values)[0]['PR04'],
              PR05: +d3.values(values)[0]['PR05'],
              PR06: +d3.values(values)[0]['PR06'],
              PR07: +d3.values(values)[0]['PR07'],
              PR08: +d3.values(values)[0]['PR08'],
              PR09: +d3.values(values)[0]['PR09'],
              PR10: +d3.values(values)[0]['PR10'],
              PR11: +d3.values(values)[0]['PR11'],
              PR12: +d3.values(values)[0]['PR12'],
              PR13: +d3.values(values)[0]['PR13'],
              PR14: +d3.values(values)[0]['PR14'],
              PR15: +d3.values(values)[0]['PR15'],
              PR16: +d3.values(values)[0]['PR16'],
              TMIN04: +d3.values(values)[0]['TMIN04'],
              TMIN05: +d3.values(values)[0]['TMIN05'],
              TMIN06: +d3.values(values)[0]['TMIN06'],
              TMIN07: +d3.values(values)[0]['TMIN07'],
              TMIN08: +d3.values(values)[0]['TMIN08'],
              TMIN09: +d3.values(values)[0]['TMIN09'],
              TMIN10: +d3.values(values)[0]['TMIN10'],
              TMIN11: +d3.values(values)[0]['TMIN11'],
              TMIN12: +d3.values(values)[0]['TMIN12'],
              TMIN13: +d3.values(values)[0]['TMIN13'],
              TMIN14: +d3.values(values)[0]['TMIN14'],
              TMIN15: +d3.values(values)[0]['TMIN15'],
              TMIN16: +d3.values(values)[0]['TMIN16'],
              TMED04: +d3.values(values)[0]['TMED04'],
              TMED05: +d3.values(values)[0]['TMED05'],
              TMED06: +d3.values(values)[0]['TMED06'],
              TMED07: +d3.values(values)[0]['TMED07'],
              TMED08: +d3.values(values)[0]['TMED08'],
              TMED09: +d3.values(values)[0]['TMED09'],
              TMED10: +d3.values(values)[0]['TMED10'],
              TMED11: +d3.values(values)[0]['TMED11'],
              TMED12: +d3.values(values)[0]['TMED12'],
              TMED13: +d3.values(values)[0]['TMED13'],
              TMED14: +d3.values(values)[0]['TMED14'],
              TMED15: +d3.values(values)[0]['TMED15'],
              TMED16: +d3.values(values)[0]['TMED16'],
              TMAX04: +d3.values(values)[0]['TMAX04'],
              TMAX05: +d3.values(values)[0]['TMAX05'],
              TMAX06: +d3.values(values)[0]['TMAX06'],
              TMAX07: +d3.values(values)[0]['TMAX07'],
              TMAX08: +d3.values(values)[0]['TMAX08'],
              TMAX09: +d3.values(values)[0]['TMAX09'],
              TMAX10: +d3.values(values)[0]['TMAX10'],
              TMAX11: +d3.values(values)[0]['TMAX11'],
              TMAX12: +d3.values(values)[0]['TMAX12'],
              TMAX13: +d3.values(values)[0]['TMAX13'],
              TMAX14: +d3.values(values)[0]['TMAX14'],
              TMAX15: +d3.values(values)[0]['TMAX15'],
              TMAX16: +d3.values(values)[0]['TMAX16']
            };
          })
          .entries(propiedades);

        select.on("change", function(d) {
           var interes = "";
           d3.selectAll(".select").each(function(d,i){ return interes+=this.value;});
           hazMapa(interes);
        });
        var interes = "";
           d3.selectAll(".select").each(function(d,i){ return interes+=this.value;});
           hazMapa(interes);
    });

````


#### - Se realiza el despligue o impresión del mapa de acuerdo a la variable seleccionada.
#### - Se hace uso de `update` para actualizar los datos y colores en el mapa.




````html

    function hazMapa(interes){

        var max = d3.max(features.features, function(d) { return d.properties[interes]; })

        var quantizepr = d3.scaleQuantize()
                         .domain([0, max])
                         .range(d3.range(5).map(function(i) { return "p" + i; }));

        var quantize = d3.scaleQuantize()
                       .domain([0, max])
                       .range(d3.range(5).map(function(i) { return "q" + i; }));

         var mapUpdate = g.selectAll("path")
                          .data(features.features);

         var mapEnter = mapUpdate.enter();

         mapEnter.append("path")
                 .merge(mapUpdate)
                 .attr("d", path)
                 .attr("class", function(d){
                   if (d.properties[interes] > 50) {
                     return quantizepr(d.properties[interes]) }
                   else { return quantize(d.properties[interes]) }
                 })
                 .on("click", clicked);
    }

    d = [{nombre: "PR", datos: "0"}, {nombre: "TMIN", datos: "0"}, {nombre: "TMED", datos: "0"}, {nombre: "TMAX", datos: "0"}];
    var colores = {"PR": "steelblue", "TMIN": "#fcc383", "TMED": "#f4794e", "TMAX": "#ce2a1d"};

    var barWidth = 300,
      barHeight = 35;
    var x = d3.scaleLinear()
            .range([0, barWidth])
            .domain([0, 300]);

    bar = barSvg.selectAll(".bar")
              .data(d);

    var barEnter = bar.enter()
      .append("g")
      .attr("class", "bar")
      .attr("transform", function(d, i) { return "translate(0," + i * barHeight + ")"; })

    d3.select("#titulo").html("Variables climáticas a representar");

    barEnter.append("rect")
        .transition()
        .duration(500)
        .attr("width", 0 )
        .attr("height", barHeight - 3)
        .attr("fill", function(d) { return colores[d.nombre]; });

    barEnter.append("text")
          .transition()
          .duration(500)
          .attr("x", 0)
          .attr("y", barHeight / 2)
          .attr("dy", ".35em")
          .text(function(d) { return d.valor});

    barEnter.append("text")
          .transition()
          .duration(500)
          .attr("x", 10)
          .attr("y", barHeight / 2)
          .attr("dy", ".35em")
          .text(function(d) { return d.nombre });

    bar.select("rect")
     .transition()
     .duration(500)
     .attr("width", 0);

    bar.select("text")
     .transition()
     .duration(500)
     .attr("x", 0)
     .text(function(d) { d3.format(".2f")(x(d.valor)); });


````


### Se agrega una gráfica que este ligada a los datos y al mapa. 



````html
    function hazGrafica(anho, estado){
        var climaticos = ['PR', 'TMIN', 'TMED', 'TMAX'];
        var datos = [];
        var f = [];

        for (i = 0; i <= 3 ; i++){
            c = {};
            c["nombre"] = climaticos[i];
            c["valor"] = estado[0].value[climaticos[i]+anho];
            datos.push(c);
        }

        var barWidth = 300,
              barHeight = 35;
          var x = d3.scaleLinear()
                    .range([0, barWidth])
                    .domain([0, 300]);

          bar = barSvg.selectAll(".bar")
                      .data(datos, function(d){ return d.nombre;});

          var barEnter = bar.enter()
              .append("g")
              .attr("class", "bar")
              .attr("transform", function(d, i) { return "translate(0," + i * barHeight + ")"; })

          d3.select("#titulo").html("Variables climáticas del Estado de " + estado[0].key.substr(0,20) + " 20" + anho);

          barEnter.append("rect")
              .transition()
              .duration(500)
              .attr("width", function(d) { return x(d.valor); })
              .attr("height", barHeight - 3)
              .attr("fill", function(d) { return colores[d.nombre]; });

          barEnter.append("text")
                  .transition()
                  .duration(500)
                  .attr("x", function(d) { return x(d.valor) + 35; })
                  .attr("y", barHeight / 2)
                  .attr("dy", ".35em")
                  .text(function(d) { return d.valor});

          barEnter.append("text")
                  .transition()
                  .duration(500)
                  .attr("x", 10 )
                  .attr("y", barHeight / 2)
                  .attr("dy", ".35em")
                  .text(function(d) { return d.nombre });

          bar.select("rect")
             .transition()
             .duration(500)
             .attr("width", function(d) {
               if ( d.valor > 50) {
                 return x(d.valor)/8}
               else { return x(d.valor) * 4 }
            });

          bar.select("text")
             .transition()
             .duration(500)
             .attr("x", function(d) {
               if (d.valor > 50) {
                 return x(d.valor)/8 + 35}
               else { return (x(d.valor) * 4) + 35 }
             })
             .text(function(d) {
               if ( d.valor > 50) {
                 return x(d.valor) + " mm de lámina de lluvia"}
               else { return d3.format(".2f")(x(d.valor)) + " °C" }
            });

      }

````



### Se agrega la función  `clicked` que servirá para hacer un zoom, dependiendo del polígono donde se de "click", posteriormente la función ` reset `  servirá para regresar al zoom de la vista original, esto se podrá realizar al hacer doble click dentro de un mismo poligono.


````html
    function clicked(d) {
        if (active.node() === this) return reset();
        active.classed("active", false);
        active = d3.select(this).classed("active", true);

        var bounds = path.bounds(d),
        dx = bounds[1][0] - bounds[0][0],
        dy = bounds[1][1] - bounds[0][1],
        x = (bounds[0][0] + bounds[1][0]) / 2,
        y = (bounds[0][1] + bounds[1][1]) / 2,
        scale = .9 / Math.max(dx / width, dy / height),
        translate = [width / 2 - scale * x, height / 2 - scale * y];

        g.transition()
         .duration(750)
         .style("stroke-width", 1.5 / scale + "px")
         .attr("transform", "translate(" + translate + ")scale(" + scale + ")");

        var estado = nest.filter(function(a) {
              return a.key == d.properties.ADMIN_NAME;
           });

        var anho = d3.select("#anho").node().value;

        hazGrafica(anho, estado);

   }

   function reset() {
        active.classed("active", false);
        active = d3.select(null);

        g.transition()
         .duration(750)
         .style("stroke-width", "1.5px")
         .attr("transform", "");

    }
````



### ACCESO AL CÓDIGO HTML
[D3]


[D3]: https://github.com/GAMM1031/PFinal_Equipo4/blob/master/D3/Final_D3.html
