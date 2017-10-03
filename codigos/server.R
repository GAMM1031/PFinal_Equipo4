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

#font_import()
#y
#loadfonts(device = "win")

clima <- readOGR("clima.shp", verbose = FALSE, stringsAsFactors = FALSE, GDAL1_integer64_policy = T)
# Los vecinos de todos los estados QUEEN
clima.nbq <- poly2nb(clima, queen = T)
clima.nbq.w <- nb2listw(clima.nbq)
# Los vecinos de todos los estados ROOK
clima.nbr <- poly2nb(clima, queen = F)
clima.nbr.w <- nb2listw(clima.nbr)
# Las coordenadas de los centroides de los estados
coords <- coordinates(clima)

##################################
# Pestaña 1: Mapa y tabla dinámica
##################################

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
      etiq = "Precipitación total registrada en el año"
    } else if (input$rad == 2) {
      etiq = "Temperatura mínima promedio en el año"
    } else if (input$rad == 3) {
      etiq = "Temperatura promedio en el año"
    }  else {
      etiq = "Temperatura máxima promedio en el año"
    }
  })
  
  et <- reactive({
    if (input$rad == 1) {
      et = "en mm de lámina de lluvia"
    }  else {
      et = "en °C"
    }
  })
  
  plt <- reactive({
    if (input$rad == 1) {
      plt = "Blues"
    }  else {
      plt = "YlOrRd"
    }
  })
  
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
        a <- paste("Temperatura mínima promedio anual del estado de", plot.total$ADMIN_NAME)
      } else if (input$rad == 3) {
        a <- paste("Temperatura promedio anual del estado de", plot.total$ADMIN_NAME)
      } else {
        a <- paste("Temperatura máxima promedio anual del estado de", plot.total$ADMIN_NAME)
      }
    })
    
    b <- reactive({
      if (input$rad == 1) {
        b <- "Lámina de lluvia en mm"
      } else {
        b <- "Temperatura en °C"
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
  

##################################
# Pestaña 2: Mapa LISA
##################################

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

    
    # LISA Map
    clima_weights <- reactive({
      if (input$radio == 1) {
        #return(nb2listw(include.self(poly2nb(clima)))), si se considera auto-vecindad
        return(nb2listw(poly2nb(clima)))
      } else if (input$radio == 2) {
        #return(nb2listw(include.self(poly2nb(clima, queen = FALSE)))), si se considera auto-vecindad
        return(nb2listw(poly2nb(clima, queen = FALSE)))
      } else if (input$radio == 3) {
        k <- knearneigh(coordinates(clima), k = input$knn_slider, longlat = TRUE)
        #return(nb2listw(include.self(knn2nb(k)))), si se considera auto-vecindad
        return(nb2listw(knn2nb(k)))
      } else if (input$radio == 4) {
        d <- dnearneigh(coordinates(clima), 0, input$dist_slider, longlat = TRUE)
        #return(nb2listw(include.self(d))), si se considera auto-vecindad
        return(nb2listw(d))
      }
    })
    
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
  
  etiqu <- reactive({
    if (input$radiodos == 1) {
      etiq = "mm de lámina de lluvia en el año "
    } else if (input$radiodos == 2) {
      etiq = "°C de temperatura mínima promedio en el año"
    } else if (input$radiodos == 3) {
      etiq = "°C de temperatura promedio en el año"
    }  else {
      etiq = "°C de temperatura máxima promedio en el año"
    }
  })
  
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