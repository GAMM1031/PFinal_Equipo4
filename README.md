# PFinal_Equipo     
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

