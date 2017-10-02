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

![img](/img/img4.png)


![img](/img/img5.png)


Mapa en leaflet del resultado de la I de Moran según el tipo de autocorrelación, además se representa la etiqueta que resulta de seleccionar con el mouse uno de las entidades, mostrando la entidad federativa seguido de los valores de la variable seleccionada para el año evaluado.

![img](/img/img6.png)


![img](/img/img7.png)

La siguiente imagen representa un ejemplo del resultado obtenido de manera conjunta
En este módulo del proyecto es posible obtener dada la selección del tipo de vecindad que se quiera evaluar, y para la selección de la variable objeto de estudio, el valor del índice de moran y los niveles de agrupamiento (dispersión) de la misma dado el año seleccionado.

En el ejemplo de la figura se muestran los resultados para una vecindad de reina, la variable de estudio es la precipitación total anual para el año 2004, en los resultados es posible apreciar que se obtuvo un valor de I de Moran = 0.36, siendo significativa la formación de clúster alto-alto para los estados de Yucatán (32.82mm), Quintana Roo (31.89mm), Campeche (32.62mm) y Tabasco (32.53mm), para el caso de la formación de clúster bajo-bajo son significativos los resultados para el Estado de México (23.12mm) e Hidalgo (23,93mm), el resto de los estados sus valores son no-significativos. 

![img](/img/img8.png)
