library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(stringr)
library(plotly)
library(DT)
library(bslib)
library(readr)
library(data.table)
library(leaflegend)
library(shinyBS)
library(shinyWidgets)

# source helpers ----------------------------------------------------------
lapply(list.files(path = "src",recursive = TRUE, full.names = TRUE), source)

# enviroscreen data
envoData <- readRDS("data/scores/allScores4spanish.rds")%>%
  dplyr::mutate(visParam = `Percentil del puntaje de Colorado EnviroScreen`)%>%
  dplyr::select("Nombre del condado", "GEOID", everything())%>% 
  dplyr::select(-"GEOID3")


# Additional Data   
oil <- readRDS("data/scores/oilgasVis.rds") 
coal <- readRDS("data/scores/coalVis.rds")
rural <- readRDS("data/scores/ruralVis.rds")
descriptors <- read_csv("data/descriptions/indicatorDesc.csv") %>% 
  dplyr::select(1:6)%>%
  `colnames<-`(c("Indicator", "Source", "Date", "Units", "Measured_Geography", "Description"))
justice40 <- readRDS("data/scores/justice40.rds") %>%
  dplyr::mutate(popup = paste0(
    "Census Tract ", GEOID ," in ", County_Name," County."    
    ,br()
    ,"Se definió como área desfavorecida según un total de ", Total.threshold.criteria.exceeded," indicadores."
    ,br()
    ,br()
    ,paste0("Lea la definición de comunidades de ", 
            tags$a(href = "https://screeningtool.geoplatform.gov/en/about", 
                   "Justice40 en la herramienta de evaluación de justicia climática y económica.", target = "_blank")  
    )
  )
)

# di community 
di <- getDI()
# storyMap Locations 
sm <- getStoryMaps()

# palette for DI layer 
diPal <- colorFactor(palette = c(
  "#a6cee3", "#33a02c","#b2df8a","#1f78b4"), levels = c("Low Income", "People of Color",
                                                        "Housing Burden", "More then one category"), di$color
)

### dark as low 
colorRamp <- c( "#54278f","#756bb1","#9e9ac8","#cbc9e2","#f2f0f7")


# create initial dataset for map  -----------------------------------------
mapData <-   envoData %>%
  dplyr::filter(area == "Condado")%>%
  dplyr::select(GEOID,"Puntaje de Colorado EnviroScreen", 
                "Percentil del puntaje de Colorado EnviroScreen", 
                "Nombre del condado",
                visParam,
                "Comunidad con carbón",
                "Comunidad con petróleo y gas",
                "Comunidad rural")%>%
  dplyr::mutate(
    popup = paste0(
      "<br/><strong>Puntaje de Colorado EnviroScreen</strong>", # needs to be text
      paste0("<br/><strong>",`Nombre del condado`,"</strong>"),
      paste0("<br/><b>Medino:</b> ", round(`Puntaje de Colorado EnviroScreen`, digits = 2),
             "<br/><b>Puntaje:</b> ", as.character(round(`Percentil del puntaje de Colorado EnviroScreen`), digits =  0)),
      paste0("<br/><b>Comunidad con carbón:</b> ", `Comunidad con carbón`),
      paste0("<br/><b>Comunidad con petróleo y gas:</b> ", `Comunidad con petróleo y gas`),
      paste0("<br/><b>Comunidad rural:</b> ", `Comunidad rural`)
    )
  ) %>% as("sf")

#palette for the map
palMap <- leaflet::colorNumeric(palette = colorRamp,
                                domain = mapData$visParam,
                                na.color = "#808080",
                                reverse = TRUE
)


# unique Indicators
indicators <- sf::st_drop_geometry(envoData) %>%
  dplyr::select(-c(`Nombre del condado`, area, GEOID)) %>%
  dplyr::select(!starts_with("Percentil "))%>%
  names()

#hist data
histData <- envoData %>%
  sf::st_drop_geometry()%>%
  dplyr::filter(area == "County")%>%
  dplyr::select(
    "GEOID"
    ,"Puntaje de Colorado EnviroScreen"
    ,"Contaminación y carga climática"
    ,"Factores de salud y sociales"
    ,"Exposiciones ambientales"
    ,"Efectos ambientales"
    ,"Vulnerabilidad climática"
    ,"Poblaciones sensibles"
    ,"Características demográficas"
  )

# set empty parameter for histogram funciton 
## set to non GEOID number for the histogram generate on loading. 
geoidMap <- "100"


ui <- fluidPage(
  tags$head(includeHTML(("GoogleAnalytics.html"))),
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    #bg = "#FFFFFF",
    #fg = "#000",
    primary = "#245d38",# green 
    secondary = "#001970", #blue 
    success = "#245d38", # green
    base_font = "Trebuchet MS,Helvetica,sans-serif",
    heading_font = "Trebuchet MS,sans-serif"
  )%>%
    bslib::bs_add_rules(sass::sass_file("www/style.scss")),
  # Title ------------------------------------------------------------------
  fluidRow(
    class = "titleElement",
    column(4,
           tags$br(), 
           tags$a(
             href = "https://cdphe.colorado.gov/enviroscreen",
             tags$img(
               src="EnviroScreen Logos/co_cdphe_pr_es_white_v.png",
               title = "Colorado Department of Public Health and Environment",
               width="70%",
               height="auto" 
             )
           )
    ),
    column(8, h1("Colorado EnviroScreen"), p("Junio de 2022"))
  ),
  br(),
  fluidRow(
    p(HTML("</br><a href='#map'>Saltar al mapa</a>")),
    p("Colorado EnviroScreen es un mapa interactivo de justicia ambiental y una herramienta de evaluación de la salud en Colorado. Un equipo de Colorado State University creó esta herramienta para el Departamento de Salud Pública y Medio Ambiente de Colorado (CDPHE). Se lanzó la versión 1.0 de Colorado EnviroScreen el 28 de junio de 2022. Para obtener más información sobre Colorado EnviroScreen, diríjase a la ",
      tags$a(href = "https://cdphe.colorado.gov/enviroscreen", "página web de Colorado EnviroScreen ", target = "_blank"),
      "del CDPHE. Puede enviar sus comentarios sobre Colorado EnviroScreen por correo electrónico al CDPHE: cdphe_ej@state.co.us"
    )
  ) ,
  fluidRow(
    p("Colorado EnviroScreen:"),
    p(
      tags$ul(
        tags$li("identifica las áreas en las que hay y ha habido inequidades ambientales;"),
        tags$li("detalla las áreas en las que la carga para la salud y/o los riesgos ambientales son mayores para las comunidades afectadas de manera desproporcionada;"),
        tags$li("identifica la ubicación geográfica de las comunidades afectadas de manera desproporcionada según la definición de la Ley de Justicia Ambiental (Ley 21-1266 de la Cámara de Representantes).")
      )
    )
  ),
  fluidRow(
    p("El propósito de Colorado EnviroScreen es:"),
    p(
      tags$ul(
        tags$li("ayudar a los usuarios a que intercedan para recibir fondos, intervenciones y cambios en las políticas con el fin de evitar, disminuir y mitigar los riesgos para la salud ambiental;"),
        tags$li("fomentar un estilo de vida saludable y sostenible en Colorado y que todas las personas reciban el mismo grado de protección de los peligros ambientales y para la salud.")
      )
    )
  ),
  fluidRow(
    p("Haga clic aquí para obtener más información sobre el trabajo que lleva a cabo el CDPHE con el fin de ",
      tags$a(href = "https://cdphe.colorado.gov/environmental-justice", "fomentar la justicia ambiental", target = "_blank"),
      ", ",
      tags$a(href = "https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology", "comprender la relación que existe entre el medio ambiente y nuestra salud", target = "_blank"),
      " , and ",
      tags$a(href = "https://cdphe.colorado.gov/air-pollution/climate-change#equity", "fomentar la equidad climática.", target = "_blank"),
    )),
  # title6(),
  
  # # description of use ------------------------------------------------------
  fluidRow(class = "sectionTitle",
           h2("Comprenda la herramienta EnviroScreen")
  ),
  # 
  tabsetPanel(
    tabPanel(title = "Propósito y limitaciones"
             ,br()
             ,p("Colorado EnviroScreen es una herramienta de mapeo de justicia ambiental que calcula un “puntaje de EnviroScreen” de acuerdo con factores medioambientales y de población.  Cuanto más alto es el puntaje de EnviroScreen, más alta es la probabilidad de que el área se vea afectada por injusticias de salud ambiental. ")
             ,p("La herramienta incluye un puntaje para cada uno de los condados, áreas censales y grupos de manzanas censales de Colorado. El Departamento de Salud Pública y Medio Ambiente de Colorado (CDPHE) mejorará y ampliará la herramienta de acuerdo con los comentarios que se reciban y a medida que surjan nuevos datos. Tenga presente que, en este momento, el mapa no incluye las zonas que están bajo jurisdicción de la tribu ute de la montaña Ute y la tribu ute del sur.")
             ,p("Pese a que EnviroScreen proporciona una sólida medida de la carga ambiental acumulativa, no es una herramienta perfecta. La herramienta usa una cantidad limitada de datos ambientales, sobre la salud y sociodemográficos para calcular el puntaje de EnviroScreen. ")
             ,p(
               tags$strong("Colorado EnviroScreen sí puede:")
               ,tags$ul(
                 tags$li("mostrar las áreas de Colorado donde es más probable que se produzcan injusticias de salud ambiental;"),
                 tags$li("identificar las áreas de Colorado en las que las agencias gubernamentales pueden dar prioridad a los recursos y trabajar para reducir la contaminación y otras fuentes de injusticias ambientales;"),
                 tags$li("proporcionar información a fin de empoderar a las comunidades para que intercedan para mejorar la salud pública y el medio ambiente;"),
                 tags$li("identificar las áreas que cumplen con la definición de “comunidad afectada de manera desproporcionada” de la Ley de Justicia Ambiental (Ley 21-1266 de la Cámara de Representantes)."),
               )
             )
             ,p(
               tags$strong("Colorado EnviroScreen no puede:"),
               tags$ul(
                 tags$li("definir qué ambientes son buenos o malos para la salud;"),
                 tags$li("establecer una relación de causa a efecto entre los riesgos ambientales y la salud; "),
                 tags$li("definir todas las áreas que podrían verse afectadas por las injusticias ambientales o los riesgos ambientales concretos;"),
                 tags$li("proporcionar información acerca del estado de salud o ambiente de una persona en particular;"),
                 tags$li("tomar en consideración todas las exposiciones ambientales;"),
                 tags$li("darnos información sobre las áreas más pequeñas que forman parte de los grupos de manzanas censales y que podrían ser más vulnerables a las exposiciones ambientales que otras zonas;"),
                 tags$li("proporcionar información sobre los riesgos para la salud de los seres no humanos o los ecosistemas."),
               )
             )
    ),
    tabPanel(title = "Cómo se usa el mapa",
             br()
             ,p("La vista por omisión del mapa muestra el estado de Colorado. Los condados, áreas censales y grupos de manzanas censales individuales están codificados por colores según su puntaje de EnviroScreen. La leyenda que aparece a la derecha del mapa explica el significado de los colores. Cuanto más oscuro es el color, peor es el puntaje de EnviroScreen. Los usuarios pueden acercar y alejar la vista, arrastrar el mapa a una ubicación diferente y hacer clic en un lugar para obtener más información sobre el puntaje de EnviroScreen de ese lugar en particular y cómo se calculó.")
             ,tags$img(
               id = "mapDesc",
               src="MapElements_3_crop.jpg",
               title = "Map Elements",
               height="auto"
             )
             ,tags$h3("Siga estos pasos para usar el mapa.")
             ,tags$h4("Paso 1: Elija la configuración del mapa.")
             ,p(
               tags$strong("En primer lugar, elija la “escala geográfica” que desea visualizar.")
               ,"el menú desplegable para ver el mapa a nivel de condados, áreas censales o grupos de manzanas censales. La división por condados es la escala más grande y el grupo de manzanas censales es la escala más pequeña. Continúe con el"
               ,tags$em(" “Indicador” del mapa.")
             )
             ,br()
             ,p(
               tags$strong("En segundo lugar, elija el “Indicador” que desea visualizar.")
               ," La capa por omisión que muestra el mapa es el puntaje general de EnviroScreen, que combina todos los indicadores de la herramienta en un solo puntaje. Haga clic en el menú desplegable y desplácese por las opciones para seleccionar otra capa, como el puntaje de los componentes agrupados o individuales. Continúe con "
               ,tags$em("“Medida o %”."))
             ,br()
             ,p(
               tags$strong("En tercer lugar, seleccione si desea visualizar el indicador “Medida o %”.")
               ," lija cómo desea ver la capa del mapa seleccionada. Valor medido muestra la medida real de la fuente de datos (p. ej., microgramos de contaminantes, casos de una enfermedad, etc.). La medida tiene mayor relevancia para las capas individuales de datos, como el ozono o las hospitalizaciones por asma. Un percentil es un rango o categoría. El número representa el porcentaje de lugares de Colorado cuyo rango es equivalente o está por debajo del rango del área seleccionada. Por ejemplo, un percentil de 80 en EnviroScreen significa que el 80 % de las áreas de Colorado tienen menos probabilidades de verse afectadas por injusticias de salud ambiental que el área en cuestión y que el 20 % de las áreas de Colorado tienen más probabilidades de verse afectadas por injusticias de salud ambiental.")
             ,br()
             ,p(
               "Una vez que haya seleccionado sus opciones, haga clic en "
               ,tags$strong("Actualizar mapa. ")
               ," El mapa podría tardar unos minutos en cargarse.")
             ,p(
               tags$em("Nota: el mapa, la figura y la tabla no se actualizarán con sus opciones si no hace clic en ")
               ,tags$strong(" Actualizar mapa")
               ,tags$em(" tras seleccionar la configuración. Para quitar el resaltado, haga clic en el botón")
               ,tags$strong(" Eliminar resaltado")
               ,tags$em(" que aparece a la derecha. Use la tabla de datos en la parte inferior de la herramienta para resaltar las distintas áreas del mapa.")
             ),
             tags$h4("Paso 2: Interactúe con el mapa."),
             p(
               "Los condados, sectores censales y grupos de bloques censales individuales están codificados por colores según la capa del mapa que elija. La leyenda que aparece a la derecha del mapa explica el significado de los colores. Cuanto más oscuros son los colores, más altos son los valores y, cuanto más altos son los valores, peor es el puntaje de EnviroScreen. Use los iconos que aparecen en la esquina superior izquierda del mapa para acercar y alejar la vista, buscar una dirección, centrar el mapa, elegir el color de fondo de su preferencia o añadir más capas."
               ,br()
               ,br()
               ,tags$strong("Aprenda más sobre una zona")
               ,br()
               ,"Haga clic en una zona del mapa para obtener más información sobre la misma. La ventana emergente muestra el valor del indicador que seleccionó en la barra de herramientas, encima del mapa. La gráfica y la tabla que aparecen debajo del mapa muestran más información acerca de la zona que acaba de seleccionar."
               ,br()
               ,br()
               ,tags$strong("Busque una dirección")
               ,br()
               ,"Use el icono de la lupa y la barra de búsqueda “Buscar con geocodificador de OSM” para buscar una dirección, ciudad, código postal o nombre de un lugar."
               ,br()
               ,br()
               ,tags$strong("Personalice y añada contexto (capas adicionales)")
               ,br()
               ,"Use el icono a la izquierda del mapa para seleccionar el mapa base o agregar capas y personalizar la vista del mapa. "
               ,br()
               ,"Las opciones del mapa base permiten elegir distintos mapas de fondo (p.ej., claro, oscuro o con calles y lugares de interés). Las opciones del mapa base no influyen en los percentiles ni en las medidas que se presentan en la herramienta."
               ,br()
               ,"Las capas adicionales proporcionan información sobre las zonas que producen petróleo y gas, tienen centrales a carbón, son comunidades rurales, han sido designadas por el gobierno federal como comunidades de Justice40 o cumplen con la definición de comunidad afectada de manera desproporcionada del Departamento de Salud Pública y Medio Ambiente del CDPHE. Las capas adicionales solo ofrecen más contexto. Las capas adicionales no forman parte de los métodos de EnviroScreen y no influyen en los percentiles o medidas que se presentan en la herramienta."
             ),
             tags$h4("Paso 3: Explore los datos de manera diferente."),
             p(
               tags$b("Use las gráficas de barras y la tabla de datos para obtener más información sobre el área seleccionada.")
               ,br()
               ,br()
               ,"Las gráficas de barras indican si un área se ve más o menos afectada que otras partes del estado para cada categoría del puntaje. Lea la información de la pestaña “Cómo se usan las gráficas de barras” a la derecha para aprender más."
               ,br()
               ,"La tabla de datos presenta los mismos datos que el mapa y las gráficas de barras en un formato tabular y descargable.  La tabla presenta los datos en la misma escala geográfica que el mapa. Lea la información de la pestaña “Cómo se usa la tabla de datos” a la derecha para aprender más."
             )
    ),
    tabPanel(title = "Cómo se usan las gráficas de barras",
             p(
               "La gráfica de barras que aparece a la derecha del mapa muestra el puntaje total de EnviroScreen. Las gráficas de barras que están debajo del mapa muestran el puntaje de los cinco componentes que conforman el puntaje total. Estas gráficas indican si un área se ve más o menos afectada que otras partes del estado para cada categoría."
               ,br()
               ,tags$strong("La altura de las barras")
               ," (eje y) representa la cantidad de zonas de Colorado cuya carga está dentro del mismo rango que el área seleccionada."
               ,br()
               ,br()
               , "The"
               ,tags$strong(" La posición horizontal")
               ,"de las barras (eje x) representa la categoría a la que pertenece la carga (de acuerdo con el rango percentil). Cuanto más hacia la izquierda está la barra, menor es la carga de esa zona en comparación con el resto de Colorado. Cuanto más hacia la derecha está la barra, mayor es la carga de esa zona en comparación con el resto de Colorado."
               ,br()
               ,br()
               ,"El área que está seleccionada en el mapa aparece en anaranjado en las gráficas. Las"
               ,tags$strong(" barras anaranjadas ")
               ,"representan el lugar que ocupa el área seleccionada en la distribución del puntaje total de EnviroScreen y los componentes individuales."
             )
             #insert image
             ,tags$img(
               id = "histoDesc",
               src="histoDesc.png",
               title = "Bar Charts Elements",
               height="auto"
             )
             # I think this was included in the text above 
             # ,br()
             # ,tags$strong("Data Table")
             # ,br()
             # ,"The area selected in the map also filters the data table below the charts. Explore the data by sorting the table. Select a row or rows to highlight the selection in the map. For example, a user could sort the table to find the areas with the highest climate vulnerability score, select rows in the table, and click “Highlight Selection on Map.” The areas selected in the table will be highlighted on the map."
    ),
    tabPanel(title = "Cómo se usa la tabla de datos",
             br()
             ,p(
               "La tabla de datos presenta los mismos datos que el mapa y las gráficas de barras en un formato tabular. Las "
               ,tags$strong(" columnas de la tabla")
               ," describen el tipo de información que se muestra (p.ej., condado, componente, nombre del indicador). Las "
               ,tags$strong(" filas de la tabla")
               ,"representan las divisiones geográficas individuales. La tabla presenta los datos en la misma escala geográfica que el mapa. Si el mapa muestra los condados, la tabla de datos mostrará los datos a nivel de condados; si el mapa muestra un área censal o grupo de manzanas censales, la tabla mostrará los datos para la división geográfica que corresponda."
               ,br()
               ,br()
               ,"La tabla muestra diez filas por omisión. Para"
               , tags$strong(" ver más filas")
               , "en pantalla, haga clic en la casilla “Mostrar xx filas” que se encuentra en la parte superior izquierda de la tabla (máximo de 100 filas). Si desea ver los datos de todas las divisiones geográficas, use la barra que aparece en la parte inferior derecha para trasladarse entre todas las páginas disponibles de la tabla."
               ,br()
               ,br()
               ,"También hay un"
               ,tags$strong(" cuadro de búsqueda")
               ,"  en la esquina superior derecha de la tabla. Escriba el nombre del condado, área censal o número del grupo de manzanas censales para identificar fácilmente la columna con los datos correspondientes."
               ,br()
               ,br()
               ,"La tabla de datos incluye asimismo "
               ,tags$strong(" distintas pestañas")
               ,"que están organizadas de acuerdo con los componentes del puntaje."
               ,br()
               ,br()
               ,"También se puede hacer clic en los encabezados de las columnas para clasificar los datos de acuerdo con su valor, de mayor a menor o de menor a mayor."
               ,br()
               ,br()
               ,"El área que esté seleccionada en el mapa aparecerá resaltada en la tabla de datos, debajo de las gráficas. Escoja una o más filas de la tabla para resaltar esa área en el mapa. Por ejemplo, ordene los datos de la tabla para buscar las áreas que tienen el puntaje de vulnerabilidad climática más alto, seleccione las filas de la tabla y luego haga clic en “Resaltar selección en el mapa”. Se resaltarán en el mapa las zonas que correspondan a las filas que se hayan seleccionado en la tabla."
               ,br()
               ,br()
               ,"También se pueden descargar los datos de la tabla haciendo clic en el cuadro"
               ,tags$strong("'Descargar datos usando división geográfica actual'.")
             )
             #insert image
             ,tags$img(
               id = "dataTable",
               src="dataTable.png",
               title = "Data Table Elements",
               height="auto"
             )
    ),
    tabPanel(title = "Explicación de los datos",
             br()
             ,tags$strong("Puntaje de EnviroScreen")
             ,p(
               "Colorado EnviroScreen mapea la superposición de las exposiciones y los efectos ambientales, la vulnerabilidad climática, las poblaciones sensibles y las características demográficas con el fin de comprender las injusticias ambientales y los riesgos para la salud ambiental que existen en Colorado. "
               ,br()
               ,br()
               ,"Para calcular el puntaje de EnviroScreen, se usa el rango relativo (percentil) de los indicadores de los datos individuales de un área. El puntaje de EnviroScreen combina los indicadores individuales y crea subcomponentes de acuerdo con un tema, como la vulnerabilidad climática o las características demográficas. A continuación, el puntaje de EnviroScreen combina los subcomponentes una vez más para calcular los puntajes de contaminación y carga climática y de factores de salud y sociales. Se multiplican estos dos puntajes para obtener el puntaje de EnviroScreen. El puntaje de EnviroScreen es el dato por omisión que aparece en el mapa. Se pueden mostrar los puntajes de los indicadores individuales o de los subcomponentes en el mapa."
               ,br()
               ,br()
               ,"Cuanto más alto el puntaje de EnviroScreen, más probable es que el área se vea afectada por injusticias de salud ambiental."
             )
             #insert image
             ,tags$img(
               id = "scoreDesc",
               src="scoreDesc.png",
               title = "Score Calculation",
               height="auto")
             ,tags$strong("Puntajes de los componentes")
             ,p(
               "Para ver los componentes que conforman el puntaje de EnviroScreen, puede seleccionar el puntaje de cualquiera de los componentes agrupados o de los componentes individuales. Cada uno de estos puntajes está compuesto por múltiples indicadores individuales, que también se pueden ver por separado. Al igual que el puntaje total de EnviroScreen, los puntajes de los componentes reflejan rangos relativos (percentiles)."
             )
             ,tags$strong("Indicadores individuales")
             ,p(
               "También se pueden ver los indicadores individuales de datos en el mapa. Estos indicadores individuales se organizan de acuerdo con el componente que conforman, en el menú desplegable “Indicador” que aparece más abajo.  Por ejemplo, las medidas de la calidad del aire aparecen bajo Exposiciones ambientales."
               ,br()
               ,br()
               ,"Cada indicador individual de datos representa algo distinto y se expresa en una unidad de medida diferente. Por ejemplo, algunos indicadores (como la calidad del aire) representan concentraciones aproximadas, mientras que otros (como las enfermedades cardíacas) son tasas o porcentajes de ciertos resultados sanitarios.  Consulte el documento técnico para obtener más información."
             )
             ,tags$strong("Fuentes de datos")
             ,p(
               "Los indicadores que se incluyen en Colorado EnviroScreen provienen de una variedad de fuentes de datos. Muchas de estas fuentes están a disposición del público a través de agencias estatales y federales, como la Agencia de Protección Ambiental de Estados Unidos (EPA), los Centros para el Control y la Prevención de Enfermedades (CDC), el Departamento de Salud Pública y Medio Ambiente de Colorado (CDPHE) y la Comisión de Conservación de Gas y Petróleo de Colorado (COGCC)."
               ,br()
               ,br()
               ,"Pese a que EnviroScreen contiene muchos indicadores, algunos de estos indicadores datan de hace algunos años o representan medidas para una escala geográfica mayor. Tampoco fue posible incluir ciertas exposiciones ambientales, impactos climáticos, resultados sanitarios y factores demográficos en EnviroScreen porque no existen fuentes de datos confiables."
             )
             ,tags$strong("Limitaciones de los datos")
             ,p(
               "Los datos que se incluyen en EnviroScreen tienen varias limitaciones, debido al uso de datos secundarios. Se conocen como datos secundarios los datos que han sido recabados por una persona u organización que no es el usuario principal. Además, los indicadores que se utilizaron a menudo corresponden a distintos años, lo cual hace que sea difícil comparar los conjuntos de datos. Más aún, no todos los datos estaban disponibles originalmente a la misma escala geográfica. Algunos de los datos se obtuvieron a nivel de condado (2 indicadores), otros a nivel de área censal (9 indicadores) y otros a nivel de grupos de manzanas censales o a una escala geográfica menor (20 indicadores). Por último, pese a que la herramienta se propone ofrecer una descripción completa de las injusticias ambientales, no siempre fue posible obtener los indicadores en formato de sistema de información geográfica. Por este motivo, Colorado EnviroScreen ofrece una representación limitada de las injusticias ambientales, debido a la disponibilidad de los datos. Consulte la guía técnica del usuario para obtener más información."
             )
    ),
    tabPanel(title = "Ejemplo",
             p(
               "A continuación se da un ejemplo de cómo las partes interesadas podrían usar Colorado EnviroScreen para alcanzar sus metas."
               ,br()
               ,br()
               ,"Un grupo sin fines de lucro solicita una subvención para instalar más herramientas de monitoreo de la contaminación del aire en su comunidad. De acuerdo con las normas de la subvención, los solicitantes deben probar que la comunidad necesita los fondos. El grupo sabe que su solicitud tendrá mayor solidez si se basa en datos científicos. El grupo usa Colorado EnviroScreen para obtener más información sobre su comunidad."
               ,br()
               ,br()
               ,"He aquí como el grupo comunitario podría usar Colorado EnviroScreen:"
               ,tags$ol(
                 tags$li("El personal del grupo sin fines de lucro se dirige a la página web de Colorado EnviroScreen.")
                 ,tags$li("Usan la función de búsqueda del mapa para ubicar el área censal en que se encuentra su comunidad.")
                 ,tags$li("Hacen clic en esa área del mapa para ver los resultados y otra información en las gráficas y la tabla.")
                 ,tags$li(" Cotejan los puntajes totales de EnviroScreen, de los componentes e indicadores de su área censal con el resto del estado.")
                 ,tags$li("Descargan los datos de su comunidad al pie de la página.")
                 ,tags$li("Usan esta información para redactar una convincente solicitud de subvención para adquirir monitores de aire para la comunidad.")
               )
             )
    ),
    tabPanel(title =  "Definiciones",
             br()
             ,tags$strong("Puntaje de EnviroScreen")
             ,p(
               "El puntaje de EnviroScreen combina las características de la población con las cargas ambientales. Este puntaje varía de 0 a 100. "
               ,tags$strong(" Cuanto más alto es el puntaje, mayor es la carga.")
               ,br()
               ,"El puntaje de EnviroScreen se expresa como percentil, que es un rango o categoría. El número representa la cantidad de condados, áreas censales o grupos de manzanas censales del estado que reciben un puntaje más bajo que la zona en cuestión."
               ,br()
               ,tags$strong("Haga de cuenta que el puntaje de EnviroScreen de un condado es 70.")
               ," Esto significa que el puntaje de EnviroScreen de este condado es más alto que el puntaje del 70 % de todos los condados de Colorado. En otras palabras, es menos probable que el 70 % de los condados de Colorado se vean afectados por injusticias de salud ambiental que el condado en cuestión."
               ,br()
               ,tags$strong("Haga de cuenta que el puntaje de EnviroScreen de un área censal es 20.")
               ,"  Esto significa que el puntaje de EnviroScreen de esta área es más alto que el puntaje del 20 % de todas las áreas censales de Colorado. En otras palabras, es menos probable que el 20 % de los condados de Colorado se vean afectados por injusticias de salud ambiental que el área censal en cuestión, o es más probable que el 80 % de las áreas censales de Colorado se vean afectadas por injusticias de salud ambiental que el área censal en cuestión. "
             )
             ,tags$strong("Puntaje de contaminación y carga climática")
             ,p(
               "El puntaje de contaminación y carga climática combina los puntajes de los siguientes componentes: exposiciones ambientales, efectos ambientales y carga climática. Este puntaje varía de 0 a 100, siendo que el puntaje más alto representa a las poblaciones con una mayor carga ambiental."
             )
             ,tags$strong("Puntaje de factores de salud y sociales")
             ,p(
               "El puntaje de factores de salud y sociales combina el puntaje de poblaciones sensibles y características demográficas. Este puntaje varía de 0 a 100, siendo que el puntaje más alto representa a las poblaciones más susceptibles y vulnerables."
             )
             ,tags$strong("Puntaje de exposiciones ambientales")
             ,p("El puntaje de exposiciones ambientales representa la exposición de una comunidad a ciertos riesgos ambientales en relación con el resto del estado. Este puntaje varía de 0 a 100, siendo que los puntajes más altos son los peores. El puntaje de exposiciones ambientales no incluye todos los contaminantes; es el promedio de los datos sobre el material particulado de diésel, la proximidad al tráfico, el ozono, el material particulado 2.5, los contaminantes tóxicos del aire, otros contaminantes del aire, el riesgo de exposición al plomo, las infracciones en relación con el agua potable y el ruido."
             )
             ,tags$strong("Puntaje de efectos ambientales")
             ,p("El puntaje de efectos ambientales representa la cantidad de sitios peligrosos o tóxicos que hay en una comunidad en relación con el resto del estado. Este puntaje varía de 0 a 100, siendo que los puntajes más altos son los peores. El puntaje es el promedio de los datos sobre la proximidad a las minas, operaciones de petróleo y gas, aguas superficiales deterioradas, instalaciones de descarga de aguas residuales, sitios Superfondos, instalaciones que usan sustancias químicas peligrosas e instalaciones que generan, tratan, almacenan o eliminan residuos peligrosos. Puesto que la mayoría de las personas no están directamente expuestas a estos sitios, para este puntaje se usa un peso o ponderación de la mitad en relación con las exposiciones ambientales al calcular el puntaje total de contaminación y carga climática."                      )
             ,tags$strong("Puntaje de carga climática")
             ,p(
               "El puntaje de carga climática representa el riesgo de sequías, inundaciones, calor extremo e incendios forestales de una comunidad, en comparación con el resto del estado. Este puntaje varía de 0 a 100; cuanto más alto es el puntaje, mayor es la carga."
             )
             ,tags$strong("Puntaje de poblaciones sensibles")
             ,p(
               "El puntaje de poblaciones sensibles expresa el grado de riesgo a las exposiciones ambientales y los impactos climáticos que corre una comunidad en relación con la salud. Por ejemplo, la contaminación del aire tiene un mayor efecto en las personas de más edad y más jóvenes, así como en las personas con afecciones de salud crónicas, como el asma. Este puntaje varía de 0 a 100, siendo que los puntajes más altos son los peores. Para calcular este puntaje, se usan los datos sobre la tasa de hospitalizaciones por asma, prevalencia de cáncer, prevalencia de diabetes, prevalencia de enfermedades cardíacas, expectativa de vida, tasa de bajo peso al nacer, salud mental, población por encima de 65 años y población por debajo de 5 años."
             )
             ,tags$strong("Puntaje de características demográficas")
             ,p(
               "El puntaje de características demográficas representa las vulnerabilidades sociales y económicas de una comunidad. Este puntaje varía de 0 a 100, siendo que los números más altos representan un mayor grado de vulnerabilidad. Para calcular este puntaje, se usan los datos sobre las personas que viven con discapacidades, la carga por gastos de vivienda, el nivel de educación alcanzado, dominio limitado del inglés, ingreso, raza y grupo u origen étnico."
             )
             ,tags$strong("Comunidad afectada de manera desproporcionada")
             ,p(
               "Este término se refiere a las áreas que cumplen con la definición de comunidad afectada de manera desproporcionada de la Ley de Justicia Ambiental de Colorado (Ley 21-1266 de la Cámara de Representantes). La definición incluye los grupos de manzanas censales con más de un 40 % de la población de bajos ingresos, hogares con sobrecarga por gastos de vivienda o personas de color. “De bajos ingresos” significa que el ingreso promedio del grupo familiar equivale o está por debajo del 200 % del nivel federal de pobreza. “Hogar con sobrecarga por gastos de vivienda” significa que el grupo familiar destina más del 30 % de los ingresos al pago de la vivienda. “Personas de color” incluye a todas las personas que no se identifican como blancos no hispanos. Esta definición no forma parte de los componentes ni del puntaje de EnviroScreen y no influye en los resultados que se presentan en el mapa, las gráficas o la tabla."
             )
             ,tags$strong("Comunidad con carbón")
             ,p(
               "Se indican como comunidades con carbón todos las áreas censales y grupos de manzanas censales de los condados que cuentan con una central eléctrica a carbón. Estos datos no forman parte de los componentes ni del puntaje de EnviroScreen y no influyen en los resultados que se presentan en el mapa, las gráficas o la tabla."
             )
             ,tags$strong("Comunidad con petróleo y gas")
             ,p(
               "Se indican como comunidades con petróleo y gas todos las áreas censales y grupos de manzanas censales de los condados que cuentan con operaciones de extracción de petróleo y gas activas. En EnviroScreen también se incluye la proximidad a las operaciones de petróleo y gas como parte del componente de efectos ambientales."
             )
             ,tags$strong("Zona urbana/rural")
             ,p(
               "De acuerdo con la Oficina del Censo de Estados Unidos, las zonas urbanas son zonas con alta densidad de población que incluyen propiedades residenciales, comerciales y de otro tipo. Se consideran como urbanos los condados que incluyen estas zonas urbanas. Se consideran como rurales todos los condados que no forman parte de los centros urbanos. Estos datos no forman parte de los componentes ni del puntaje de EnviroScreen y no influyen en los resultados que se presentan en el mapa, las gráficas o la tabla. "
             )
             ,tags$strong("Justice40")
             ,p(
               "La Casa Blanca lanzó la Iniciativa Justice40 a principios de 2022. La meta de la Iniciativa Justice40 es que se destine el 40 por ciento del total de los beneficios de las inversiones del gobierno federal en siete áreas clave a las comunidades desfavorecidas. Estas siete áreas clave son las siguientes: cambio climático, energía limpia y eficiencia energética, tráfico limpio, vivienda asequible y sostenible, formación y desarrollo de la fuerza laboral, descontaminación y disminución de la contaminación histórica y desarrollo de una infraestructura de importancia clave para evitar la contaminación del agua. De acuerdo con la definición de la Iniciativa Justice40, se considera que una comunidad es “desfavorecida” si uno o más indicadores ambientales o climáticos del área censal se encuentran por encima del umbral y los indicadores socioeconómicos del área censal están por encima del umbral. Esta definición no forma parte de los componentes ni del puntaje de EnviroScreen y no influye en los resultados que se presentan en el mapa, las gráficas o la tabla."
             )
             ,tags$strong("Story Maps")
             ,p(
               "A StoryMap is an immersive story that combines text, interactive maps, and other multimedia content. In Colorado EnviroScreen, the StoryMaps highlight life experiences that are complementary to the data included in the tool but importantly, they do not contribute to the EnviroScreen score."
             )
    ),
    tabPanel("Creación de la herramienta",
             br()
             ,tags$strong("Producto de una relación de colaboración")
             ,p(
               "Colorado EnviroScreen surgió de la colaboración entre el Departamento de Salud Pública y Medio Ambiente de Colorado (CDPHE) y Colorado State University (CSU) como resultado de un proceso de licitación competitiva. "
               ,br()
               ,br()
               ,"Con los aportes de la comunidad, las partes interesadas y el CDPHE, el equipo de CSU trabajó incansablemente en la creación y desarrollo de la herramienta EnviroScreen. De aquí en adelante, el CDPHE se encargará de mantener la herramienta, que pasará a ser de su propiedad."
               ,br()
               ,br()
               ,"Se deben enviar las preguntas o comentarios sobre Colorado EnviroScreen a cdphe_ej@state.co.us."
             )
             ,br()
             ,tags$strong("Participación comunitaria")
             ,p(
               "La participación y comentarios de la comunidad de Colorado constituyeron un factor esencial para la creación y desarrollo de Colorado EnviroScreen."
               ,br()
               ,br()
               ,"El equipo de CSU y el CDPHE se empeñaron en obtener la participación del público desde el comienzo del proceso de creación de Colorado EnviroScreen. En las primeras etapas de definición de la herramienta, el equipo realizó entrevistas personales, convocó grupos focales en inglés y español y organizó una gran reunión abierta al público en ambos idiomas. Gracias a estas actividades de extensión, el equipo reunió información acerca de cómo los diversos habitantes de Colorado podrían usar la herramienta y qué debería incluir."
               ,br()
               ,br()
               ,"Tras que creara una versión básica de Colorado EnviroScreen, el equipo lanzó una prueba beta cerrada como parte de la cual más de 100 usuarios verificaron la herramienta. En esta etapa, el equipo reunió información mediante un cuestionario bilingüe en línea y llevó a cabo entrevistas con representantes de importantes grupos de usuarios de Colorado EnviroScreen. "
             )
    ),
    tabPanel("Recursos adicionales"
             ,h4("Materiales de apoyo para Colorado EnviroScreen")
             ,p(
               "Guía básica del usuario (inglés |español)"
               ,br()
               ,br()
               ,"Guía técnica del usuario (solo disponible en inglés, por el momento)."
               ,br()
               ,br()
               ,"Resumen ejecutivo de participación comunitaria"
             )
             ,h4("Programas del CDPHE")
             ,p(
               "Programa de Justicia Ambiental "
               ,tags$a(
                 href = "https://cdphe.colorado.gov/environmental-justice"
                 ,tags$em("https://cdphe.colorado.gov/environmental-justice")
                 , target = "_blank"
               )
             )
             ,p(
               "Oficina de Toxicología y Epidemiología Ambiental "
               ,tags$a(
                 href = "https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology "
                 ,tags$em("https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology ")
                 , target = "_blank"
               )
             )
             ,p(
               "División de Control de la Contaminación del Aire "
               ,tags$a(
                 href = "https://cdphe.colorado.gov/environment/air-pollution-control"
                 ,tags$em("https://cdphe.colorado.gov/environment/air-pollution-control")
                 , target = "_blank"
               )
             )
             ,p(
               "División de Control de la Calidad del Agua"
               ,tags$a(
                 href = "https://cdphe.colorado.gov/water-quality "
                 ,tags$em("https://cdphe.colorado.gov/water-quality")
                 , target = "_blank"
               )
             )
             ,p(
               "División de Materiales Peligrosos y Manejo de Residuos"
               ,tags$a(
                 href = "https://cdphe.colorado.gov/hm"
                 ,tags$em("https://cdphe.colorado.gov/hm")
                 , target = "_blank"
               )
             )
             ,p(
               "Oficina de Equidad de la Salud"
               ,tags$a(
                 href = "https://cdphe.colorado.gov/ohe"
                 ,tags$em("https://cdphe.colorado.gov/ohe")
                 , target = "_blank"
               )
             )
             ,p(
               "Prevención y Bienestar"
               ,tags$a(
                 href = "https://cdphe.colorado.gov/health/prevention-and-wellness"
                 ,tags$em("https://cdphe.colorado.gov/health/prevention-and-wellness")
                 , target = "_blank"
               )
             )
             ,p(
               "Información del CDPHE sobre Commerce City y North Denver"
               ,tags$a(
                 href = "https://cdphe.colorado.gov/cc-nd"
                 ,tags$em("https://cdphe.colorado.gov/cc-nd")
                 , target = "_blank"
               )
             )
             ,h4("Otras agencias estatales")
             ,p(
               "Departamento de Recursos Naturales de Colorado"
               ,tags$a(
                 href = "https://dnr.colorado.gov/"
                 ,tags$em("https://dnr.colorado.gov/")
                 , target = "_blank"
               )
             )
             ,p(
               "Departamento de Transporte de Colorado "
               ,tags$a(
                 href = "https://www.codot.gov/"
                 ,tags$em("https://www.codot.gov/")
                 , target = "_blank"
               )
             )
             ,p(
               "Departamento de Servicios Humanos de Colorado"
               ,tags$a(
                 href = "https://cdhs.colorado.gov/"
                 ,tags$em("https://cdhs.colorado.gov/")
                 , target = "_blank"
               )
             )
             ,p(
               "Comisión de Conservación de Gas y Petróleo de Colorado"
               ,tags$a(
                 href = "https://cogcc.state.co.us/#/home"
                 ,tags$em("https://cogcc.state.co.us/#/home")
                 , target = "_blank"
               )
             )
             ,p(
               "Comisión de Servicios Públicos de Colorado"
               ,tags$a(
                 href = "https://puc.colorado.gov/"
                 ,tags$em("https://puc.colorado.gov/")
                 , target = "_blank"
               )
             )
             ,p(
               "Departamento de Transporte de Colorado"
               ,tags$a(
                 href = "https://www.codot.gov/"
                 ,tags$em("https://www.codot.gov/")
                 , target = "_blank"
               )
             )
             ,p(
               "Portal de mapeo de los peligros y riesgos de Colorado"
               ,tags$a(
                 href = "https://coloradohazardmapping.com/"
                 ,tags$em("https://coloradohazardmapping.com/")
                 , target = "_blank"
               )
             )
             ,h4("Environmental justice at federal programs and agencies")
             ,p(
               "Agencia de Protección Ambiental de Estados Unidos (EPA)"
               ,tags$a(
                 href = "https://www.epa.gov/environmentaljustice"
                 ,tags$em("https://www.epa.gov/environmentaljustice")
                 , target = "_blank"
               )
             )
             ,p(
               "Departamento de Vivienda y Desarrollo Urbano de EE. UU."
               ,tags$a(
                 href = "https://www.hud.gov/climate/environmental_justice"
                 ,tags$em("https://www.hud.gov/climate/environmental_justice")
                 , target = "_blank"
               )
             )
             ,p(
               "Iniciativa Justice40 "
               ,tags$a(
                 href = "https://www.whitehouse.gov/omb/briefing-room/2021/07/20/the-path-to-achieving-justice40/"
                 ,tags$em("https://www.whitehouse.gov/omb/briefing-room/2021/07/20/the-path-to-achieving-justice40/")
                 , target = "_blank"
               )
             )
             ,p(
               "Herramienta de evaluación del clima y la justicia económica del Consejo de Calidad Ambiental - versión beta "
               ,tags$a(
                 href = "https://screeningtool.geoplatform.gov/en/#3/33.47/-97.5"
                 ,tags$em("https://screeningtool.geoplatform.gov/en/#3/33.47/-97.5")
                 , target = "_blank"
               )
             )
             ,p(
               "Centros para el Control y Prevención de las Enfermedades "
               ,tags$a(
                 href = "https://www.cdc.gov/nceh/tracking/topics/EnvironmentalJustice.htm"
                 ,tags$em("https://www.cdc.gov/nceh/tracking/topics/EnvironmentalJustice.htm")
                 , target = "_blank"
               )
             )
             ,h4("Real-time air monitoring")
             ,p(
               "Programa Love My Air de Denver "
               ,tags$a(
                 href = "https://www.denvergov.org/Government/Agencies-Departments-Offices/Agencies-Departments-Offices-Directory/Public-Health-Environment/Environmental-Quality/Air-Quality/Love-My-Air"
                 ,tags$em("https://www.denvergov.org/Government/Agencies-Departments-Offices/Agencies-Departments-Offices-Directory/Public-Health-Environment/Environmental-Quality/Air-Quality/Love-My-Air")
                 , target = "_blank"
               )
             )
             ,p(
               "Programa Love My Air del departamento de salud de Tri-County"
               ,tags$a(
                 href = "https://www.tchd.org/868/Love-My-Air"
                 ,tags$em("https://www.tchd.org/868/Love-My-Air")
                 , target = "_blank"
               )
             )
             ,p(
               "Programa de monitoreo del aire de Cultivando en Commerce City "
               ,tags$a(
                 href = "https://www.bouldair.com/commerce_city.htm"
                 ,tags$em("https://www.bouldair.com/commerce_city.htm")
                 , target = "_blank"
               )
             )
             ,p(
               "AirNow.gov "
               ,tags$a(
                 href = "https://www.airnow.gov/"
                 ,tags$em("https://www.airnow.gov/")
                 , target = "_blank"
               )
             )
             ,p(
               "Monitoreo del aire impulsado por la comunidad de la refinería Suncor"
               ,tags$a(
                 href = "https://www.ccnd-air.com/"
                 ,tags$em("https://www.ccnd-air.com/")
                 , target = "_blank"
               )
             )
             ,p(
               "Monitoreo del material particulado (polvo) del Departamento de Transporte de Colorado para el proyecto Central 70 en North Denver"
               ,tags$a(
                 href = "https://www.codot.gov/projects/i70east/resources/air-quality"
                 ,tags$em("https://www.codot.gov/projects/i70east/resources/air-quality")
                 , target = "_blank"
               )
             )
    ),
    tabPanel("Guías para los usuarios",
             br(),
             p(
               tags$a(
                 href = "https://docs.google.com/document/d/1_GEjGbOd3CmXwZu09QJ9oO4ZI8hqXtFwZAAeTsNV5lQ/edit?usp=sharing"
                 ,tags$em("Guías para los usuarios")
                 , target = "_blank"
               )
             )
    )
  ),
  
  
  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  br(),
  fluidRow(class = "sectionTitle",id = "map",
           # action button : update map elements
           column(2,
                  tags$div(title = "Click here to update map display",
                           actionButton(inputId = "updateMap", "Actualizar mapa"),
                  ),
           ),
           # select geography
           column(2,
                  tags$div(title="Click here to select area to display on the map",
                           selectInput(
                             inputId = "Geom",
                             label = "Escala geográfica",
                             choices = c("Condado", "Sector censal", "Grupo de bloques censales"),
                             selected = "Condado",
                             width = "90%"
                           )
                  )
           ),
           # select indicator
           column(4,
                  tags$div(title="Click here to select variable for map",
                           selectInput(
                             inputId = "Indicator",
                             label = "Indicador",
                             choices = list(
                               "Puntaje de Colorado EnviroScreen" = "Puntaje de Colorado EnviroScreen",
                               "Puntaje de los componentes en conjunto" = c("Contaminación y carga climática",
                                                                            "Factores de salud y sociales"),
                               "Puntaje de los componentes" =c("Exposiciones ambientales",
                                                               "Efectos ambientales",
                                                               "Vulnerabilidad climática",
                                                               "Poblaciones sensibles",
                                                               "Características demográficas"),
                               "Exposiciones ambientales" = c("Emisiones de contaminantes tóxicos del aire",
                                                              "Material particulado (PM) de diésel",
                                                              "Reglamentos sobre agua potable",
                                                              "Riesgo de exposición al plomo",
                                                              "Ruido",
                                                              "Otros contaminantes del aire",
                                                              "Ozono",
                                                              "Contaminación por partículas finas",
                                                              "Proximidad y volumen de tránsito"
                               ),
                               "Efectos ambientales" = c("Arroyos y ríos deteriorados",
                                                         "Proximidad a instalaciones de residuos peligrosos",
                                                         "Proximidad a ubicaciones de minería",
                                                         "Proximidad a los sitios de la Lista Nacional de Prioridades",
                                                         "Proximidad a petróleo y gas",
                                                         "Proximidad a los sitios del Plan de Gestión de Riesgos",
                                                         "Indicador de descargas de aguas residuales"
                               ),
                               "Vulnerabilidad climática" = c("Sequía",
                                                              "Días de calor extremo",
                                                              "Llanuras aluviales",
                                                              "Riesgo de incendios forestales"
                               ),
                               "Poblaciones sensibles" = c("Tasa de hospitalización por asma",
                                                           "Predominio de cáncer",
                                                           "Predominio de diabetes",
                                                           "Cardiopatías en adultos",
                                                           "Expectativa de vida",
                                                           "Bajo peso al nacer",
                                                           "Indicador de salud mental",
                                                           "Población por encima de 64 años",
                                                           "Población por debajo de 5 años"
                               ),
                               "Características demográficas" = c("Sobrecarga por gastos de vivienda",
                                                                  "Porcentaje de discapacidades",
                                                                  "Porcentaje que no completaron los estudios de secundaria",
                                                                  "Porcentaje de aislamiento lingüístico",
                                                                  "Porcentaje de bajos ingresos",
                                                                  "Porcentaje de personas de color"
                               )
                             ),
                             selected = "Puntaje de Colorado EnviroScreen",
                             width = "90%"
                           )
                  )
           ),
           # toggle between measured and percentile
           column(2,
                  tags$div(title="Click here to show measured value or rank of the variable",
                           selectInput(
                             inputId = "Percentile",
                             label = "Medida o %",
                             choices = c("Valor medido", "Rango percentil"),
                             selected = "Rango percentil"
                           )
                  )
           ),
           column(
             2,
             tags$div(title="Click here to remove highlighted features",
                      actionButton("removeHighlight", "Eliminar resaltado")
             )
           ),
           tags$blockquote(textOutput("indicatorDesc"))
           
  ),
  
  
  # display map -------------------------------------------------------------
  fluidRow(tags$style(type = "text/css", "#mymap {height: calc(100vh - 250px) !important;}"), #style = {"background-color:#4d3a7d;"},
           column(1),
           column(7, leafletOutput("mymap")),
           column(3, br(),br(),br(),br(),
                  plotlyOutput("histEnviroScreen" ,height = "80%", width = "100%")),
           column(1),
           
  ),
  
  # show plots --------------------------------------------------------------
  # plot of the datasets
  br(),
  fluidRow(class = "plotArea",
           column(1),
           column(2, br(), plotlyOutput("histExposure", height=300)),
           column(2, br(), plotlyOutput("histEffect", height=300)),
           column(2, br(), plotlyOutput("histClimate", height=300)),
           column(2, br(), plotlyOutput("histSocial", height=300)),
           column(2, br(), plotlyOutput("histDemo", height=300)),
           column(1),
           p("El puntaje de EnviroScreen combina cinco componentes: exposiciones ambientales, efectos ambientales, vulnerabilidad climática, población sensible y composición demográfica. Cuando se hace clic en una ubicación del mapa, las barras anaranjadas de la gráfica indican el puntaje de esa ubicación. Las barras anaranjadas muestran el puntaje de cada componente de esa ubicación en comparación con el resto de Colorado. En conjunto, las gráficas muestran cómo se calcula el puntaje de EnviroScreen para la ubicación seleccionada.")
  ),
  br(),
  
  # show reactive table -----------------------------------------------------
  # table showing the results
  fluidRow(class = "sectionTitle",
           h2("EnviroScreen Score Data"),
           p("Use las pestañas que están encima de la tabla para filtrar los elementos del puntaje de Colorado EnviroScreen.  Seleccione una fila de la tabla y luego presione el botón anaranjado `Resaltar selección en el mapa` al pie de la tabla para ver la ubicación en el mapa."),
    
  ),
  fluidRow(class = "dataTableArea",
           radioGroupButtons(inputId = "tableSelect", label = "",
                             choices = c("Puntaje de los componentes en conjunto", "Puntaje de los componentes",
                                         "Exposiciones ambientales", "Efectos ambientales",
                                         "Vulnerabilidad climática", "Poblaciones sensibles",
                                         "Características demográficas",
                                         "Clasificación de las comunidades",
                                         "Descripción de los indicadores"),
                             #justified = TRUE
           ),
           # data table output ----
           # changed to just single table
           DT::dataTableOutput("tableAll")
  ),
  
  # download table option  --------------------------------------------------
  fluidRow(
    column(3,
           offset = 1,
           tags$div(title="Click here to add selections to map display",
                    actionButton("button_table", "Resaltar selección en el mapa ")
           ),
    ),
    column(3,
           tags$div(title="Click here to download content",
                    downloadButton("downloadData", "Descargar datos para división geográfica actual")
           ),
    ),
    column(3,
           tags$div(title="Click here to download Indicator Descriptions",
                    downloadButton("downloadData2", "Descargar descripciones de indicadores")
           ),
    )
  ),
  br(),
  
  fluidRow( class = "titleElement",
            column(4,
                   h3("Additional Resources"),
                   p(class = "href2",
                     "Guía básica del usuario (",
    tags$a(href = "https://drive.google.com/file/d/1iytdPG5iK2VBNpIy8k6oT6lU6-QKMLOa/view?usp=sharing",
           tags$span(style="color:white","inglés"), target = "_blank"),
    "and ",
    tags$a(href = "https://drive.google.com/file/d/17rQ90fNt3DF-0PbySpGjo2tiy9AmDiCc/view?usp=sharing",
           tags$span(style="color:white","español"), target = "_blank"),
    ")."
                   ),
    p(class = "href2",
      "El código y repositorios de los datos están disponibles aquí ",
      tags$a(href= "https://geospatialcentroid.github.io/Colorado_EnviroScreen/",
             tags$span(style="color:white","aquí"), target = "_blank")
    )
            ),
    
    column(4,tags$a(
      href = "https://cdphe.colorado.gov/enviroscreen",
      tags$img(
        src="EnviroScreen Logos/co_cdphe_pr_es_white_v.png",
        title = "Colorado Department of Public Health and Environment",
        width="100%",
        height="auto"
      )
    )
    ),
    column(4,tags$a(
      href = "https://www.colostate.edu/",
      tags$img(
        src="csu.png",
        title = "Colorado State University",
        width="50%",
        height="auto"
      )
    )
    )
  ),
  
)

server <- function(input, output,session) {
  # # storing GEOIDs from table selection -------------------------------------
  RV<-reactiveValues(Clicks=list())
  
  # reactive geometry selection --------------------------------------------------
  # select table
  df1 <- reactive({
    envoData %>%
      dplyr::filter(area == input$Geom) ### need to change the input to spanish 
  })
  
  # generate map ------------------------------------------------------------
  ## tie all this into an external function just to clean up the server script. I want the
  ## server to be focused on reactive coded not the static stuff.
  output$mymap <- renderLeaflet({
    sm_Icon <- makeIcon("www/StoryMaps.png",
                        iconWidth = 40,
                        iconHeight = 40)
    map <- leaflet(options = leafletOptions(minZoom = 6)) %>%
      setView( lng = -105.76356278240084
               , lat = 39.13085942963124
               , zoom = 7 )%>%
      # add z levels ------------------------------------------------------------
    addMapPane("index", zIndex = 408) %>%
      addMapPane("binary", zIndex = 409) %>% # for the addPolyLine objects
      addMapPane("elements", zIndex = 410) %>%
      # add tiles ---------------------------------------------------------------
    addProviderTiles("CartoDB.DarkMatter", group = "Oscuro") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
      addProviderTiles("Stamen.Toner", group = "Claro")%>%
      # add search function -----------------------------------------------------
    leaflet.extras::addSearchOSM(
      options = leaflet.extras::searchOptions(autoCollapse = TRUE,
                                              hideMarkerOnCollapse = TRUE))%>%
      # add map reset -----------------------------------------------------------
    leaflet.extras::addResetMapButton() %>%
      # add spatial Data --------------------------------------------------------
    addPolygons(
      data = mapData,
      color = "#F9C1AE", #"#454547",
      weight = 0.2,
      smoothFactor = 0.5,
      opacity = 1.0,
      fillOpacity = 0.5,
      fillColor = ~ palMap(`Puntaje de Colorado EnviroScreen`),
      # https://stackoverflow.com/questions/48953149/dynamic-color-fill-for-polygon-using-leaflet-in-shiny-not-working
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      ),
      popup = mapData$popup,
      options = pathOptions(pane = "index"),
      layerId = mapData$GEOID,
      group = "Puntaje del indicador"
    )%>%
      addPolyLine(sf1 = oil, group = "Comunidad con petroleo y gas",
                  popup = "<strong>Definition: </strong> Counties that have active oil and gas operations.")%>%
      addPolyLine(sf1 = rural, group = "Comunidad rural",
                  popup = "<strong>Definition: </strong> Counties that do not contain a U.S. Census Bureau's urban area") %>%
      addPolyLine(sf1 = coal, group = "Comunidad con carbon",
                  popup = "<strong>Definition: </strong> Counties that have a coal-burning power plant.") %>%
      addPolygons(
        data = di,
        fillColor =  ~diPal(`color`),
        color = "#454547",
        weight = 1,
        fillOpacity = 0.8,
        popup = di$popup,
        group = "Comunidad afectada de manera desproporcionada",
        options = pathOptions(pane = "elements")
      )%>%
      addPolygons(
        data = justice40,
        popup = justice40$popup,
        fillColor  = "#fb9a99",
        fillOpacity = 0.8,
        color = "#636363",
        weight = 1,
        group = "Comunidad de Justice40",
        options = pathOptions(pane = "elements")
      )%>%
      addMarkers(
        data = sm,
        label = ~Area,
        popup = ~popup,
        # fillColor = "goldenrod",
        # fillOpacity = 1,
        # stroke = F,
        group = "Esquemas narrativos",
        options = pathOptions(pane = "elements"),
        icon = sm_Icon
      )%>%
      # add legend --------------------------------------------------------------
    addLegend(
      "topright",
      colors = colorRamp,
      title = "Valores aproxestimados.",
      labels = c(" Mayor carga", "", "", "", " Menos carga"),
      opacity = 1,
      layerId = "firstLegend",
      group = "Puntaje del indicador",
      na.label = "No Data"
    ) %>%
      addLegend("topright",
                colors = c("#a6cee3", "#33a02c","#b2df8a","#1f78b4"),
                title = "Comunidad afectada de manera desproporcionada",
                labels = c("Bajos ingresos", "Personas de color",
                           "Sobrecarga por gastos de vivienda", "Más de una categoría"),
                opacity = 1,
                group = "Comunidad afectada de manera desproporcionada"
      )%>%
      addLegendImage(images = "www/oilGas.png",
                     labels = "Comunidad con petróleo y gas",
                     width = 25,
                     height = 25,
                     position = 'topright',
                     group = "Comunidad con petroleo y gas",
                     labelStyle = "font-size: 16")%>%
      addLegendImage(images = "www/rural.png",
                     labels = "Comunidad rural",
                     width = 25,
                     height = 25,
                     position = 'topright',
                     group = "Comunidad rural",
                     labelStyle = "font-size: 16")%>%
      addLegendImage(images = "www/coal.png",
                     labels = "Comunidad con carbón",
                     width = 25,
                     height = 25,
                     position = 'topright',
                     group = "Comunidad con carbon",
                     labelStyle = "font-size: 16")%>%
      addLegend("topright",
                colors = "#fb9a99",
                labels =  "Comunidad de Justice40",
                opacity = 1,
                group = "Comunidad de Justice40"
      )%>%
      # add control groups ------------------------------------------------------
    addLayersControl(
      baseGroups = c("Claro","Oscuro", "OpenStreetMap"),
      overlayGroups = c(
        "Puntaje del indicador",
        "Comunidad con carbon",
        "Comunidad rural",
        'Comunidad con petroleo y gas',
        "Comunidad afectada de manera desproporcionada",
        "Comunidad de Justice40",
        "Esquemas narrativos"
      ),
      position = "topleft",
      options = layersControlOptions(collapsed = TRUE))%>%
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Map Layers</label>');
            $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Base Maps</label>');
        }
    ")%>%
      # hide layers (off when stating)
      hideGroup(
        group = c(
          "Comunidad con carbon",
          "Comunidad rural",
          'Comunidad con petroleo y gas',
          "Comunidad afectada de manera desproporcionada",
          "Comunidad de Justice40",
          "Esquemas narrativos"))
    map
  })
  
  
  # indicator summary -------------------------------------------------------
  # output for indicator summary'
  desc1 <- descriptors
  output$indicatorDesc <- renderText({
    ind1 <- input$Indicator
    desc1 <- descriptors %>% dplyr::filter(Indicator == ind1) %>% dplyr::select("Description") %>% pull()
    paste0(input$Indicator," : ", as.character(desc1))
  })
  

  # histograms --------------------------------------------------------------
  # font for title  
  fontHeader <- list(
    family = "museo-sans",
    color = "#000000",
    size = 18
  )
  bg_color <- "#FFFFFF" # removing the blue background for this 
  # font for labels
  fontBody <- list(
    family = "Trebuchet MS")
  # plot Margins 
  mrg <- list(l = 20, r = 20,
              b = 20, t = 30,
              pad = 10)
  
  output$histEnviroScreen <- renderPlotly({
    # setting text for font elements
    fontHeader <- list(
      family = "museo-sans",
      color = "#000000",
      size = 18
    )
  
        # condition for y axis label
    if(input$Geom == "Condado"){
      yaxisLabel <- "Cantidad de condados"
    }
    if(input$Geom == "Sector censal"){
      yaxisLabel <- "Cantidad de sectores censales"
    }
    if(input$Geom == "Grupo de bloques censales"){
      yaxisLabel <- "Cantidad de grupos de bloques censales"
    }
    bg_color <- "#FFFFFF" # removing the blue background for this
    
    
    # # filter to input parameter
    df2 <- df1() %>%
      dplyr::select("value" = "Puntaje de Colorado EnviroScreen", GEOID) %>%
      as.data.frame()
    
    # construct histograms to get bin splits
    t1 <- hist(df2$value)
    # # determine bins of histogram feature
    df2$bins <-  findInterval(x = df2$value, vec = t1$breaks)
    # set title for the plot
    title <- "Puntaje de Colorado EnviroScreen"
    xlabel <- "Carga"

    minBin <- min(t1$breaks)
    maxBin <- max(t1$breaks)
    
    colors <- df2 %>%
      dplyr::count(bins) %>%
      dplyr::mutate(color = "#6d3a5d")

    if(!is.null(input$mymap_shape_click$id)){
      ## determine if map click value is reflective of current map data
      if(input$mymap_shape_click[1] %in% df2$GEOID){
        binGroup <- df2[df2$GEOID == input$mymap_shape_click[1], "bins"]
        # set color
        colors <- df2%>%
          dplyr::count(bins)%>%
          dplyr::mutate(
            color = case_when(
              bins == binGroup  ~"#bc6123",
              TRUE ~"#6d3a5d"
            ))}}
      
    
    plot_ly(df2,x=~value,nbinsx = length(unique(df2$bins)))%>%
      add_histogram(
              marker = list(color = colors$color,
                            line = list(width = 0.5,
                                        color = 'rgb(0, 0, 0)')))%>%
        layout(title = list(text="Puntaje de Colorado EnviroScreen"
                            ,font  = fontHeader)
              ,xaxis = list(title = xlabel,
                             ticktext = list("Menor", "Mayor"),
                             tickvals = list(minBin,maxBin),
                             tickmode = "array",
                             tickangle = 45),
               yaxis = list(title = yaxisLabel),
               plot_bgcolor = bg_color,
               font = fontBody,
               margin = mrg)%>%
        hide_legend()%>%
        style(hoverinfo = 'none')
    
  })
  output$histExposure<- renderPlotly({
  # setting text for font elements
  fontHeader <- list(
    family = "museo-sans",
    color = "#000000",
    size = 18
  )
  
  # condition for y axis label
  if(input$Geom == "Condado"){
    yaxisLabel <- "Cantidad de condados"
  }
  if(input$Geom == "Sector censal"){
    yaxisLabel <- "Cantidad de sectores censales"
  }
  if(input$Geom == "Grupo de bloques censales"){
    yaxisLabel <- "Cantidad de grupos de bloques censales"
  }
  bg_color <- "#FFFFFF" # removing the blue background for this
  
  
  # # filter to input parameter
  df2 <- df1() %>%
    dplyr::select("value" = "Exposiciones ambientales", GEOID) %>%
    as.data.frame()
  
  # construct histograms to get bin splits
  t1 <- hist(df2$value)
  # # determine bins of histogram feature
  df2$bins <-  findInterval(x = df2$value, vec = t1$breaks)
  # set title for the plot
  title <- "Exposiciones ambientales"
  xlabel <- "Carga"
  
  minBin <- min(t1$breaks)
  maxBin <- max(t1$breaks)
  
  colors <- df2 %>%
    dplyr::count(bins) %>%
    dplyr::mutate(color = "#6d3a5d")
  
  if(!is.null(input$mymap_shape_click$id)){
    ## determine if map click value is reflective of current map data
    if(input$mymap_shape_click[1] %in% df2$GEOID){
      binGroup <- df2[df2$GEOID == input$mymap_shape_click[1], "bins"]
      # set color
      colors <- df2%>%
        dplyr::count(bins)%>%
        dplyr::mutate(
          color = case_when(
            bins == binGroup  ~"#bc6123",
            TRUE ~"#6d3a5d"
          ))}}
  
  
  plot_ly(df2,x=~value,nbinsx = length(unique(df2$bins)))%>%
    add_histogram(
      marker = list(color = colors$color,
                    line = list(width = 0.5,
                                color = 'rgb(0, 0, 0)')))%>%
    layout(title = list(text="Exposiciones ambientales"
                        ,font  = fontHeader)
           ,xaxis = list(title = xlabel,
                         ticktext = list("Menor", "Mayor"),
                         tickvals = list(minBin,maxBin),
                         tickmode = "array",
                         tickangle = 45),
           yaxis = list(title = yaxisLabel),
           plot_bgcolor = bg_color,
           font = fontBody,
           margin = mrg)%>%
    hide_legend()%>%
    style(hoverinfo = 'none')
  })
  output$histEffect<- renderPlotly({
  # setting text for font elements
  fontHeader <- list(
    family = "museo-sans",
    color = "#000000",
    size = 18
  )
  
  # condition for y axis label
  if(input$Geom == "Condado"){
    yaxisLabel <- "Cantidad de condados"
  }
  if(input$Geom == "Sector censal"){
    yaxisLabel <- "Cantidad de sectores censales"
  }
  if(input$Geom == "Grupo de bloques censales"){
    yaxisLabel <- "Cantidad de grupos de bloques censales"
  }
  bg_color <- "#FFFFFF" # removing the blue background for this
  
  
  # # filter to input parameter
  df2 <- df1() %>%
    dplyr::select("value" = "Efectos ambientales", GEOID) %>%
    as.data.frame()
  
  # construct histograms to get bin splits
  t1 <- hist(df2$value)
  # # determine bins of histogram feature
  df2$bins <-  findInterval(x = df2$value, vec = t1$breaks)
  # set title for the plot
  title <- "Efectos ambientales"
  xlabel <- "Carga"
  
  minBin <- min(t1$breaks)
  maxBin <- max(t1$breaks)
  
  colors <- df2 %>%
    dplyr::count(bins) %>%
    dplyr::mutate(color = "#6d3a5d")
  
  if(!is.null(input$mymap_shape_click$id)){
    ## determine if map click value is reflective of current map data
    if(input$mymap_shape_click[1] %in% df2$GEOID){
      binGroup <- df2[df2$GEOID == input$mymap_shape_click[1], "bins"]
      # set color
      colors <- df2%>%
        dplyr::count(bins)%>%
        dplyr::mutate(
          color = case_when(
            bins == binGroup  ~"#bc6123",
            TRUE ~"#6d3a5d"
          ))}}
  
  
  plot_ly(df2,x=~value,nbinsx = length(unique(df2$bins)))%>%
    add_histogram(
      marker = list(color = colors$color,
                    line = list(width = 0.5,
                                color = 'rgb(0, 0, 0)')))%>%
    layout(title = list(text="Efectos ambientales"
                        ,font  = fontHeader)
           ,xaxis = list(title = xlabel,
                         ticktext = list("Menor", "Mayor"),
                         tickvals = list(minBin,maxBin),
                         tickmode = "array",
                         tickangle = 45),
           yaxis = list(title = yaxisLabel),
           plot_bgcolor = bg_color,
           font = fontBody,
           margin = mrg)%>%
    hide_legend()%>%
    style(hoverinfo = 'none')
  })
  output$histClimate<- renderPlotly({
    # setting text for font elements
    fontHeader <- list(
      family = "museo-sans",
      color = "#000000",
      size = 18
    )
    
    # condition for y axis label
    if(input$Geom == "Condado"){
      yaxisLabel <- "Cantidad de condados"
    }
    if(input$Geom == "Sector censal"){
      yaxisLabel <- "Cantidad de sectores censales"
    }
    if(input$Geom == "Grupo de bloques censales"){
      yaxisLabel <- "Cantidad de grupos de bloques censales"
    }
    bg_color <- "#FFFFFF" # removing the blue background for this
    
    
    # # filter to input parameter
    df2 <- df1() %>%
      dplyr::select("value" = "Vulnerabilidad climática", GEOID) %>%
      as.data.frame()
    
    # construct histograms to get bin splits
    t1 <- hist(df2$value)
    # # determine bins of histogram feature
    df2$bins <-  findInterval(x = df2$value, vec = t1$breaks)
    # set title for the plot
    title <- "Vulnerabilidad climática"
    xlabel <- "Carga"
    
    minBin <- min(t1$breaks)
    maxBin <- max(t1$breaks)
    
    colors <- df2 %>%
      dplyr::count(bins) %>%
      dplyr::mutate(color = "#6d3a5d")
    
    if(!is.null(input$mymap_shape_click$id)){
      ## determine if map click value is reflective of current map data
      if(input$mymap_shape_click[1] %in% df2$GEOID){
        binGroup <- df2[df2$GEOID == input$mymap_shape_click[1], "bins"]
        # set color
        colors <- df2%>%
          dplyr::count(bins)%>%
          dplyr::mutate(
            color = case_when(
              bins == binGroup  ~"#bc6123",
              TRUE ~"#6d3a5d"
            ))}}
    
    
    plot_ly(df2,x=~value,nbinsx = length(unique(df2$bins)))%>%
      add_histogram(
        marker = list(color = colors$color,
                      line = list(width = 0.5,
                                  color = 'rgb(0, 0, 0)')))%>%
      layout(title = list(text="Vulnerabilidad climática"
                          ,font  = fontHeader)
             ,xaxis = list(title = xlabel,
                           ticktext = list("Menor", "Mayor"),
                           tickvals = list(minBin,maxBin),
                           tickmode = "array",
                           tickangle = 45),
             yaxis = list(title = yaxisLabel),
             plot_bgcolor = bg_color,
             font = fontBody,
             margin = mrg)%>%
      hide_legend()%>%
      style(hoverinfo = 'none')
  })
  output$histSocial<- renderPlotly({
    # setting text for font elements
    fontHeader <- list(
      family = "museo-sans",
      color = "#000000",
      size = 14
    )
    
    # condition for y axis label
    if(input$Geom == "Condado"){
      yaxisLabel <- "Cantidad de condados"
    }
    if(input$Geom == "Sector censal"){
      yaxisLabel <- "Cantidad de sectores censales"
    }
    if(input$Geom == "Grupo de bloques censales"){
      yaxisLabel <- "Cantidad de grupos de bloques censales"
    }
    bg_color <- "#FFFFFF" # removing the blue background for this
    
    
    # # filter to input parameter
    df2 <- df1() %>%
      dplyr::select("value" = "Poblaciones sensibles", GEOID) %>%
      as.data.frame()
    
    # construct histograms to get bin splits
    t1 <- hist(df2$value)
    # # determine bins of histogram feature
    df2$bins <-  findInterval(x = df2$value, vec = t1$breaks)
    # set title for the plot
    title <- "Poblaciones sensibles"
    xlabel <- "Susceptibilidad"
    
    minBin <- min(t1$breaks)
    maxBin <- max(t1$breaks)
    
    colors <- df2 %>%
      dplyr::count(bins) %>%
      dplyr::mutate(color = "#6d3a5d")
    
    if(!is.null(input$mymap_shape_click$id)){
      ## determine if map click value is reflective of current map data
      if(input$mymap_shape_click[1] %in% df2$GEOID){
        binGroup <- df2[df2$GEOID == input$mymap_shape_click[1], "bins"]
        # set color
        colors <- df2%>%
          dplyr::count(bins)%>%
          dplyr::mutate(
            color = case_when(
              bins == binGroup  ~"#bc6123",
              TRUE ~"#6d3a5d"
            ))}}
    
    
    plot_ly(df2,x=~value,nbinsx = length(unique(df2$bins)))%>%
      add_histogram(
        marker = list(color = colors$color,
                      line = list(width = 0.5,
                                  color = 'rgb(0, 0, 0)')))%>%
      layout(title = list(text="Poblaciones sensibles"
                          ,font  = fontHeader)
             ,xaxis = list(title = xlabel,
                           ticktext = list("Menor", "Mayor"),
                           tickvals = list(minBin,maxBin),
                           tickmode = "array",
                           tickangle = 45),
             yaxis = list(title = yaxisLabel),
             plot_bgcolor = bg_color,
             font = fontBody,
             margin = mrg)%>%
      hide_legend()%>%
      style(hoverinfo = 'none')
  })
  output$histDemo<- renderPlotly({
    # setting text for font elements
    fontHeader <- list(
      family = "museo-sans",
      color = "#000000",
      size = 14
    )
    
    # condition for y axis label
    if(input$Geom == "Condado"){
      yaxisLabel <- "Cantidad de condados"
    }
    if(input$Geom == "Sector censal"){
      yaxisLabel <- "Cantidad de sectores censales"
    }
    if(input$Geom == "Grupo de bloques censales"){
      yaxisLabel <- "Cantidad de grupos de bloques censales"
    }
    bg_color <- "#FFFFFF" # removing the blue background for this
    
    
    # # filter to input parameter
    df2 <- df1() %>%
      dplyr::select("value" = "Características demográficas", GEOID) %>%
      as.data.frame()
    
    # construct histograms to get bin splits
    t1 <- hist(df2$value)
    # # determine bins of histogram feature
    df2$bins <-  findInterval(x = df2$value, vec = t1$breaks)
    # set title for the plot
    title <- "Características demográficas"
    xlabel <- "Vulnerabilidad"
    
    minBin <- min(t1$breaks)
    maxBin <- max(t1$breaks)
    
    colors <- df2 %>%
      dplyr::count(bins) %>%
      dplyr::mutate(color = "#6d3a5d")
    
    if(!is.null(input$mymap_shape_click$id)){
      ## determine if map click value is reflective of current map data
      if(input$mymap_shape_click[1] %in% df2$GEOID){
        binGroup <- df2[df2$GEOID == input$mymap_shape_click[1], "bins"]
        # set color
        colors <- df2%>%
          dplyr::count(bins)%>%
          dplyr::mutate(
            color = case_when(
              bins == binGroup  ~"#bc6123",
              TRUE ~"#6d3a5d"
            ))}}
    
    
    plot_ly(df2,x=~value,nbinsx = length(unique(df2$bins)))%>%
      add_histogram(
        marker = list(color = colors$color,
                      line = list(width = 0.5,
                                  color = 'rgb(0, 0, 0)')))%>%
      layout(title = list(text="Características demográficas"
                          ,font  = fontHeader)
             ,xaxis = list(title = xlabel,
                           ticktext = list("Menor", "Mayor"),
                           tickvals = list(minBin,maxBin),
                           tickmode = "array",
                           tickangle = 45),
             yaxis = list(title = yaxisLabel),
             plot_bgcolor = bg_color,
             font = fontBody,
             margin = mrg)%>%
      hide_legend()%>%
      style(hoverinfo = 'none')
  })
      
      
      # table output ------------------------------------------------------------  
      # output for datatable based on columns selected
      tableData <- reactive({
        geoid1 <- input$mymap_shape_click
        if(is.null(geoid1$id)){
          geoid1 <- 1
        }
        
        if(input$tableSelect == "Descripción de los indicadores"){
          ## Need a GEOID value to support the click selection function
          descriptors %>%
            dplyr::mutate("GEOID" = NA)%>%
            dplyr::select("GEOID","Indicator Name"="Indicator"
                          ,"Data Source"= "Source"
                          ,"Date (data collection)"= "Date"
                          ,"Units"
                          ,"Measured Geography" = "Measured_Geography")
        }else{
          # primary table. 
          table1 <- df1() %>% sf::st_drop_geometry()
          # sort table if geoid has been selected
          if(geoid1[1] %in% table1$GEOID){
            #sort table by GEOID
            table1 <- setorder(x = table1, GEOID, na.last= TRUE)

            feature <- grep(pattern = geoid1[1], x = table1$GEOID)
            # order based on selected values
            order2 <- c(feature:nrow(table1), 1:(feature-1))

            table1 <- table1[order2, ]
          }

          #select columns based on input
          
          if(input$tableSelect == "Puntaje de los componentes en conjunto"){
            # df1()
            table1 %>%
              select(
                "GEOID",
                "Nombre del condado",
                "Percentil del puntaje de Colorado EnviroScreen",
                "Puntaje de Colorado EnviroScreen",
                "Percentil de contaminación y carga climática",
                "Contaminación y carga climática",
                "Percentil de factores de salud y sociales",
                "Factores de salud y sociales"
              )
          } else if(input$tableSelect == "Puntaje de los componentes") {
            table1 %>% 
              select(
                "GEOID",
                "Nombre del condado",
                "Percentil de exposiciones ambientales",
                "Exposiciones ambientales",
                "Percentil de efectos ambientales",
                "Efectos ambientales",
                "Percentil de vulnerabilidad climática",
                "Vulnerabilidad climática",
                "Percentil de poblaciones sensibles",
                "Poblaciones sensibles",
                "Percentil de características demográficas",
                "Características demográficas"
              )
          } else if(input$tableSelect == "Exposiciones ambientales") {
            table1 %>% 
              select(
                "GEOID",
                "Nombre del condado"
                ,"Ozono"
                ,"Percentil de ozono"
                ,"Contaminación por partículas finas"
                ,"Percentil de contaminación por partículas finas"
                ,"Riesgo de exposición al plomok"
                ,"Percentil de riesgo de exposición al plomo"
                ,"Material particulado (PM) de diésel"
                ,"Percentil de material particulado (PM) de diésel"
                ,"Proximidad y volumen de tránsito"
                ,"Percentil de proximidad y volumen de tránsito"
                ,"Emisiones de contaminantes tóxicos del aire"
                ,"Percentil de emisiones de contaminantes tóxicos del aire"
                ,"Otros contaminantes del aire"
                ,"Percentil de otros contaminantes del aire" 
                ,"Reglamentos sobre agua potable"
                ,"Percentil de reglamentos sobre agua potable"
                ,"Ruido"
                ,"Percentil de ruido"
              )
          } else if(input$tableSelect == "Efectos ambientales") {
            table1 %>% 
              select(
                "GEOID"
                ,"Nombre del condado"
                ,"Indicador de descargas de aguas residuales"
                ,"Percentil del indicador de descargas de aguas residuales"
                ,"Proximidad a los sitios de la Lista Nacional de Prioridades"
                ,"Percentil de proximidad a los sitios de la Lista Nacional de Prioridades"
                ,"Proximidad a los sitios del Plan de Gestión de Riesgos"
                ,"Percentil de proximidad a los sitios del Plan de Gestión de Riesgos"
                ,"Proximidad a instalaciones de residuos peligrosos"
                ,"Percentil de proximidad a instalaciones de residuos peligrosos"
                ,"Proximidad a petróleo y gas" 
                ,"Percentil de proximidad a petróleo y gas" 
                ,"Proximidad a ubicaciones de minería" 
                ,"Percentil de proximidad a ubicaciones de minería" 
                ,"Arroyos y ríos deteriorados" 
                ,"Percentil de arroyos y ríos deteriorados"
              )
          }  else if(input$tableSelect == "Vulnerabilidad climática") {
            table1 %>% 
              select(
                "GEOID"
                ,"Nombre del condado"
                ,"Riesgo de incendios forestales"
                ,"Percentil de riesgo de incendios forestales"
                ,"Llanuras aluviales"
                ,"Percentil de llanuras aluviales"
                ,"Sequía"
                ,"Percentil de sequía"
                ,"Días de calor extremo"
                ,"Percentil de días de calor extremo"
              )
          } else if(input$tableSelect == "Poblaciones sensibles") {
            table1 %>% 
              select(
                "GEOID"
                ,"Nombre del condado"
                ,"Población por debajo de 5 años"
                ,"Percentil de población por debajo de 5 años"
                ,"Población por encima de 64 años"
                ,"Percentil de población por encima de 64 años"
                ,"Cardiopatías en adultos"
                ,"Percentil de cardiopatías en adultos"
                ,"Tasa de hospitalización por asma"
                ,"Percentil de tasa de hospitalización por asma"
                ,"Expectativa de vida"
                ,"Percentil de expectativa de vida"
                ,"Bajo peso al nacer"
                ,"Percentil de bajo peso al nacer"
                ,"Predominio de cáncer"
                ,"Percentil de predominio de cáncer"
                ,"Predominio de diabetes"
                ,"Percentil de predominio de diabetes"
                ,"Indicador de salud mental"
                ,"Percentil del indicador de salud mental"
              )
          } else if(input$tableSelect == "Características demográficas") {
            table1 %>%  
              select(
                "GEOID"
                ,"Nombre del condado"
                ,"Porcentaje de personas de color"
                ,"Percentil del porcentaje de personas de color" 
                ,"Porcentaje que no completaron los estudios de secundaria"
                ,"Percentil del porcentaje que no completaron los estudios de secundaria"
                ,"Porcentaje de bajos ingresos"
                ,"Percentil del porcentaje de bajos ingresos"
                ,"Porcentaje de aislamiento lingüístico"
                ,"Percentil del porcentaje de aislamiento lingüístico"
                ,"Porcentaje de discapacidades"
                ,"Percentil del porcentaje de discapacidades"
                ,"Sobrecarga por gastos de vivienda" 
                ,"Percentil de sobrecarga por gastos de vivienda"
              )
          }  else if(input$tableSelect == "Clasificación de las comunidades") {
            table1 %>%  
              select(
                "GEOID",
                "Nombre del condado",
                "Comunidad afectada de manera desproporcionada",
                "Comunidad de Justice40"
                ,"Comunidad con carbón"
                ,"Comunidad con petróleo y gas"
                ,"Comunidad rural", 
                "Total de la población"
              )
          } 

        }
      })
      
      
      # storing GEOIDs from table/map selection -------------------------------------
      RV<-reactiveValues()

      observeEvent(input$tableAll_rows_selected, {
        RV$select <- isolate(tableData() %>% dplyr::slice(input$tableAll_rows_selected) %>% pull(GEOID))
      })

      observeEvent(input$mymap_shape_click, {
        RV$select <- isolate(input$mymap_shape_click$id)
      })


      # Render the table outputs ------------------------------------------------

      output$tableAll <- renderDataTable({
        DT::datatable(tableData(),
                      options = list(autoWidth = TRUE, scrollX = TRUE))
      })
      
      # Table proxy for selection
      observe({
        DT::dataTableProxy("tableAll") %>%
          selectRows(which(tableData()$GEOID %in% RV$select))
      })

      # download data -----------------------------------------------------------
      # Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(input$Geom, "_data.csv", sep = "")
        },
        content = function(file) {
          write.csv(df1() %>% sf::st_drop_geometry() %>% select(-"visParam"), file, row.names = FALSE)    }
      )
      # Downloadable csv of data description ----
      output$downloadData2 <- downloadHandler(
        filename = function() {
          "enviroscreenDataDescriptions.csv"
        },
        content = function(file) {
          write.csv(descriptors, file, row.names = FALSE)
        }
      )
      
      # proxy map elements  -----------------------------------------------------
      observeEvent(input$updateMap, {
        ### helpful source https://stackoverflow.com/questions/37433569/changing-leaflet-map-according-to-input-without-redrawing
        # geography
        geo <- input$Geom

        # indicator
        in1 <- input$Indicator
        # need to lower the first letter in the string but nothing else
        # grab first letter and set to lower
        t1 <- tolower(str_sub(in1, 1, 1))
        # subset out first letter
        t2 <- str_sub(in1, 2, nchar(in1))
        # construct new string
        indicator1 <- in1
        # sometimes it's del sometimes it de
        if(in1 %in% c("Puntaje de Colorado EnviroScreen"
                      ,"Indicador de descargas de aguas residuales"
                      ,"Indicador de salud mental"
                      ,"Porcentaje de discapacidades"
        )){
          indicator2 <- paste0("Percentil del ", t1,t2)
        }else{
          indicator2 <- paste0("Percentil de ", t1,t2)
        }

        if(input$Percentile == "Valor medido"){
          indicator <- indicator1
        }
        if(input$Percentile == "Rango percentil"){
          indicator <- indicator2
        }

        # filter and assign visparam
        ed2 <- envoData[envoData$area == geo, ]
        ed2 <- ed2 %>%
          mutate(visParam = !!as.symbol(indicator))%>%# https://stackoverflow.com/questions/62862705/r-shiny-mutate-replace-how-to-mutate-specific-column-selected-from-selectinput
          dplyr::select(GEOID,
                        `Nombre del condado`,
                        indicator1,
                        indicator2,
                        `Comunidad con carbón`
                        ,`Comunidad con petróleo y gas`
                        ,`Comunidad rural`
                        ,visParam)

        ed2 <- ed2 %>%
          dplyr::mutate(
            popup = paste0(
              "<br/><strong>", as.character(in1),"</strong>", # needs to be text
              paste0("<br/><strong>",`Nombre del condado`,"</strong>"),
              paste0("<br/><b>Medida:</b> ", round(!!as.symbol(indicator1), digits = 2),
                       "<br/><b>Puntaje:</b> ", round(!!as.symbol(indicator2), digits =  0)),
              paste0("<br/><b>Comunidad con carbón:</b> ", `Comunidad con carbón`),
              paste0("<br/><b>Comunidad con petróleo y gas:</b> ", `Comunidad con petróleo y gas`),
              paste0("<br/><b>Comunidad rural:</b> ", `Comunidad rural`)
          )
        )

        # palette
        pal1 <- leaflet::colorNumeric(palette = colorRamp,
                                      domain = ed2$visParam,
                                      reverse = TRUE)
        # legend labels
        labels1 <- defineLegend(in1)
        leafletProxy("mymap") %>%
          clearGroup(group = "Puntaje del indicador") %>%
          addPolygons(
            data = ed2,
            color = "#F9C1AE",
            weight = 0.2,
            smoothFactor = 0.5,
            opacity = 1.0,
            layerId = ed2$GEOID,
            fillOpacity = 0.5,
            fillColor =  ~pal1(ed2$visParam),
            popup = ed2$popup,
            highlightOptions = highlightOptions(
              color = "white",
              weight = 2,
              bringToFront = TRUE
            ),
            options = pathOptions(pane = "index"),
            group = "Puntaje del indicador"

          )%>%
          removeControl(layerId = "firstLegend")%>%
          addLegend(
            "topright",
            colors = colorRamp,
            title = "Valores aprox.",
            labels = labels1,
            opacity = 1,
            layerId = "firstLegend",
            group = "Puntaje del indicador"
            # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          )
      })

      # add selected features to map --------------------------------------------
      observeEvent(input$button_table, {
        mapFeatures <- envoData %>% select("GEOID") %>% dplyr::filter(GEOID %in% RV$select)
          # add features to map
          leafletProxy("mymap") %>%
            clearGroup(group = "Table Highlight") %>%
            addPolygons(
              data = mapFeatures,
              fillColor = "#fffb17",
              fillOpacity = 0.3,
              color = "#fffb17",
              options = pathOptions(pane = "index"),
              group = "Table Highlight"
            )
        })

      # remove selected features from map
      observeEvent(input$removeHighlight, {
        # add features to map
        leafletProxy("mymap") %>%
          clearGroup(group = "Table Highlight")
      })
}
# Run the application
shinyApp(ui = ui, server = server)

