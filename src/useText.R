# useText

use1 <- function(){
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
  )
}

use2 <- function(){
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
             )
}

use3 <- function(){
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
             )
}

use4 <- function(){
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
             )
}

use5 <- function(){
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
             )
}

use6 <- function(){
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
             )
}

use7 <- function(){
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
             )
}

use8 <- function(){
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
             )
}

use9 <- function(){
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
             )
}

use10 <- function(){
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
}

