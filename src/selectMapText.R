#text for the Select Reactive Elements section 
# group all non reactive text elements by fluid rows.Largerly for organization.


mapText1 <- function(){
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
                               "Puntaje de los componentes agrupados" = c("Contaminación y carga climática",
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
                                                              "Proximidad y volumen de tráfico"
                               ),
                               "Efectos ambientales" = c("Arroyos y ríos deteriorados",
                                                         "Proximidad a instalaciones de residuos peligrosos",
                                                         "Proximidad a minas",
                                                         "Proximidad a los sitios de la Lista Nacional de Prioridades",
                                                         "Proximidad a petróleo y gas",
                                                         "Proximidad a los sitios del Plan de Gestión de Riesgos",
                                                         "Indicador de descargas de aguas residuales"
                               ),
                               "Vulnerabilidad climática" = c("Sequía",
                                                              "Días de calor extremo",
                                                              "Inundación (planicies aluviales)",
                                                              "Riesgo de incendios forestales"
                               ),
                               "Poblaciones sensibles" = c("Tasa de hospitalización por asma",
                                                           "Prevalencia de cáncer",
                                                           "Prevalencia de diabetes",
                                                           "Enfermedades cardiacas en adultos",
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

  )
}
