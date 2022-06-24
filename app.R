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
envoData <- readRDS("data/scores/allScores_4_spanish.rds")%>%
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
  title1(),
  br(),
  title2(),
  title3(),
  title4(),
  title5(),
  # title6(),
  
  # # description of use ------------------------------------------------------
  fluidRow(class = "sectionTitle",
           h2("Understanding the EnviroScreen Tool")
  ),
  # 
  tabsetPanel(
      use1(),
      use2(),
      use3(),
      use4(),
      use5(),
      use6(),
      use7(),
      use8(),
      use9(),
      use10(),
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

