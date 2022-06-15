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

# set spanish encoding 
Sys.setlocale(locale = "UTF-8")

# source helpers ----------------------------------------------------------
lapply(list.files(path = "src",recursive = TRUE, full.names = TRUE), source)

version <- 4

# enviroscreen data
envoData <- readRDS(paste0("data/scores/allScores_",version,"_Spanish.rds"))%>%
  dplyr::mutate(visParam = `Percentil del puntaje de Colorado EnviroScreen`)%>%
  dplyr::select("Nombre del condado", "GEOID", everything())%>% 
  dplyr::select(-"GEOID3")
names(envoData) <- iconv(names(envoData), to = "UTF-8")


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
    ,paste0("<strong>Definition: </strong>")
    ,"In early 2022, the White House launched the Justice40 Initiative. The goal of the Justice40 Initiative is to provide 40 percent of the overall benefits of certain Federal investments in seven key areas to disadvantaged communities. These seven key areas are: climate change, clean energy and energy efficiency, clean transit, affordable and sustainable housing, training and workforce development, the remediation and reduction of legacy pollution, and the development of critical clean water infrastructure. According to the definition of Justice40, a community qualifies as “disadvantaged,” if the census tract is above the threshold for one or more environmental or climate indicators and the tract is above the threshold for the socioeconomic indicators."
    ))

# di community 
di <- getDI()
# storyMap Locations 
sm <- getStoryMaps()

# palette for DI layer 
diPal <- colorFactor(palette = c(
  "#a6cee3", "#33a02c","#b2df8a","#1f78b4"), levels = c("Low Income", "People of Color",
                                                       "Housing Burden", "More then one category"), di$color
)

### light as low 
#colorRamp <- c(  "#f2f0f7"  ,"#cbc9e2"  ,"#9e9ac8"  ,"#756bb1"  ,"#54278f")
### dark as low 
colorRamp <- c( "#54278f","#756bb1","#9e9ac8","#cbc9e2","#f2f0f7")



# create initial dataset for map  -----------------------------------------
mapData <- initialMapData(envoData)
# palette for the map
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



# UI  ---------------------------------------------------------------------
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
    column(8, h1("Puntaje de Colorado EnviroScreen"), p("June 2022"))
  ),
  br(),
  fluidRow(
    p(HTML("</br><a href='#map'>Jump to Map</a>")),
    p("Colorado Enviroscreen is an interactive environmental justice mapping tool and health screening tool for Colorado. It was developed for the Colorado Department of Public Health and Environment (CDPHE) by a team from Colorado State University. Colorado EnviroScreen Version 1.0 launched on June XX, 2022. You can learn more about Colorado EnviroScreen on CDPHE’s ",
      tags$a(href = "https://cdphe.colorado.gov/enviroscreen", "Colorado EnviroScreen webpage.", target = "_blank"),
      "You can send feedback about Colorado EnviroScreen to CDPHE by emailing cdphe_ej@state.co.us"    
    )
  ),
  fluidRow(
    p("Colorado EnviroScreen:"),
    p(
      tags$ul(
        tags$li("Identifies areas with current and past environmental inequities."),
        tags$li("Help users advocate for funding, interventions, and policy changes to avoid, minimize, and mitigate environmental health risksPinpoints areas where disproportionately impacted communities have a greater health burden and/or face more environmental risks."),
        tags$li("dentifies geographically disproportionately impacted communities based on the definition in Colorado’s Environmental Justice Act (House Bill 21-1266).")
      )
    )
  ),
  fluidRow(
    p("Colorado EnviroScreen is intended to:"),
    p(
      tags$ul(
        tags$li("Help users advocate for funding, interventions, and policy changes to avoid, lessen, and mitigate environmental health risks."),
        tags$li("Advance a healthy and sustainable Colorado where everyone has the same degree of protection from environmental and health hazards.")
      )
    )
  ),
  fluidRow(
    p("Click here for more information about CDPHE’s work to ",
      tags$a(href = "https://cdphe.colorado.gov/environmental-justice", "advance environmental justice", target = "_blank"),
      ", ",
      tags$a(href = "https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology", "understand the connection between the environment and our health", target = "_blank"),
      " , and ",
      tags$a(href = "https://cdphe.colorado.gov/air-pollution/climate-change#equity", "advance climate equity.", target = "_blank"),
     )),
  fluidRow(
           p("Scroll down for more information about how to use Colorado EnviroScreen.")
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
    p("The EnviroScreen score combines five components: Environmental exposures, Environmental effects, Climate vulnerability, Sensitive population, and Demographics. When you click a location on the map, the orange bars in this chart show the score for that location. The orange bars show how the location compares to the rest of Colorado for each component score. Together, the charts show how the EnviroScreen score is calculated for the selected location.")
  ),
  br(),
  
  # show reactive table -----------------------------------------------------
  # table showing the results
  fluidRow(class = "sectionTitle",
           h2("EnviroScreen Score Data"),
           p("Use the tabs above  the table to filter through different elements of the
    Colorado EnviroScreen Score. You can select specific rows in the table, then hit the
    Orange `Highlight Selection on Map` button below the table to view the location on the map."),

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
                        "For more information about how to use environmental justice mapping tools, 
    please review the Climate Equity Data Viewer guide (available in", 
    tags$a(href = "https://drive.google.com/file/d/1iytdPG5iK2VBNpIy8k6oT6lU6-QKMLOa/view?usp=sharing",
           tags$span(style="color:white","English"), target = "_blank"),
    "and ",
    tags$a(href = "https://drive.google.com/file/d/17rQ90fNt3DF-0PbySpGjo2tiy9AmDiCc/view?usp=sharing",
           tags$span(style="color:white","Spanish"), target = "_blank"),
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

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # storing GEOIDs from table selection -------------------------------------
  RV<-reactiveValues(Clicks=list())

  # reactive geometry selection --------------------------------------------------
  # select table
  df1 <- reactive({
    envoData %>%
      dplyr::filter(area == input$Geom)
  })

    # generate map ------------------------------------------------------------
  ### tie all this into an external function just to clean up the server script. I want the 
  ### server to be focused on reactive coded not the static stuff. 
   output$mymap <- renderLeaflet({
     # render leaflet object
     createMap(mapData = mapData, di = di, diPal = diPal,
              pal = colorRamp, palMap = palMap,
              oil=oil, rural = rural, coal = coal, justice40 = justice40,
              storyMap = sm)
    })

  # indicator summary -------------------------------------------------------
  # output for indicator summary'
  desc1 <- descriptors
  output$indicatorDesc <- renderText({
    ind1 <- input$Indicator
    desc1 <- descriptors %>% dplyr::filter(Indicator == ind1) %>% dplyr::select("Description") %>% pull()
    paste0(input$Indicator," : ", as.character(desc1))
  })


  # histogram plots ---------------------------------------------------------
  # if input changes reset map value 
  plotGroup <- c( "Puntaje de Colorado EnviroScreen", "Exposiciones ambientales",
                  "Efectos ambientales","Vulnerabilidad climática",
                  "Poblaciones sensibles","Características demográficas")
  
  ### need individual output objects for each plot 
  # output$histEnviroScreen <- renderPlotly({
  #   genPlots(dataframe = df1(),parameter = "Puntaje de Colorado EnviroScreen", geometry = input$Geom, geoid = input$mymap_shape_click)
  # })
  # output$histExposure<- renderPlotly({
  #   genPlots(dataframe = df1(),parameter = "Exposiciones ambientales",geometry = input$Geom, geoid = input$mymap_shape_click)
  # })
  # output$histEffect<- renderPlotly({
  #   genPlots(dataframe = df1(),parameter = "Efectos ambientales",geometry = input$Geom, geoid = input$mymap_shape_click)
  # })
  # output$histClimate<- renderPlotly({
  #   genPlots(dataframe = df1(),parameter = "Vulnerabilidad climática",geometry = input$Geom, geoid = input$mymap_shape_click)
  # })
  # output$histSocial<- renderPlotly({
  #   genPlots(dataframe = df1(),parameter = "Poblaciones sensibles",geometry = input$Geom, geoid = input$mymap_shape_click)
  # })
  # output$histDemo<- renderPlotly({
  #   genPlots(dataframe = df1(),parameter = "Características demográficas",geometry = input$Geom, geoid = input$mymap_shape_click)
  # })
    
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
        dplyr::select("GEOID","Indicator Name"="Indicator","Data Source"= "Source",
                      "Date (data collection)"= "Date","Units","Measured Geography" = "Measured_Geography")
    }else{
      genTable(tableData = df1(), geoid = geoid1, colSelected = input$tableSelect)
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
    #selection = list(mode = 'multiple', selected = which(tableData()$GEOID %in% RV$select)))
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
    ### Test on local system and this works
    # leaflet()%>%
    #   addPolygons(
    #     data = ed2,
    #     color = "#F9C1AE", 
    #     weight = 0.2,
    #     smoothFactor = 0.5,
    #     opacity = 1.0,
    #     layerId = ed2$GEOID,
    #     fillOpacity = 0.5,
    #     fillColor =  ~pal1(ed2$visParam),
    #     popup = ed2$popup)
      
    # legend labels 
    labels1 <- defineLegend(in1)
    leafletProxy("mymap") %>%
      clearGroup(group = "Indicator Score") %>%
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
        group = "Indicator Score"
        
      )%>%
      removeControl(layerId = "firstLegend")%>%
      addLegend(
        "topright",
        colors = colorRamp,
        title = "Valores aprox.",
        labels = labels1,
        opacity = 1,
        layerId = "firstLegend",
        group = "Indicator Score"
        # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )
  })
  # add selected features to map 
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
  
  # remov-e selected features from map 
  observeEvent(input$removeHighlight, {
    # add features to map 
    leafletProxy("mymap") %>%
      clearGroup(group = "Table Highlight") 
  })

  # click observer event ----------------------------------------------------
  observeEvent(input$mymap_shape_click, {
    geoidMap <- input$mymap_shape_click$id  # typo was on this line
  })
}

# Run the application
shinyApp(ui = ui, server = server)

