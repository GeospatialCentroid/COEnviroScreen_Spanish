#' createMap
#' @description : generates the base leaflet map for the applicaiton.
#'
#' @param mapData : SF object containing all data for the initial map.
#' @param pal : palette to be used in the legend
#' @param palMap : the leaflet palette option to but used for the map visualization
#'
#' @return leaftlet map object
#' @export
#'
#'
createMap <- function(mapData,pal, palMap, diPal, oil, rural, coal, di, justice40, storyMaps) {
  #story map icon 
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
  addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
    addProviderTiles("Stamen.Toner", group = "Light")%>%
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
    group = "Indicator Score"
  )%>%
    addPolyLine(sf1 = oil, group = "Oil and Gas Community", 
                popup = "adding definition") %>%
    addPolyLine(sf1 = rural, group = "Urban/Rural", 
                popup = "adding definition") %>%
    addPolyLine(sf1 = coal, group = "Coal Community", 
                popup = "adding definition") %>%
    addPolygons(
      data = di,
      fillColor =  ~diPal(`color`),
      color = "#454547",
      weight = 1,
      fillOpacity = 0.8,
      popup = di$popup,
      group = "Disproportionately Impacted Community",
      options = pathOptions(pane = "elements")
    )%>%
    addPolygons(
      data = justice40,
      popup = justice40$popup,
      fillColor  = "#fb9a99",
      fillOpacity = 0.8,
      color = "#636363",
      weight = 1,
      group = "Justice40",
      options = pathOptions(pane = "elements")
    )%>%
    addMarkers(
      data = storyMaps,
      label = ~Area,
      popup = ~popup,
      group = "Story Maps",
      options = pathOptions(pane = "elements"),
      icon = sm_Icon
    )%>%
    # add legend --------------------------------------------------------------
  addLegend(
    "topright",
    colors = pal,
    title = "Valores aprox.",
    labels = c("Mayor carga", "", "", "", "Menos carga"),
    opacity = 1,
    layerId = "firstLegend",
    group = "Indicator Score",
    na.label = "No Data"
    
    # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
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
                   group = "Comunidad con petróleo y gas",
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
                   group = "Comunidad con carbón",
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
      "Comunidad con carbón",
      "Comunidad rural",
      "Comunidad con petróleo y gas",
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
        "Comunidad con carbón",
        "Comunidad rural",
        "Comunidad con petróleo y gas",
        "Comunidad afectada de manera desproporcionada",
        "Comunidad de Justice40",
        "Esquemas narrativos"))
  
  return(map)
}