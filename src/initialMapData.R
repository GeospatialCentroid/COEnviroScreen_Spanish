#' initialMapData
#'
#' @param data1 : sf object with EnviroScreen Score 
#' @description : generate the spatial data for the intital leaflet map 
#' @return
#' @export
#'
#' @examples
initialMapData <- function(data1){
  ### need to replace special characters for the popup element  
  d1 <- data1 %>%
    dplyr::filter(area == "County")%>%
    dplyr::select(GEOID,"Puntaje de Colorado EnviroScreen", 
                  "Percentil del puntaje de Colorado EnviroScreen", 
                  "Nombre del condado",
                  visParam,
                  "ccc"= 93,
                  "ccpg" = 94,
                  "Comunidad rural")%>%
    dplyr::mutate(
      popup = paste0(
        "<br/><strong>Puntaje de Colorado EnviroScreen</strong>", # needs to be text
        paste0("<br/><strong>",`Nombre del condado`,"</strong>"),
        paste0("<br/><b>Medino:</b> ", round(`Puntaje de Colorado EnviroScreen`, digits = 2),
               "<br/><b>Puntaje:</b> ", as.character(round(`Percentil del puntaje de Colorado EnviroScreen`), digits =  0)),
        paste0("<br/><b>Comunidad con carbón:</b> ", `ccc`),
        paste0("<br/><b>Comunidad con petróleo y gas:</b> ", `ccpg`),
        paste0("<br/><b>Comunidad rural:</b> ", `Comunidad rural`)
      )
    )
  
  d1 <- as(d1, "sf")
  return(d1)
}