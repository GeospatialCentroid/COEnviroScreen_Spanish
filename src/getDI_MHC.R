#' generate di community 
#' @description : produces a sf object of disporpotionally impacted communities
#'
#' @return : sf object 
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
getDI_MHC <- function(){
  
  diCommunity_MHC <- readRDS("data/scores/diCommunities_MHC.rds")%>%
    mutate(popup =
             paste0(
               "<br/><strong>Comunidad afectada de manera desproporcionada: Comunidad de casas móviles </strong>",
               "<br/><b>Nombre: </b>", Park.Name,
               "<br/>",
               "<br/>",
               "Lea la definición de comunidad afectada de manera desproporcionada de Colorado en " 
               ,tags$a(href = "https://cdphe.colorado.gov/environmental-justice", " el sitio web del Programa de Justicia Ambiental del CDPHE.", target = "_blank")
               
             )
    )%>%
    mutate(
      color = "black"
    )%>%
    as('sf')
  return(diCommunity_MHC)
  
  
}