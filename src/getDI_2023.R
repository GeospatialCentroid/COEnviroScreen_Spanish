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
getDI_2023 <- function(){
  
  diCommunity_2023 <- readRDS("data/scores/diCommunities_2023.rds")%>%
    mutate(
      Mn_FLAG = case_when(
        Min_FLAG == 1 ~ "Sí",
        Min_FLAG == 0 ~ "No"
      ),
      FLP_FLA = case_when(
        FLP_FLAG == 1 ~ "Sí",
        FLP_FLAG == 0 ~ "No"
      ),
      Br_FLAG = case_when(
        Burdened_FLAG == 1 ~ "Sí",
        Burdened_FLAG == 0 ~ "No"
      ),
      Ling_FLAG = case_when(
        LingIso_FLAG == 1 ~ "Sí",
        LingIso_FLAG == 0 ~ "No"
      ),
      J40_FLAG = case_when(
        Justice40_FLAG == 1 ~ "Sí",
        Justice40_FLAG == 0 ~ "No"
      ),
      Trib_FLAG = case_when(
        TribalLands_FLAG == 1 ~ "Sí",
        TribalLands_FLAG == 0 ~ "No"
      ),
      Sc_FLAG = case_when(
        Score_FLAG == 1 ~ "Sí",
        Score_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><strong>Definición actual de comunidad afectada de manera desproporcionada (mayo de 2023):</strong>",
               "<br/><b>Grupo de manzanas censales: </b>", GEOID,
               "<br/>",
               "<br/><b>Más del 40 % de las viviendas son de bajos ingresos: </b>", FLP_FLA,
               "<br/><b>Porcentaje de bajos ingresos: </b>", round(Pov_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 40 % de las viviendas están integradas por personas de color : </b>", Mn_FLAG,
               "<br/><b>Porcentaje de personas de color: </b>", round(Min_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 50 % de las viviendas experimentan sobrecarga por gastos de vivienda : </b>", Br_FLAG,
               "<br/><b>Porcentaje con sobrecarga por gastos de vivienda: </b>", round(HH_Burdened_Pct*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 20 % de la población está aislada desde el punto de vista lingüístico: </b>", Ling_FLAG,
               "<br/><b>Porcentaje de aislamiento lingüístico: </b>", round(LingIso_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Área bajo jurisdicción tribal: </b>", Trib_FLAG,
               "<br/>",
               "<br/><b>Esta área se considera como desfavorecida de acuerdo con la herramienta de evaluación de justicia climática y económica (Justice40): </b>", J40_FLAG,
               "<br/>",
               "<br/><b>El puntaje de EnviroScreen (percentil) es mayor que 80: </b>", Sc_FLAG,
               "<br/><b>Puntaje de EnviroScreen (percentil): </b>", round(EnviroScreen_Pctl, digits = 1),
               "<br/>",
               "<br/>",
               "Lea la definición de comunidad afectada de manera desproporcionada de Colorado en " 
               ,tags$a(href = "https://cdphe.colorado.gov/environmental-justice", " el sitio web del Programa de Justicia Ambiental del CDPHE.", target = "_blank")
               
             )
    )%>%
    mutate(
      color = as.factor(case_when(
        Mn_FLAG == "Sí" & DI_communityCount == 1 ~ "People of Color",
        FLP_FLA == "Sí" & DI_communityCount == 1 ~ "Low Income",
        Br_FLAG == "Sí" & DI_communityCount == 1 ~ "Housing Burden",
        Ling_FLAG == "Sí" & DI_communityCount == 1 ~ "Linguistically isolated",
        J40_FLAG == "Sí" & DI_communityCount == 1 ~ "Federally identified (CEJST)",
        Trib_FLAG == "Sí" & DI_communityCount == 1 ~ "Tribal Lands",
        Sc_FLAG == "Sí" & DI_communityCount == 1 ~ "EnviroScreen Score",
        TRUE ~ "Más de una categoría"
      ))
    )%>%
    as('sf')
  return(diCommunity_2023)
  
  
}