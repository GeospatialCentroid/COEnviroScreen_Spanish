#' generate di community 
#' @description : produces a sf object of disproportionately impacted communities
#'
#' @return : sf object 
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
getDI_AQCC <- function(){

  diCommunity_AQCC <- readRDS("data/scores/diCommunities_AQCC.rds")%>%
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
      Sc_FLAG = case_when(
        Score_FLAG == 1 ~ "Sí",
        Score_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><strong>Reglamento 3 de la AQCC - Comunidad afectada de manera desproporcionada</strong>",
               "<br/><b>Grupo de manzanas censales: </b>", GEOID,
               "<br/>",
               "<br/><b>Vulnerabilidades socioeconómicas:</b>",
               "<br/>",
               "<br/><b>Más del 40 % de los hogares son de bajos ingresos: </b>", FLP_FLA,
               "<br/><b>Porcentaje de bajos ingresos: </b>", round(Pov_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 40 % de los hogares están integrados por personas de color : </b>", Mn_FLAG,
               "<br/><b>Porcentaje de personas de color: </b>", round(Min_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 50 % de los hogares experimentan sobrecarga por gastos de vivienda : </b>", Br_FLAG,
               "<br/><b>Porcentaje con sobrecarga por gastos de vivienda: </b>", round(HH_Burdened_Pct*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 20 % de la población está aislada desde el punto de vista lingüístico: </b>", Ling_FLAG,
               "<br/><b>Porcentaje de aislamiento lingüístico: </b>", round(LingIso_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Impactos acumulativos:</b>",
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
        Sc_FLAG == "Sí" ~ "Cumulatively Impacted Community",
        TRUE ~ "Socioeconomically Vulnerable Community"
        # Sc_FLAG == "Yes" & DI_communityCount == 1 ~ "Cumulatively Impacted Community",
        # Sc_FLAG == "No" & DI_communityCount > 1 ~ "Socioeconomically Vulnerable Community"
        # TRUE ~ "Both"
        
      ))
    )%>%
    as('sf')
  return(diCommunity_AQCC)
  
}