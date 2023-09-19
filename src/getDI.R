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
getDI <- function(){
  
  diCommunity <- readRDS("data/scores/diCommunities.rds")%>%
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
      Sc_FLAG = case_when(
        Score_FLAG == 1 ~ "Sí",
        Score_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><strong>Comunidad afectada de manera desproporcionada: </strong>",
               "<br/><b>Grupo de manzanas censales: </b>", GEOID,
               "<br/>",
               "<br/><b>Más del 40 % de los hogares son de bajos ingresos: </b>", FLP_FLA,
               "<br/><b>Porcentaje de bajos ingresos: </b>", round(Pov_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 40 % de los hogares están integrados por personas de color : </b>", Mn_FLAG,
               "<br/><b>Porcentaje de personas de color: </b>", round(Min_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Más del 40 % de los hogares experimentan sobrecarga por gastos de vivienda : </b>", Br_FLAG,
               "<br/><b>Porcentaje con sobrecarga por gastos de vivienda: </b>", round(HH_Burdened_Pct*100, digits = 1),
               "<br/>",
               "<br/><b>El puntaje de EnviroScreen (percentil) es mayor que 80: </b>", Sc_FLAG,
               "<br/><b>Puntaje de EnviroScreen (percentil): </b>", round(EnviroScreen_Pctl, digits = 1),
               "<br/>",
               "<br/>",
               "Lea la definición de comunidad afectada de manera desproporcionada de Colorado en la " 
               ,tags$a(href = "https://cdphe.colorado.gov/environmental-justice", " Ley de Justicia Ambiental.", target = "_blank")
             )
    )%>%
    mutate(
      color = as.factor(case_when(
        Mn_FLAG == "Sí" & FLP_FLA == "No" & Br_FLAG == "No" & Sc_FLAG == "No" ~ "People of Color",
        Mn_FLAG == "No" & FLP_FLA == "Sí" & Br_FLAG == "No" & Sc_FLAG == "No" ~ "Low Income",
        Mn_FLAG == "No" & FLP_FLA == "No" & Br_FLAG == "Sí" & Sc_FLAG == "No" ~ "Housing Burden",
        Mn_FLAG == "No" & FLP_FLA == "No" & Br_FLAG == "No" & Sc_FLAG == "Sí" ~ "EnviroScreen Score",
        TRUE ~ "Más de una categoría"
      ))
    )%>%
    as('sf')
  
  return(diCommunity)
  
}