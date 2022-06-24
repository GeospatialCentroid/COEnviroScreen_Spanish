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

# Add categories and specific visualization parameters 
# Will need to add custom legend as well
# low income : rgb(253,209,138)   #fdd18a
# 
# people of color : id="rgb(173,225,233)"   #ade1df
# 
# housing burden : rgb(81,161,152) #51a198
# 
# more then one categoy :rgb(192,149,180) #c095b4
# 
getDI <- function(){
  
  diCommunity <- readRDS("data/scores/diCommunities.rds")%>%
    mutate(
      Mn_FLAG = case_when(
        Min_FLAG == 1 ~ "Yes",
        Min_FLAG == 0 ~ "No"
      ),
      FLP_FLA = case_when(
        FLP_FLAG == 1 ~ "Yes",
        FLP_FLAG == 0 ~ "No"
      ),
      Br_FLAG = case_when(
        Burdened_FLAG == 1 ~ "Yes",
        Burdened_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><strong>Comunidad afectada de manera desproporcionada: </strong>",
               "<br/><b>Grupo de manzanas censales: </b>", GEOID,
               "<br/>",
               "<br/><b>40 % de las viviendas son de bajos ingresos: </b>", FLP_FLA,
               "<br/><b>Porcentaje de bajos ingresos: </b>", round(Pov_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>40 % de las viviendas están integradas por personas de color : </b>", Mn_FLAG,
               "<br/><b>Porcentaje de personas de color: </b>", round(Min_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>40 % de las viviendas con sobrecarga por gastos de vivienda : </b>", Br_FLAG,
               "<br/><b>Porcentaje con sobrecarga por gastos de vivienda: </b>", round(HH_Burdened_Pct*100, digits = 1)
               ,"<br/>"
               ,"<br/>"
               ,"Lea la definición de comunidad afectada de manera desproporcionada de Colorado en la " 
               ,tags$a(href = "https://cdphe.colorado.gov/environmental-justice", " Ley de Justicia Ambiental.", target = "_blank")
             )
          )%>%
    mutate(
      color = as.factor(case_when(
        Mn_FLAG == "Yes" & FLP_FLA == "No" & Br_FLAG == "No" ~ "People of Color",
        Mn_FLAG == "No" & FLP_FLA == "Yes" & Br_FLAG == "No" ~ "Low Income",
        Mn_FLAG == "No" & FLP_FLA == "No" & Br_FLAG == "Yes" ~ "Housing Burden",
        TRUE ~ "Más de una categoría"
      ))
    )%>%
    as('sf')
  return(diCommunity)
}