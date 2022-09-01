

defineLegend<- function(indicator){
    #establish classes 
    cat1 <- c("Poblaciones sensibles"
              ,"Población por debajo de 5 años"
              ,"Población por encima de 64 años"
              ,"Enfermedades cardiacas en adultos"
              ,"Tasa de hospitalización por asma"
              ,"Expectativa de vida"
              ,"Bajo peso al nacer")
              
    cat2 <- c("Características demográficas"
              ,"Porcentaje de personas de color"
              ,"Porcentaje que no completaron los estudios de secundaria"
              ,"Porcentaje de bajos ingresos"
              ,"Percentil del porcentaje de aislamiento lingüístico"
              ,"Porcentaje de discapacidades")
    
    # conditional statement to assign label value. 
    t <- ifelse(test = indicator %in% cat1,
           yes = labels <- c("Más susceptible", "", "", "", "Menos susceptible"),
           no = ifelse(
             test = indicator %in% cat2,
             yes = labels <- c("Más vulnerable", "", "", "", "Menos vulnerable"),
             no = labels <- c("Mayor carga", "", "", "", "Menos carga")
           )
          )
  
    return(labels)
}