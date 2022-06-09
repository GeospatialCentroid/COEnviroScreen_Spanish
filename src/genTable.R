
genTable <- function(tableData, geoid, colSelected){ 
  # primary table. 
  
  
  table1 <- tableData %>% sf::st_drop_geometry()
  # sort table if geoid has been selected 
  if(geoid[1] %in% table1$GEOID){
    #sort table by GEOID 
    table1 <- setorder(x = table1, GEOID, na.last= TRUE)
    
    feature <- grep(pattern = geoid[1], x = table1$GEOID)
    # order based on selected values 
    order2 <- c(feature:nrow(table1), 1:(feature-1))
    
    table1 <- table1[order2, ]
  }
  
  #select columns based on input
  
  if(colSelected == "Puntaje de los componentes en conjunto"){
    table2 <-  table1 %>% 
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Percentil del puntaje de Colorado EnviroScreen"
        ,"Puntaje de Colorado EnviroScreen"
        # ,"Percentil de contaminación y carga climática"
        # ,"Contaminación y carga climática"
        ,"Percentil de factores de salud y sociales"
        ,"Factores de salud y sociales"
      )
  } else if(colSelected == "Puntaje de los componentes") {
    table2 <-  table1 %>% 
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Percentil de exposiciones ambientales"
        ,"Exposiciones ambientales"
        ,"Percentil de efectos ambientales"
        ,"Efectos ambientales"
        ,"Percentil de vulnerabilidad climática"
        ,"Vulnerabilidad climática"
        ,"Percentil de poblaciones sensibles"
        ,"Poblaciones sensibles"
        ,"Percentil de características demográficas"
        ,"Características demográficas"
      )
  } else if(colSelected == "Exposiciones ambientales") {
    table2 <-  table1 %>% 
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Percentil de emisiones de contaminantes tóxicos del aire"
        ,"Emisiones de contaminantes tóxicos del aire"
        ,"Percentil de material particulado (PM) de diésel"
        ,"Material particulado (PM) de diésel"
        ,"Percentil de reglamentos sobre agua potable"
        ,"Reglamentos sobre agua potable"
        ,"Percentil de riesgo de exposición al plomo"
        ,"Riesgo de exposición al plomo"
        ,"Percentil de ruido"
        ,"Ruido"
        ,"Percentil de otros contaminantes del aire"
        ,"Otros contaminantes del aire"
        ,"Percentil de ozono"
        ,"Ozono"
        ,"Percentil de contaminación por partículas finas"
        ,"Contaminación por partículas finas"
        ,"Percentil de proximidad y volumen de tránsito"
        ,"Proximidad y volumen de tránsito"
      )
  } else if(colSelected == "Efectos ambientales") {
    table2 <- table1 %>% 
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Percentil de arroyos y ríos deteriorados"
        ,"Arroyos y ríos deteriorados"
        ,"Percentil de proximidad a instalaciones de residuos peligrosos"
        ,"Proximidad a instalaciones de residuos peligrosos"
        ,"Percentil de proximidad a ubicaciones de minería"
        ,"Proximidad a ubicaciones de minería"
        ,"Percentil de proximidad a los sitios de la Lista Nacional de Prioridades"
        ,"Proximidad a los sitios de la Lista Nacional de Prioridades"
        ,"Percentil de proximidad a petróleo y gas"
        ,"Proximidad a petróleo y gas"
        ,"Percentil de proximidad a los sitios del Plan de Gestión de Riesgos"
        ,"Proximidad a los sitios del Plan de Gestión de Riesgos"
        ,"Percentil del indicador de descargas de aguas residuales"
        ,"Indicador de descargas de aguas residuales"
      )
  }  else if(colSelected == "Vulnerabilidad climática") {
    table2 <- table1 %>% 
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Percentil de sequía"
        ,"Sequía"
        ,"Percentil de días de calor extremo"
        ,"Días de calor extremo"
        ,"Percentil de llanuras aluviales"
        ,"Llanuras aluviales"
        ,"Percentil de riesgo de incendios forestales"
        ,"Riesgo de incendios forestales"
      )
  } else if(colSelected == "Poblaciones sensibles") {
    table2 <- table1 %>% 
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Percentil de tasa de hospitalización por asma"
        ,"Tasa de hospitalización por asma"
        ,"Percentil de predominio de cáncer"
        ,"Predominio de cáncer"
        ,"Percentil de predominio de diabetes"
        ,"Predominio de diabetes"
        ,"Percentil de cardiopatías en adultos"
        ,"Cardiopatías en adultos"
        ,"Percentil de expectativa de vida"
        ,"Expectativa de vida"
        ,"Percentil de bajo peso al nacer"
        ,"Bajo peso al nacer"
        ,"Percentil del indicador de salud mental"
        ,"Indicador de salud mental"
        ,"Percentil de población por encima de 64 años"
        ,"Población por encima de 64 años"
        ,"Percentil de población por debajo de 5 años"
        ,"Población por debajo de 5 años"
      )
  } else if(colSelected == "Características demográficas") {
    table2 <- table1 %>%  
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Percentil de sobrecarga por gastos de vivienda"
        ,"Sobrecarga por gastos de vivienda"
        ,"Percentil del porcentaje de discapacidades"
        ,"Porcentaje de discapacidades"
        ,"Percentil del porcentaje que no completaron los estudios de secundaria"
        ,"Porcentaje que no completaron los estudios de secundaria"
        ,"Percentil del porcentaje de aislamiento lingüístico"
        ,"Porcentaje de aislamiento lingüístico"
        ,"Percentil del porcentaje de bajos ingresos"
        ,"Porcentaje de bajos ingresos"
        ,"Percentil del porcentaje de personas de color"
        ,"Porcentaje de personas de color"
      )
  }  else if(colSelected == "Clasificación de las comunidades") {
    table2 <- table1 %>%  
      select(
        "GEOID"
        ,"Nombre del condado"
        ,"Comunidad afectada de manera desproporcionada"
        ,"Comunidad de Justice40"
        ,"Comunidad con carbón"
        ,"Comunidad con petróleo y gas"
        ,"Comunidad rural"
        ,"Total de la población"
      )
  }
  
  
  return(table2)
}


