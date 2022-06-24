#text for the title section 
# group all non reactive text elements by fluid rows.Largerly for organization. 

title1 <- function(){
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
    column(8, h1("Puntaje de Colorado EnviroScreen"), p("Junio de 2022"))
  )
}

title2 <- function(){
  fluidRow(
    p(HTML("</br><a href='#map'>Saltar al mapa</a>")),
    p("Colorado EnviroScreen es un mapa interactivo de justicia ambiental y una herramienta de evaluación de la salud en Colorado. Un equipo de Colorado State University creó esta herramienta para el Departamento de Salud Pública y Medio Ambiente de Colorado (CDPHE). Se lanzó la versión 1.0 de Colorado EnviroScreen el XX de junio de 2022. Para obtener más información sobre Colorado EnviroScreen, diríjase a la ",
        tags$a(href = "https://cdphe.colorado.gov/enviroscreen", "página web de Colorado EnviroScreen ", target = "_blank"),
      "del CDPHE. Puede enviar sus comentarios sobre Colorado EnviroScreen por correo electrónico al CDPHE: cdphe_ej@state.co.us"
    )
  ) 
}


title3 <- function(){
  fluidRow(
    p("Colorado EnviroScreen:"),
    p(
      tags$ul(
        tags$li("identifica las áreas en las que hay y ha habido inequidades ambientales;"),
        tags$li("detalla las áreas en las que la carga para la salud y/o los riesgos ambientales son mayores para las comunidades afectadas de manera desproporcionada;"),
        tags$li("identifica la ubicación geográfica de las comunidades afectadas de manera desproporcionada según la definición de la Ley de Justicia Ambiental (Ley 21-1266 de la Cámara de Representantes).")
      )
    )
  )
}
  
title4 <- function(){
  fluidRow(
    p("El propósito de Colorado EnviroScreen es:"),
    p(
      tags$ul(
        tags$li("ayudar a los usuarios a que intercedan para recibir fondos, intervenciones y cambios en las políticas con el fin de evitar, disminuir y mitigar los riesgos para la salud ambiental;"),
        tags$li("fomentar un estilo de vida saludable y sostenible en Colorado y que todas las personas reciban el mismo grado de protección de los peligros ambientales y para la salud.")
      )
    )
  )
}

title5 <- function(){
  fluidRow(
    p("Haga clic aquí para obtener más información sobre el trabajo que lleva a cabo el CDPHE con el fin de ",
      tags$a(href = "https://cdphe.colorado.gov/environmental-justice", "fomentar la justicia ambiental", target = "_blank"),
      ", ",
      tags$a(href = "https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology", "comprender la relación que existe entre el medio ambiente y nuestra salud", target = "_blank"),
      " , and ",
      tags$a(href = "https://cdphe.colorado.gov/air-pollution/climate-change#equity", "fomentar la equidad climática.", target = "_blank"),
    ))
}

title6 <- function(){
  fluidRow(
    p("Desplácese hacia abajo para obtener más información acerca de cómo usar Colorado EnviroScreen.")
  )
}

