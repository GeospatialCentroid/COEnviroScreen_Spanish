options(rsconnect.locale.cache = FALSE, 
        rsconnect.locale = "es_ES.UTF-8") 

rsconnect::deployApp(  
                     appFiles = "app.R",
                     account = "geocentroid", 
                     server = "shinyapps.io",     
                     appName = "EnviroScreen_Shiny_Spanish")


