options(rsconnect.locale.cache = FALSE, 
        rsconnect.locale = "es_ES.UTF-8") 

rsconnect::deployApp(  
                     appFiles = "app.R",
                     account = "teeo-cdphe", 
                     server = "shinyapps.io",     
                     appName = "COEnviroScreen_Spanish")


