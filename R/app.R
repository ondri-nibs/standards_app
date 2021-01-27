#' @title runApp
#'
#' @description A function for running the ONDRI Standards ShinyApp.
#' @author Roberto Lentini Jedid Ahn
#' 
#' @import shiny shinyFiles shinyWidgets shinyjs shinyalert
#' @import tools methods utils xml2 V8 stringr dplyr lubridate varhandle readr rowr
#' @export
#' 
runApp <- function(){
  shiny::addResourcePath("www", system.file("www", package = "ONDRIStandardsApp"))
  
  # Create the user interface (front end).
  ui <- fluidPage(theme = "www/style.css",
                  
                  # Create title with logo.
                  titlePanel(title = tags$div(
                    img(class = "ondri-logo", src = "www/ONDRI_full-logo_web.png"), 
                    "ONDRI Standards Application 0.9.0.9002"),
                    windowTitle = "ONDRI Standards Application"),
                  
                  # Create UI consisting of 4 tabs.
                  tabsetPanel(selectUI(), TabularUI(), NonTabularUI(), id = "tabs")
  )
  
  # Define server logic (back end).
  server <- function(input, output, session) {
    # Erasing previous saved data if any.
    saveLog(NULL)
    
    #Hide the level 1 checks tab on load
    hideTab(inputId =  "tabs",target = "NonTabularChecks")
    hideTab(inputId = "tabs", target = "TabularChecks")
    
    # Back end code for select directory tab.
    selectServer(input, output, session)
    
    # Back end code for non tabular checks
    nontabularServer(input, output, session)
    
    # Back end code for tabular checks
    tabularServer(input, output, session)
    updateTabularCheckbox(input, session)
    
    # Back end code for lvl 1 tab.
    lvl1Server(input, output, session)
    updateLvl1Checkbox(input, session)
    
    # Back end code for lvl 2 tab.
    lvl2Server(input, output, session)
    updateLvl2Checkbox(input, session)
    
    # Back end code for lvl 3 tab.
    lvl3Server(input, output, session)
    updateLvl3Checkbox(input, session)
    
  }
  
  # Run the application.
  shinyApp(ui, server, options = list(display.mode = "normal", launch.browser = TRUE))
}

# [END]