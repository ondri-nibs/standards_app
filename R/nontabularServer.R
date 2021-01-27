# nontabularServer.R
#
# Purpose: Create the server logic for the non-tabular tab for tabular structure checks.
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-06-12
# ==========================================================================================

nontabularServer <- function(input, output, session){
  
  # Button to bring user back to the select form page
  observeEvent(input$Back2, {
    {js$reset()}
  })
  
  # Save the log file
  observe({
    # Computer volumes depend on OS: Windows, Mac, or Linux.
    volumes <- c(Home = fs::path_home(), getVolumes()())
    
    
    shinyFileSave(input, "save_NonTab", roots=volumes)
    fileinfo <- parseSavePath(volumes, input$save_NonTab)
    if (nrow(fileinfo) > 0) {
      path <- as.character(fileinfo$datapath)
      create_log_file(path)
    }
    
  })
}
#[END]