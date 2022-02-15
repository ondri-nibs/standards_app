# TabularUI.R
#
# Purpose: Create the user interface for the Tabular data.
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-06-09
#
# ========================================================================================

# code to reset App
jsResetCode <- "shinyjs.resetTab = function() {history.go(0)}" # Define the js method that resets the page

jsCode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"


NonTabularUI <- function(){
  tabPanel(title = "NonTabularChecks",
           
           id = "NonTabularChecks",
           
           # Generate the log file
           shinySaveButton("save_NonTab", 
                           "Generate Log File", 
                           "Save file as ...", 
                           filetype=list(xml="xml"),
                           style = "color: white;
                                background-color: #5BC0DE;
                                float: right;
                                margin: 10px;
                                height: 50px;
                                font-weight: 400px;
                                font-size: 16px;"),
           
           #used to call the js code and css code that will be able to disable the tabs
           useShinyjs(),
           extendShinyjs(text = jsCode, functions = c("disableTab", "enableTab")),
           inlineCSS(css),
           
           # Go back to File select Button
           extendShinyjs(text = jsResetCode, functions = c("resetTab")),
           actionButton(label = "Back To Select", 
                        inputId = "Back2",
                        style = "color: white; 
                                background-color: #EA526F;
                                float: right;
                                margin: 10px;
                                height: 50px;
                                font-weight: 400px;
                                font-size: 16px;
                                
                        "),
           
           tabsetPanel(lvl1UI(),  lvl2UI(), lvl3UI())
           
           )
}



#[END]