# TabularUI.R
#
# Purpose: Create the user interface for the Tabular data tab.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
# Modified by: Roberto Letini (rlenini@research.baycrest.org)
#
# Date: 2019-06-19
# Modified: 2020-06-12
# ========================================================================================

# code to reset App
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

# code that will allow tab to be disabled
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

TabularUI <- function(){
  tabPanel(title = "TabularChecks",
           
           id = "TabularChecks",
           
           #used to call the js code and css code that will be able to disable the tabs
           useShinyjs(),
           extendShinyjs(text = jsCode, functions = c("disableTab", "enableTab")),
           inlineCSS(css),
           
           # Generate the log file
           shinySaveButton("save", 
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
           
           
           # Go back to File select Button
           extendShinyjs(text = jsResetCode, functions = c("reset")),
           
           actionButton(label = "Back To Select", 
                        inputId = "Back",
                        style = "color: white; 
                                background-color: #EA526F;
                                float: right;
                                margin: 10px;
                                height: 50px;
                                font-weight: 400px;
                                font-size: 16px;
                                
                        "),
           
           
           sidebarLayout(sidebarPanel(width = 3, class = "input-section",
                         
                         tags$strong('File - Run:'),
                         
                        conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                           checkboxInput(inputId = "checkTabularRequired",
                                         label = "Required Files Check",
                                         value = TRUE),
                           
                           # Checkbox inputs to select standards to run.
                           checkboxInput(inputId = "checkTabularDirName",
                                         label = "Directory Name Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularFileNames",
                                         label = "File Names Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "compareTabularFileNames", 
                                         label = "File Names Comparison",
                                         value = TRUE)
                        ), 
                        
                         checkboxInput(inputId = "checkTabularFileExt", 
                                       label = "File Extensions Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularCompressedFiles",
                                       label = "Compressed Files Check",
                                       value = TRUE),
                         
                         br(), br(),
                         tags$strong("README - Run: "),
                        
                        conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                         
                           # Checkbox inputs to select standards to run.
                           checkboxInput(inputId = "checkTabularRequired1",
                                         label = "Required Files Check (Mandatory)",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularREADMEColumnNames",
                                         label = "Column Names Check (Mandatory)",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "compareTabularREADMEFileNames",
                                         label = "README File Names Comparison",
                                         value = TRUE)
                        ),
                         

                           checkboxInput(inputId = "checkTabularREADMEBlankCells",
                                         label = "Blank Cells Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularREADMECommas",
                                         label = "Commas Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularREADMENumOfCharacters",
                                         label = "Number of Characters Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularREADMEWhiteSpaces",
                                         label = "White Spaces Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularREADMEEncapsulation",
                                         label = "Encapsulation Check",
                                         value = TRUE),
                         
                         br(), br(),
                         
                         tags$strong("DICT - Run: "),
                         
                        conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                         
                         # Checkbox inputs to select standards to run.
                         checkboxInput(inputId = "checkTabularRequired2",
                                       label = "Required Files Check (Mandatory)",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDICTColumnNames",
                                       label = "Column Names Check (Mandatory)",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDICTColumnLabels",
                                       label = "Column Labels Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDICTDataTypes",
                                       label = "Data Types Check",
                                       value = TRUE)
                        ),
                        
                         checkboxInput(inputId = "checkTabularDICTBlankCells",
                                       label = "Blank Cells Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDICTCommas",
                                       label = "Commas Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDICTNumOfCharacters",
                                       label = "Number of Characters Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDICTWhiteSpaces",
                                       label = "White Spaces Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDICTEncapsulation",
                                       label = "Encapsulation Check",
                                       value = TRUE),
                         
                         br(), br(),
                         
                         tags$strong("DATA - Run: "),
                         
                        conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                         
                           # Checkbox inputs to select standards to run.
                           checkboxInput(inputId = "checkTabularRequired3",
                                         label = "Required Files Check (Mandatory)",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATAColumnNames",
                                         label = "Column Names Check (Mandatory)",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDirName1",
                                         label = "Lvl 1 Directory Name Check (Mandatory)",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATAColumnNameFormat",
                                         label = "Column Name Format Check",
                                         value = TRUE),
                           
                           conditionalPanel(condition = "input.participantFile == 'TRUE'",
                                             
                             checkboxInput(inputId = "verifyTabularDATASubjectIDs",
                                           label = "Subject IDs Verification",
                                           value = TRUE)
                           ),
                           
                           checkboxInput(inputId = "compareTabularDATASubjectIDs",
                                         label = "Subject IDs Comparison",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATATransferIDs",
                                         label = "Transfer IDs Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATAMissingCodeRows",
                                         label = "Missing Code Rows Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATAVisitCodes",
                                         label = "Visit Codes Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATASiteCodes",
                                         label = "Site Codes Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATADateRange",
                                         label = "Date Range Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDATAMissingCodes",
                                         label = "Missing Codes Check",
                                         value = TRUE)
                        ),
                        
                         checkboxInput(inputId = "checkTabularDATADateFormat",
                                       label = "Date Format Check",
                                       value = TRUE),
                        
                         checkboxInput(inputId = "checkTabularDATAColumnNameSyntax",
                                       label = "Column Name Syntax Check",
                                       value = TRUE),
                         
                         
                         checkboxInput(inputId = "checkTabularDATAPrecisionLevels",
                                       label = "Precision Levels Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDATASpecialValues",
                                       label = "Special Values Check",
                                       value = TRUE),
                         
                         
                         checkboxInput(inputId = "checkTabularDATABlankCells",
                                       label = "Blank Cells Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDATACommas",
                                       label = "Commas Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDATANumOfCharacters",
                                       label = "Number of Characters Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDATAWhiteSpaces",
                                       label = "White Spaces Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularDATAEncapsulation",
                                       label = "Encapsulation Check",
                                       value = TRUE),
                         
                         br(), br(),
                         
                         tags$strong("MISSING - Run: "),
                         
                        conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                         
                           checkboxInput(inputId = "checkTabularRequired4",
                                         label = "Required Files Check (Mandatory)",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGColumnNames",
                                         label = "Column Names Check (Mandatory)",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularDirName2",
                                         label = "Lvl 1 Directory Name Check (Mandatory)",
                                         value = TRUE),
                           
                           conditionalPanel( condition = "input.participantFile == 'TRUE'",
                                             
                             checkboxInput(inputId = "checkTabularMISSINGColumnNameFormat",
                                           label = "Column Name Format Check",
                                           value = TRUE)
                           ),
                           
                           checkboxInput(inputId = "verifyTabularMISSINGSubjectIDs",
                                         label = "Subject IDs Verification",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "compareTabularMISSINGSubjectIDs",
                                         label = "Subject IDs Comparison",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGTransferIDs",
                                         label = "Transfer IDs Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "compareTabularColumnNames",
                                         label = "Column Names Comparison",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGMissingColumn",
                                         label = "MISSING_CODE Column Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGVisitCodes",
                                         label = "Visit Codes Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGSiteCodes",
                                         label = "Site Codes Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGDateFormat",
                                         label = "Date Format Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGDateRange",
                                         label = "Date Range Check",
                                         value = TRUE),
                           
                           checkboxInput(inputId = "checkTabularMISSINGMissingCodes",
                                         label = "Missing Codes Check",
                                         value = TRUE)
                           
                        ),
                        
                         checkboxInput(inputId = "checkTabularMISSINGBlankCells",
                                       label = "Blank Cells Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularMISSINGCommas",
                                       label = "Commas Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularMISSINGNumOfCharacters",
                                       label = "Number of Characters Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularMISSINGWhiteSpaces",
                                       label = "White Spaces Check",
                                       value = TRUE),
                         
                         checkboxInput(inputId = "checkTabularMISSINGEncapsulation",
                                       label = "Encapsulation Check",
                                       value = TRUE),
                         
                         br(),
                         
                         # Buttons to select or deselect all checkboxes.
                         actionButton(inputId = "tabularSelectButton", 
                                      label = "Select All",
                                      class = "select-button"),
                         
                         actionButton(inputId = "tabularDeselectButton", 
                                      label = "Deselect All",
                                      class = "deselect-button")
                                  ),
                         
                         #Right Side panel
                         mainPanel(width = 9,
                                   column(width = 8, 
                                          # Static progress bar.
                                          fluidRow(br(), progressBar(id = "tabularProgressBar", 
                                                                     value = 0,
                                                                     display_pct = T)
                                          )
                                   ),
                                   
                                   column(width = 4,
                                          # Dropdown to select algorithm.
                                          selectInput(inputId = "lvl1AlgoSelection", label = "", 
                                                      choices = "")
                                   ),
                                   
                                   
                                   column(width = 8,
                                          # Dropdown to select algorithm.
                                          fluidRow(class = "output-section",
                                                   tags$div(class = "output-text",
                                                            htmlOutput(outputId = "tabularOutputText")))
                                   ),
                                   
                                   column(width = 4,
                                          #Output box for IDs
                                          fluidRow(class = "output-section",
                                                   tags$div(class = "output-text",
                                                            htmlOutput(outputId = "tabularOutputID")))
                                          ),
                                   
                                   column(width = 12,
                                          # Buttons below output box.
                                          fluidRow(
                          
                                            actionButton(inputId = "tabularCheckDirButton", 
                                                         label = "Check Directory",
                                                         class = "check-button"),
                                            
                                            actionButton(inputId = "tabularCheckREADMEButton", 
                                                         label = "Check README",
                                                         class = "check-button"),
                                            
                                            actionButton(inputId = "tabularCheckDICTButton", 
                                                         label = "Check DICT",
                                                         class = "check-button"),
                                            
                                            actionButton(inputId = "tabularCheckDATAButton", 
                                                         label = "Check DATA",
                                                         class = "check-button"),
                                            
                                            actionButton(inputId = "tabularCheckMISSINGButton", 
                                                         label = "Check MISSING",
                                                         class = "check-button")
                                          )
                                   )
                                   
                                          )
                                   
                                   
                                 
           ))
}

#[END]