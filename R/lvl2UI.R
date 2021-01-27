# lvl2UI.R
#
# Purpose: Create the user interface for the level 2 tab.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# ========================================================================================

lvl2UI <- function(){
  tabPanel(title = "Level2Checks",
           
           
           id = "Level2Checks",
           
           
           sidebarLayout(
             # Left side panel.
             sidebarPanel(width = 3, class = "input-section", useShinyjs(),
                          
                          tags$strong("Directory - Run: "),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                               
                              checkboxInput(inputId = "checkLvl2Required",
                                            label = "Required Files Check",
                                            value = TRUE),
                              
                              # Checkbox inputs to select standards to run.
                              checkboxInput(inputId = "checkLvl2DirNames",
                                            label = "Directory Names Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2FileNames",
                                            label = "File Names Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "compareLvl2FileNames", 
                                            label = "File Names Comparison",
                                            value = TRUE)
                          ),
                          
                          checkboxInput(inputId = "checkLvl2CompressedFiles",
                                        label = "Compressed Files Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("README - Run: "),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                              # Checkbox inputs to select standards to run.
                              checkboxInput(inputId = "checkLvl2Required1",
                                            label = "Required Files Check (Mandatory)",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2READMEColumnNames",
                                            label = "Column Names Check (Mandatory)",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "compareLvl2READMEFileNames",
                                            label = "README File Names Comparison",
                                            value = TRUE)
                          
                          ),
                          
                          checkboxInput(inputId = "checkLvl2READMEBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2READMECommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2READMENumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2READMEWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2READMEEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("DICT - Run: This only runs on level 2
                                      directories that contain a DICT file."),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                             
                                # Checkbox inputs to select standards to run.
                                checkboxInput(inputId = "checkLvl2Required2",
                                              label = "Required Files Check (Mandatory)",
                                              value = TRUE),
                                
                                checkboxInput(inputId = "checkLvl2DICTColumnNames",
                                              label = "Column Names Check (Mandatory)",
                                              value = TRUE),
                                
                                checkboxInput(inputId = "checkLvl2DICTColumnLabels",
                                              label = "Column Labels Check",
                                              value = TRUE),
                                
                                checkboxInput(inputId = "checkLvl2DICTDataTypes",
                                              label = "Data Types Check",
                                              value = TRUE)
                            ),
                          
                          checkboxInput(inputId = "checkLvl2DICTBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2DICTCommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2DICTNumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2DICTWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2DICTEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("DATA - Run: This only runs on level 2
                                      directories that contain a DATA file."),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                                  # Checkbox inputs to select standards to run.
                                  checkboxInput(inputId = "checkLvl2Required3",
                                                label = "Required Files Check (Mandatory)",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl2DATAColumnNames",
                                                label = "Column Names Check (Mandatory)",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl1DirName3",
                                                label = "Lvl 1 Directory Name Check (Mandatory)",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl2DATAColumnNameFormat",
                                                label = "Column Name Format Check",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl2DATAColumnNameSyntax",
                                                label = "Column Name Syntax Check",
                                                value = TRUE),
                                  
                                  conditionalPanel( condition = "input.participantFile == 'TRUE'",
                                                    
                                    checkboxInput(inputId = "verifyLvl2DATASubjectIDs",
                                                  label = "Subject IDs Verification",
                                                  value = TRUE),
                                    
                                    checkboxInput(inputId = "checkLvl2DATADateRange",
                                                  label = "Date Range Check",
                                                  value = TRUE)
                                  
                                  ),
                                  checkboxInput(inputId = "compareLvl2DATASubjectIDs",
                                                label = "Subject IDs Comparison",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl2DATATransferIDs",
                                                label = "Transfer IDs Check",
                                                value = TRUE),
                                  checkboxInput(inputId = "checkLvl2DATAMissingCodeRows",
                                                label = "Missing Code Rows Check",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl2DATAVisitCodes",
                                                label = "Visit Codes Check",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl2DATASiteCodes",
                                                label = "Site Codes Check",
                                                value = TRUE),
                                  
                                  checkboxInput(inputId = "checkLvl2DATAMissingCodes",
                                                label = "Missing Codes Check",
                                                value = TRUE)
                              ), 
                              
                              checkboxInput(inputId = "checkLvl2DATAPrecisionLevels",
                                            label = "Precision Levels Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2DATASpecialValues",
                                            label = "Special Values Check",
                                            value = TRUE),
                             
                              checkboxInput(inputId = "checkLvl2DATADateFormat",
                                            label = "Date Format Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2DATABlankCells",
                                            label = "Blank Cells Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2DATACommas",
                                            label = "Commas Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2DATANumOfCharacters",
                                            label = "Number of Characters Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2DATAWhiteSpaces",
                                            label = "White Spaces Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2DATAEncapsulation",
                                            label = "Encapsulation Check",
                                            value = TRUE),
                              
                              br(), br(),
                              
                              tags$strong("MISSING - Run: This only runs on level 2
                                          directories that contain a MISSING file."),
                              
                              conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                               
                              checkboxInput(inputId = "checkLvl2Required4",
                                            label = "Required Files Check (Mandatory)",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGColumnNames",
                                            label = "Column Names Check (Mandatory)",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl1DirName4",
                                            label = "Lvl 1 Directory Name Check (Mandatory)",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGColumnNameFormat",
                                            label = "Column Name Format Check",
                                            value = TRUE),
                              
                              conditionalPanel( condition = "input.participantFile == 'TRUE'",
                                                
                                checkboxInput(inputId = "verifyLvl2MISSINGSubjectIDs",
                                              label = "Subject IDs Verification",
                                              value = TRUE),
                                
                                checkboxInput(inputId = "checkLvl2MISSINGDateRange",
                                              label = "Date Range Check",
                                              value = TRUE)
                              ),
                              checkboxInput(inputId = "compareLvl2MISSINGSubjectIDs",
                                            label = "Subject IDs Comparison",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGTransferIDs",
                                            label = "Transfer IDs Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "compareLvl2ColumnNames",
                                            label = "Column Names Comparison",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGMissingColumn",
                                            label = "MISSING_CODE Column Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGVisitCodes",
                                            label = "Visit Codes Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGSiteCodes",
                                            label = "Site Codes Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGDateFormat",
                                            label = "Date Format Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl2MISSINGMissingCodes",
                                            label = "Missing Codes Check",
                                            value = TRUE)
                              
                          ),
                          
                          checkboxInput(inputId = "checkLvl2MISSINGBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2MISSINGCommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2MISSINGNumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2MISSINGWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl2MISSINGEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(),
                          
                          # Buttons to select or deselect all checkboxes.
                          actionButton(inputId = "lvl2SelectButton", 
                                       label = "Select All",
                                       class = "select-button"),
                          
                          actionButton(inputId = "lvl2DeselectButton", 
                                       label = "Deselect All",
                                       class = "deselect-button")
             ),
             
             # Right side panel.
             mainPanel(width = 9,
                       column(width = 8, 
                              # Static progress bar.
                              fluidRow(br(), progressBar(id = "lvl2ProgressBar", 
                                                         value = 0,
                                                         display_pct = T)
                              )
                       ),
                       
                       column(width = 4,
                              # Dropdown to select algorithm.
                              selectInput(inputId = "lvl2AlgoSelection", label = "", 
                                          choices = "")
                       ),
                       
                       column(width = 8,
                              # Output box for flagged messages.
                              fluidRow(class = "output-section",
                                       tags$div(class = "output-text", 
                                                htmlOutput(outputId = "lvl2OutputText"))
                              )
                       ),
                       
                       column(width = 4,
                              # Output box for IDs.
                              fluidRow(class = "output-section",
                                       tags$div(class = "output-text", 
                                                htmlOutput(outputId = "lvl2OutputID"))
                              )
                       ),
                       
                       column(width = 12,
                              # Buttons below output box.
                              fluidRow(
                                actionButton(inputId = "lvl2CheckDirButton", 
                                             label = "Check Directory",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl2CheckREADMEButton", 
                                             label = "Check README",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl2CheckDICTButton", 
                                             label = "Check DICT",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl2CheckDATAButton", 
                                             label = "Check DATA",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl2CheckMISSINGButton", 
                                             label = "Check MISSING",
                                             class = "check-button")
                              )
                       )
             )
           )
  )
}

# [END]