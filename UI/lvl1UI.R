# lvl1UI.R
#
# Purpose: Create the user interface for the level 1 tab.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-06-19
#
# ========================================================================================

lvl1UI <- function(){
  tabPanel(title = "Level1Checks",
           
           id = "Level1Checks",
 
           sidebarLayout(
             # Left side panel.
             sidebarPanel(width = 3, class = "input-section", useShinyjs(),
                          
                          tags$strong("Directory - Run: "),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                            checkboxInput(inputId = "checkLvl1Required",
                                          label = "Required Files Check",
                                          value = TRUE),
                            
                            # Checkbox inputs to select standards to run.
                            checkboxInput(inputId = "checkLvl1DirName",
                                          label = "Directory Name Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1FileNames",
                                          label = "File Names Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "compareLvl1FileNames", 
                                          label = "File Names Comparison",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1FileExt", 
                                          label = "File Extensions Check",
                                          value = TRUE),
                          ),
                          
                          checkboxInput(inputId = "checkLvl1CompressedFiles",
                                        label = "Compressed Files Check",
                                        value = TRUE),
                        
                          checkboxInput(inputId = "checkLvl1DuplicateFileNames",
                                        label = "Duplicate File Names Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("README - Run: "),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                            # Checkbox inputs to select standards to run.
                            checkboxInput(inputId = "checkLvl1Required1",
                                          label = "Required Files Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1READMEColumnNames",
                                          label = "Column Names Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "compareLvl1READMEFileNames",
                                          label = "README File Names Comparison",
                                          value = TRUE),
                          
                          ),
                          
                          checkboxInput(inputId = "checkLvl1READMEBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1READMECommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1READMENumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1READMEWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1READMEEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("DICT - Run: This only runs on level 1
                                      directories that contain a DICT file."),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                            # Checkbox inputs to select standards to run.
                            checkboxInput(inputId = "checkLvl1Required2",
                                          label = "Required Files Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1DICTColumnNames",
                                          label = "Column Names Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1DICTColumnLabels",
                                          label = "Column Labels Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1DICTDataTypes",
                                          label = "Data Types Check",
                                          value = TRUE),
                          
                          ),
                          
                          checkboxInput(inputId = "checkLvl1DICTBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DICTCommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DICTNumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DICTWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DICTEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("DATA - Run: This only runs on level 1
                                      directories that contain a DATA file."),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                          # Checkbox inputs to select standards to run.
                          checkboxInput(inputId = "checkLvl1Required3",
                                        label = "Required Files Check (Mandatory)",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAColumnNames",
                                        label = "Column Names Check (Mandatory)",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DirName1",
                                        label = "Lvl 1 Directory Name Check (Mandatory)",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAColumnNameFormat",
                                        label = "Column Name Format Check",
                                        value = TRUE),
                          
                          conditionalPanel( condition = "input.participantFile == 'TRUE'",
                                            
                            checkboxInput(inputId = "verifyLvl1DATASubjectIDs",
                                          label = "Subject IDs Verification",
                                          value = TRUE),
                          ),
                          checkboxInput(inputId = "compareLvl1DATASubjectIDs",
                                        label = "Subject IDs Comparison",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATATransferIDs",
                                        label = "Transfer IDs Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAMissingCodeRows",
                                        label = "Missing Code Rows Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAVisitCodes",
                                        label = "Visit Codes Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATASiteCodes",
                                        label = "Site Codes Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATADateRange",
                                        label = "Date Range Check",
                                        value = TRUE),
                          
                          
                          checkboxInput(inputId = "checkLvl1DATAMissingCodes",
                                        label = "Missing Codes Check",
                                        value = TRUE),
                          
                          ),
                          
                          
                          checkboxInput(inputId = "checkLvl1DATADateFormat",
                                        label = "Date Format Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAPrecisionLevels",
                                        label = "Precision Levels Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATASpecialValues",
                                        label = "Special Values Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAColumnNameSyntax",
                                       label = "Column Name Syntax Check",
                                       value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATABlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATACommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATANumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1DATAEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("MISSING - Run: This only runs on level 1
                                      directories that contain a MISSING file."),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                            checkboxInput(inputId = "checkLvl1Required4",
                                          label = "Required Files Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGColumnNames",
                                          label = "Column Names Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1DirName2",
                                          label = "Lvl 1 Directory Name Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGColumnNameFormat",
                                          label = "Column Name Format Check",
                                          value = TRUE),
                            
                            conditionalPanel( condition = "input.participantFile == 'TRUE'",
                                              
                              checkboxInput(inputId = "verifyLvl1MISSINGSubjectIDs",
                                            label = "Subject IDs Verification",
                                            value = TRUE),
                            ),
                            
                            checkboxInput(inputId = "compareLvl1MISSINGSubjectIDs",
                                          label = "Subject IDs Comparison",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGTransferIDs",
                                          label = "Transfer IDs Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "compareLvl1ColumnNames",
                                          label = "Column Names Comparison",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGMissingColumn",
                                          label = "MISSING_CODE Column Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGVisitCodes",
                                          label = "Visit Codes Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGSiteCodes",
                                          label = "Site Codes Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGDateFormat",
                                          label = "Date Format Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGDateRange",
                                          label = "Date Range Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1MISSINGMissingCodes",
                                          label = "Missing Codes Check",
                                          value = TRUE),
                          ),
                          
                          checkboxInput(inputId = "checkLvl1MISSINGBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1MISSINGCommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1MISSINGNumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1MISSINGWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl1MISSINGEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(),
                          
                          # Buttons to select or deselect all checkboxes.
                          actionButton(inputId = "lvl1SelectButton", 
                                       label = "Select All",
                                       class = "select-button"),
                          
                          actionButton(inputId = "lvl1DeselectButton", 
                                       label = "Deselect All",
                                       class = "deselect-button")
                          ),
             
             # Right side panel.
             mainPanel(width = 9,
                       column(width = 8, 
                              # Static progress bar.
                              fluidRow(br(), progressBar(id = "lvl1ProgressBar", 
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
                              # Output box for flagged messages.
                              fluidRow(class = "output-section",
                                       tags$div(class = "output-text", 
                                                htmlOutput(outputId = "lvl1OutputText"))
                              )
                       ),
                       
                       column(width = 4,
                              # Output box for IDs.
                              fluidRow(class = "output-section",
                                       tags$div(class = "output-text", 
                                                htmlOutput(outputId = "lvl1OutputID"))
                              )
                       ),
                       
                       column(width = 12,
                              # Buttons below output box.
                              fluidRow(
                                actionButton(inputId = "lvl1CheckDirButton", 
                                             label = "Check Directory",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl1CheckREADMEButton", 
                                             label = "Check README",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl1CheckDICTButton", 
                                             label = "Check DICT",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl1CheckDATAButton", 
                                             label = "Check DATA",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl1CheckMISSINGButton", 
                                             label = "Check MISSING",
                                             class = "check-button")
                              )
                       )
             )
             )
           )
}

# [END]