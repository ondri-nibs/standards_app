# lvl3UI.R
#
# Purpose: Create the user interface for the level 3 tab.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-03
#
# ========================================================================================


lvl3UI <- function(){
  tabPanel(title = "Level3Checks",
           
           
           id = "Level3Checks",
           
           sidebarLayout(
             # Left side panel.
             sidebarPanel(width = 3, class = "input-section", useShinyjs(),
                          
                          tags$strong("Directory - Run: "),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                          
                            checkboxInput(inputId = "checkLvl3Required",
                                          label = "Required Files Check",
                                          value = TRUE),
                            
                            # Checkbox inputs to select standards to run.
                            checkboxInput(inputId = "checkLvl3FileNames",
                                          label = "File Names Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "compareLvl3FileNames", 
                                          label = "File Names Comparison",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "verifyLvl3FileNameSubjectIDs",
                                          label = "File Name Subject IDs Verification",
                                          value = TRUE)
                          
                          ),
                          
                          checkboxInput(inputId = "checkLvl3CompressedFiles",
                                        label = "Compressed Files Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("FILELIST - Run: "),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                           
                            # Checkbox inputs to select standards to run.
                            checkboxInput(inputId = "checkLvl3Required1",
                                          label = "Required Files Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkFILELISTColumnNames",
                                          label = "Column Names Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1DirName5",
                                          label = "Lvl 1 Directory Name Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkFILELISTColumnNameFormat",
                                          label = "Column Name Format Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "verifyLvl3FILELISTSubjectIDs",
                                          label = "Subject IDs Verification",
                                          value = TRUE),
                            
                            conditionalPanel( condition = "input.participantFile == 'TRUE'",
                                              
                              checkboxInput(inputId = "compareLvl3SubjectIDs",
                                            label = "Subject IDs Comparison",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkFILELISTDateRange",
                                            label = "Date Range Check",
                                            value = TRUE)
                            ),
                            
                            checkboxInput(inputId = "checkLvl3FILELISTTransferIDs",
                                          label = "Transfer IDs Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "compareParticipantFileNames", 
                                          label = "Participant File Names Comparison",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "verifyOndriID",
                                          label = "ONDRI ID Verification",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkFILELISTVisitCodes",
                                          label = "Visit Codes Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkFILELISTSiteCodes",
                                          label = "Site Codes Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkFILELISTMissingCodes",
                                          label = "Missing Codes Check",
                                          value = TRUE)
                          ),
                          
                          checkboxInput(inputId = "checkFILELISTDateFormat",
                                        label = "Date Format Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkFILELISTBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkFILELISTCommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkFILELISTNumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkFILELISTWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkFILELISTEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          br(), br(),
                          
                          tags$strong("MISSING - Run: This only runs on level 3
                                      directories that contain a MISSING file."),
                          
                          conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                          
                            checkboxInput(inputId = "checkLvl3Required2",
                                          label = "Required Files Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl3MISSINGColumnNames",
                                          label = "Column Names Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl1DirName6",
                                          label = "Lvl 1 Directory Name Check (Mandatory)",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl3MISSINGColumnNameFormat",
                                          label = "Column Name Format Check",
                                          value = TRUE),
                            
                            conditionalPanel(condition = "input.participantFile == 'TRUE'",
                                                
                              checkboxInput(inputId = "verifyLvl3MISSINGSubjectIDs",
                                            label = "Subject IDs Verification",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl3MISSINGDateFormat",
                                            label = "Missing Codes Check",
                                            value = TRUE),
                              
                              checkboxInput(inputId = "checkLvl3MISSINGDateRange",
                                            label = "Date Range Check",
                                            value = TRUE)
                            ),
                            checkboxInput(inputId = "checkLvl3MISSINGTransferIDs",
                                          label = "Transfer IDs Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "compareLvl3ColumnNames",
                                          label = "Column Names Comparison",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl3MISSINGMissingColumn",
                                          label = "MISSING_CODE Column Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl3MISSINGVisitCodes",
                                          label = "Visit Codes Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl3MISSINGSiteCodes",
                                          label = "Site Codes Check",
                                          value = TRUE),
                            
                            checkboxInput(inputId = "checkLvl3MISSINGMissingCodes",
                                          label = "Missing Codes Check",
                                          value = TRUE)
                          ),
                          
                          checkboxInput(inputId = "checkLvl3MISSINGBlankCells",
                                        label = "Blank Cells Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl3MISSINGCommas",
                                        label = "Commas Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl3MISSINGNumOfCharacters",
                                        label = "Number of Characters Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl3MISSINGWhiteSpaces",
                                        label = "White Spaces Check",
                                        value = TRUE),
                          
                          checkboxInput(inputId = "checkLvl3MISSINGEncapsulation",
                                        label = "Encapsulation Check",
                                        value = TRUE),
                          
                          
                          br(),
                          
                          # Buttons to select or deselect all checkboxes.
                          actionButton(inputId = "lvl3SelectButton", 
                                       label = "Select All",
                                       class = "select-button"),
                          
                          actionButton(inputId = "lvl3DeselectButton", 
                                       label = "Deselect All",
                                       class = "deselect-button")
             ),
             
             # Right side panel.
             mainPanel(width = 9,
                       column(width = 8, 
                              # Static progress bar.
                              fluidRow(br(), progressBar(id = "lvl3ProgressBar", 
                                                         value = 0,
                                                         display_pct = T)
                                       )
                              ),
                       
                       column(width = 4,
                              # Dropdown to select algorithm.
                              selectInput(inputId = "lvl3AlgoSelection", label = "", 
                                          choices = "")
                              ),
                       
                       column(width = 8,
                              # Output box for flagged messages.
                              fluidRow(class = "output-section",
                                       tags$div(class = "output-text", 
                                                htmlOutput(outputId = "lvl3OutputText"))
                                       )
                              ),
                       
                       column(width = 4,
                              # Output box for IDs.
                              fluidRow(class = "output-section",
                                       tags$div(class = "output-text", 
                                                htmlOutput(outputId = "lvl3OutputID"))
                                       )
                              ),
                       
                       column(width = 12,
                              # Buttons below output box.
                              fluidRow(
                                actionButton(inputId = "lvl3CheckDirButton", 
                                             label = "Check Directory",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl3CheckFILELISTButton", 
                                             label = "Check FILELIST",
                                             class = "check-button"),
                                
                                actionButton(inputId = "lvl3CheckMISSINGButton", 
                                             label = "Check MISSING",
                                             class = "check-button")
                                )
                              )
                       )
             )
           )
}

# [END]