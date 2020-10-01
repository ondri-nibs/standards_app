# selectUI.R
#
# Purpose: Create the user interface for the select tab to select a level 1 directory 
# and an IDs file by browsing the user's local computer and home directory.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-06-19
#
# ========================================================================================

selectUI <- function(){
  
 tabPanel(title = "Select",
           
          id = "Select",
          
           fluidRow(
             
             # Left side panel.
             column(
               class = "input-section",
               
               width = 6,
               
               shinyDirButton(id = "dir", 
                              label = "Click to select a directory.",
                              title = "Please select the level 1 directory of a data package.",
                              icon = icon("folder")),
               
               verbatimTextOutput("dirOutput", placeholder = TRUE),
               
               br(),
               
               selectInput(inputId = "StudySelection",
                           width = "200px",
                           label = "Select Study",
                           choices = c("- Choose A Study -", "OND01", "BEAM", "ReMiNDD", "Custom", "Package only checks")),
               
               conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                 radioButtons(inputId = "participantFile",
                            label = "Participant File",
                            choices = list("Check with Participant File" = TRUE,
                                           "Check without Participant File" = FALSE),
                            selected = TRUE
                            ),
               ),
               
               conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
                                conditionalPanel( condition = "input.participantFile == 'TRUE'",
                 shinyFilesButton(id = "participantIDFile",
                                  label = "Click to select a participant ID file.",
                                  title = "Please select the participant ID file.",
                                  multiple = FALSE,
                                  icon = icon("file")),
                 
                  verbatimTextOutput("participantIDFileOutput", placeholder = TRUE),
                                )
               ),
               
               br(),
               
               #Only shows this panel if the Study is OND01.
               conditionalPanel(
                 condition = "(input.StudySelection == 'OND01' & input.participantFile == 'TRUE')",
                 shinyFilesButton(id = "transferIDFile",
                                  label = "Click to select a transfer ID file.",
                                  title = "Please select the transfer ID file.",
                                  multiple = FALSE,
                                  icon = icon("file")),
                 
                 verbatimTextOutput("transferIDFileOutput", placeholder = TRUE),
               ),
               
               useShinyalert(),
               
               
               actionButton(inputId = "BeginCheck",
                            label = "Begin Check",
                            style = "color: white; background-color: #81D3EA;"),
               
               tableOutput('extTable'),
             ),
             
             # The input form on the right side panel
             
             conditionalPanel(condition = "(input.StudySelection != 'Package only checks')",
             column(width = 6,
                 
                   titlePanel("Codes Form"),
                   
                   # Only show study code section in table if study name == Custom
                   conditionalPanel(condition = "(input.StudySelection == 'Custom')",
                       tags$div(style="display:inline-block;",
                                fluidRow(
                                  column(8,
                                     textInput(
                                       inputId = "studyCode",
                                       label = "Study Code"),
                                  ),
                                  column(4,
                                     actionButton(style="margin-top:28px;
                                                  margin-left:-81px",
                                       inputId = "addStudyCode",
                                                  label = "Add"),
                                  )
                                )
                        )
                   ),
                   
                   
                   tags$div(style="display:inline-block;",
                            fluidRow(
                              column(8,
                                     textInput(
                                       inputId = "cohortCode",
                                       label = "Cohort Code"),
                              ),
                              column(4,
                                     actionButton(style="margin-top:28px;
                                              margin-left:-81px",
                                                  inputId = "addCohortCode",
                                                  label = "Add"),
                              )
                            )
                    ),
                   
                   tags$div(style="display:inline-block;",
                            fluidRow(
                              column(8,
                                     textInput(
                                       inputId = "visitCode",
                                       label = "Visit Code",
                                       value = 0),
                              ),
                              column(4,
                                     actionButton(style="margin-top:28px;
                                              margin-left:-81px",
                                                  inputId = "addVisitCode",
                                                  label = "Add"),
                              )
                            )
                   ),
                   
                   tags$div(style="display:inline-block;",
                            fluidRow(
                              column(8,
                                     textInput(
                                       inputId = "platformCode",
                                       label = "Platform Code"),
                              ),
                              column(4,
                                     actionButton(style="margin-top:28px;
                                              margin-left:-81px",
                                                  inputId = "addPlatformCode",
                                                  label = "Add"),
                              )
                            )
                   ),
                   
                   tags$div(style="display:inline-block;",
                            fluidRow(
                              column(8,
                                     textInput(
                                       inputId = "siteCode",
                                       label = "Site Code"),
                              ),
                              column(4,
                                     actionButton(style="margin-top:28px;
                                              margin-left:-81px",
                                                  inputId = "addSiteCode",
                                                  label = "Add"),
                              )
                            )
                   ),
                   
                   
                   tags$div(style="height:350px;
                            width:800px;
                            ",
                              tags$div(style="display:flex;
                                       ",
                                  
                                  conditionalPanel(condition = "(input.StudySelection == 'Custom')",
                                  tags$div(style="width: 150px;
                                           height: 84px;
                                           border: 1px solid black;
                                           background-color: grey",
                                    tags$h3(style="text-align:center;",
                                            "Study Codes"),
                                  )
                                  ),
                                  
                                  tags$div(style="width:200px;
                                           border: 1px solid black;
                                           background-color:grey;",
                                    tags$h3(style="text-align:center;",
                                            "Cohort Codes"),
                                  ),
                                  tags$div(style="width:200px;
                                           border: 1px solid black;
                                           background-color: grey",
                                  tags$h3(style="text-align:center;",
                                          "Visit Codes"),
                                  ),
                                  tags$div(style="width:200px;
                                           border: 1px solid black;
                                           background-color: grey",
                                  tags$h3(style="text-align:center;",
                                          "Platform Codes"),
                                  ),
                                  tags$div(style="width:200px;
                                           border: 1px solid black;
                                           background-color: grey",
                                  tags$h3(style="text-align:center;",
                                           "Sites Codes"),
                                  ),
                                  
                              ),
                            
                            tags$div(style="display: flex;",
                                     
                                     conditionalPanel(condition = "(input.StudySelection == 'Custom')",
                                       tags$div(style="background-color: white;
                                                border: 1px solid black;
                                                height: 300px;
                                                width: 150px;
                                                overflow: scroll",
                                          tags$div(id = "placeholder")
                                       )
                                     ),
                                     
                                       tags$div(style="background-color: white;
                                                border: 1px solid black;
                                                height: 300px;
                                                width: 200px;
                                                overflow: scroll",
                                                tags$div(id = "placeholder2")
                                       ),
                                       tags$div(style="background-color: white;
                                                border: 1px solid black;
                                                height: 300px;
                                                width: 200px;
                                                overflow: scroll",
                                                tags$div(id = "placeholder3")
                                       ),
                                       tags$div(style="background-color: white;
                                                border: 1px solid black;
                                                height: 300px;
                                                width: 200px;
                                                overflow: scroll",
                                                tags$div(id = "placeholder4")
                                                ),
                                                
                                       tags$div(style="background-color: white;
                                                border: 1px solid black;
                                                height: 300px;
                                                width: 200px;
                                                overflow: scroll",
                                                tags$div(id = "placeholder5")
                                       ),
                                                
                     )
               )
                 )
             )
               )
             )

}

# [END]