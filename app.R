# app.R
#
# Purpose: Run the ONDRI Joint Standards Check Application by connecting user
# interface (front end) with server logic (back end).
#
# Please click on the "Run App" button in the top right corner of this pane
# to open the Shiny app.
#
# ========================================================================================
library(xml2) # creating xml element
library(shiny)

library(tools)#  to extract file extensions

library(shinyalert)
#this import might not be needed
library(V8)
library(stringr)
library(shinydashboard) # For Dashboard UI

library(plyr)
library(shiny)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)

library(shinyWidgets)
library(shinyFiles)
library(shinyjs)        # To disable checkboxes for mandatory checks.
library(dplyr)

library(rowr)           # For printing of subject IDs.
library(varhandle)      # For precision level check.
library(lubridate)      # For date range check.
library(readr)          # For encoding check.

source("UI/NonTabularUI.R")
source("UI/selectUI.R")
source("UI/lvl1UI.R")
source("UI/lvl2UI.R")
source("UI/lvl3UI.R")
source("UI/TabularUI.R")

source("server/dataStorage/create_log_file.R")
source("server/dataStorage/dataRetainer.R")
source("server/dataStorage/logfile_datastorage.R")
source("server/dataStorage/XML_add_child.R")
       
source("server/nontabularServer.R")
source("server/tabularserver.R")
source("server/checkbox/updateTabularCheckbox.R")
source("server/selectServer.R")
source("server/lvl1Server.R")
source("server/checkbox/updateLvl1Checkbox.R")
source("server/lvl2Server.R")
source("server/checkbox/updateLvl2Checkbox.R")
source("server/lvl3Server.R")
source("server/checkbox/updateLvl3Checkbox.R")

source("server/lvl1checks/checkForDuplicateFileNames.R")  
source("server/lvl1checks/checkLvl1MISSINGColumnNames.R")
source("server/lvl2checks/checkLvl2MISSINGColumnNames.R")
source("server/lvl3checks/checkLvl3MISSINGColumnNames.R")

source("server/common/convertDataFrame.R")
source("server/common/output/outputSelectionErrorMsg.R")
source("server/common/output/outputMissingErrorMsg.R")
source("server/common/output/outputDoesNotExistMsg.R")
source("server/common/output/combineFlaggedMsgs.R") 
source("server/common/get/getParticipantIDDF.R")
source("server/common/get/getFileName.R")
source("server/common/get/getFirstColumn.R")

source("server/common/check/checkForFileTypes.R") # Mandatory check.
source("server/common/check/checkRequired.R")     # Mandatory check.
source("server/common/check/checkEncoding.R")     # Mandatory check.
source("server/common/check/checkLvl1DirName.R")  # Mandatory check.
source("server/common/check/checkFileNames.R")    # Mandatory check.
source("server/common/check/compareFileNames.R")  # Mandatory check.

source("server/common/check/compareREADMEFileNames.R")
source("server/common/check/columnNames/checkColumnLabels.R")
source("server/common/check/columnNames/checkColumnNameSyntax.R")
source("server/common/check/columnNames/checkForColumnNameDuplicates.R")
source("server/common/check/columnNames/checkColumnNameFormat.R")
source("server/common/check/columnNames/checkREADMEColumnNames.R")
source("server/common/check/columnNames/checkDICTColumnNames.R")
source("server/common/check/columnNames/checkDATAColumnNames.R")
source("server/common/check/columnNames/checkMISSINGColumnNames.R")
source("server/common/check/columnNames/compareColumnNames.R") # For MISSING files only.

source("server/common/check/checkForCompressedFiles.R")
source("server/common/check/verifySubjectIDs.R")          # For DATA/FILELIST/MISSING files.
source("server/common/check/checkForTransferIDs.R")       # For DATA/FILELIST/MISSING files.
source("server/common/get/getTransferIDLocations.R")      # For DATA/FILELIST/MISSING files.
source("server/common/check/checkDataTypes.R")            # For DICT files only.
source("server/common/get/getInvalidDataTypes.R")         # For DICT files only.
source("server/common/check/checkPrecisionLevels.R")      # For DATA files only.
source("server/common/get/getInvalidPrecisionLevels.R")   # For DATA files only.
source("server/common/check/checkForSpecialValues.R")     # For DATA files only.
source("server/common/get/getSpecialValueLocations.R")    # For DATA files only.
source("server/common/check/checkForMissingCodeRows.R")   # For DATA files only.
source("server/common/check/checkVisitCodes.R")
source("server/common/get/getInvalidVisitCodes.R")
source("server/common/check/checkSiteCodes.R")
source("server/common/get/getInvalidSiteCodes.R")
source("server/common/check/checkDateFormat.R")
source("server/common/get/getInvalidDateFormat.R")
source("server/common/check/checkDateRange.R")
source("server/common/check/checkMissingCodes.R")
source("server/common/get/getInvalidMissingCodes.R")
source("server/common/check/checkMissingCodeColumn.R")    # For MISSING files only.
source("server/common/get/getInvalidMissingCodeColumn.R") # For MISSING files only.
source("server/common/check/checkForBlankCells.R")   
source("server/common/get/getBlankCellLocations.R")
source("server/common/check/checkForCommas.R")
source("server/common/get/getCommaLocations.R")
source("server/common/check/checkNumOfCharacters.R")
source("server/common/get/getNumOfCharacterLocations.R")
source("server/common/check/checkForWhiteSpaces.R")
source("server/common/get/getWhiteSpaceLocations.R")
source("server/common/check/checkForEncapsulation.R")
source("server/common/get/getEncapsulationLocations.R")

source("server/common/check/checkIftab.R")

# Create the user interface (front end).
ui <- fluidPage(theme = "style.css",
                
                # Create title with logo.
                titlePanel(title = tags$div(
                  img(class = "ondri-logo", src = "ONDRI_full-logo_web.png",
                      style="width: 278px;"), 
                  "ONDRI Complete Standards Application 0.9.0.9001"),
                  windowTitle = "ONDRI Complete Standards Application"),
                
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
shinyApp(ui = ui, server = server)

# [END]