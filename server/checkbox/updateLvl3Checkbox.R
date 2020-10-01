# updateLvl3Checkbox.R
#
# Purpose: Create the server logic for the lvl 3 tab to select or deselect all
# checkboxes in the sidebar panel.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# ========================================================================================

updateLvl3Checkbox <- function(input, session){
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to TRUE only if "Select All" button is clicked.
    input$lvl3SelectButton, {
      updateCheckboxInput(session, inputId = "checkLvl3Required", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3FileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl3FileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyLvl3FileNameSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3CompressedFiles", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkFILELISTColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyLvl3FILELISTSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl3SubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3FILELISTTransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareParticipantFileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyOndriID", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTSiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTDateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTDateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkFILELISTEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyLvl3MISSINGSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGTransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl3ColumnNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGMissingColumn", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGSiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGDateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGDateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGEncapsulation", value = TRUE)
    }
  )
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to FALSE only if "Deselect All" button is clicked.
    input$lvl3DeselectButton, {
      updateCheckboxInput(session, inputId = "checkLvl3Required", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3FileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl3FileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyLvl3FileNameSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3CompressedFiles", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkFILELISTColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyLvl3FILELISTSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl3SubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3FILELISTTransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareParticipantFileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyOndriID", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTSiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTDateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTDateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkFILELISTEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyLvl3MISSINGSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGTransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl3ColumnNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGMissingColumn", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGSiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGDateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGDateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl3MISSINGEncapsulation", value = FALSE)
    }
  )
}

# [END]