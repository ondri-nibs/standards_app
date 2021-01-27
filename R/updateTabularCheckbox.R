# updateTabularCheckbox.R
#
# Purpose: Create the server logic for the tabular tab to select or deselect all
# checkboxes in the sidebar panel.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
# Modified by: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2019-07-15
# Modified: 2020-06-12
# ========================================================================================

updateTabularCheckbox <- function(input, session){
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to TRUE only if "Select All" button is clicked.
    input$tabularSelectButton, {
      updateCheckboxInput(session, inputId = "checkTabularRequired", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDirName", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularFileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "compareTabularFileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularFileExt", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularCompressedFiles", value = TRUE)
      
      updateCheckboxInput(session, inputId = "compareTabularREADMEFileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularREADMEBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularREADMECommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularREADMENumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularREADMEWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularREADMEEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkTabularDICTColumnLabels", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDICTDataTypes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDICTBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDICTCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDICTNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDICTWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDICTEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkTabularDATAColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATAColumnNameSyntax", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyTabularDATASubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareTabularDATASubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATATransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATAPrecisionLevels", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATASpecialValues", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATAMissingCodeRows", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATAVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATASiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATADateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATADateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATAMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATABlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATACommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATANumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATAWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularDATAEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkTabularMISSINGColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyTabularMISSINGSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareTabularMISSINGSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGTransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareTabularColumnNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGMissingColumn", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGSiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGDateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGDateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGEncapsulation", value = TRUE)
    }
  )
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to FALSE only if "Deselect All" button is clicked.
    input$tabularDeselectButton, {
      updateCheckboxInput(session, inputId = "checkTabularRequired", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDirName", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularFileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "compareTabularFileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularFileExt", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularCompressedFiles", value = FALSE)
      
      
      updateCheckboxInput(session, inputId = "compareTabularREADMEFileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularREADMEBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularREADMECommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularREADMENumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularREADMEWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularREADMEEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkTabularDICTColumnLabels", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDICTDataTypes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDICTBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDICTCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDICTNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDICTWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDICTEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkTabularDATAColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATAColumnNameSyntax", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyTabularDATASubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareTabularDATASubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATATransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATAPrecisionLevels", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATASpecialValues", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATAMissingCodeRows", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATAVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATASiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATADateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATADateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATAMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATABlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATACommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATANumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATAWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularDATAEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkTabularMISSINGColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyTabularMISSINGSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareTabularMISSINGSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGTransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareTabularColumnNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGMissingColumn", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGSiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGDateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGDateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkTabularMISSINGEncapsulation", value = FALSE)
    }
  )
}

# [END]