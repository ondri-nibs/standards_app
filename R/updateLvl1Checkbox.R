# updateLvl1Checkbox.R
#
# Purpose: Create the server logic for the lvl 1 tab to select or deselect all
# checkboxes in the sidebar panel.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# ========================================================================================

updateLvl1Checkbox <- function(input, session){
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to TRUE only if "Select All" button is clicked.
    input$lvl1SelectButton, {
      updateCheckboxInput(session, inputId = "checkLvl1Required", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DirName", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1FileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl1FileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1FileExt", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1CompressedFiles", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DuplicateFileNames", value = TRUE)
      
      
      updateCheckboxInput(session, inputId = "compareLvl1READMEFileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1READMEBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1READMECommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1READMENumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1READMEWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1READMEEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkLVl1DICTColumnLabels", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLVl1DICTDataTypes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLVl1DICTBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLVl1DICTCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLVl1DICTNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLVl1DICTWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLVl1DICTEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkLvl1DATAColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAColumnNameSyntax", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyLvl1DATASubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl1DATASubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATATransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAPrecisionLevels", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATASpecialValues", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAMissingCodeRows", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATASiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATADateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATADateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATABlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATACommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATANumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyLvl1MISSINGSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl1MISSINGSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGTransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl1ColumnNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGMissingColumn", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGSiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGDateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGDateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGEncapsulation", value = TRUE)
    }
  )
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to FALSE only if "Deselect All" button is clicked.
    input$lvl1DeselectButton, {
      updateCheckboxInput(session, inputId = "checkLvl1Required", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DirName", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1FileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl1FileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1FileExt", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1CompressedFiles", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DuplicateFileNames", value = FALSE)
      
      updateCheckboxInput(session, inputId = "compareLvl1READMEFileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1READMEBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1READMECommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1READMENumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1READMEWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1READMEEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkLvl1DICTColumnLabels", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DICTDataTypes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DICTBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DICTCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DICTNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DICTWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DICTEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkLvl1DATAColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAColumnNameSyntax", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyLvl1DATASubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl1DATASubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATATransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAPrecisionLevels", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATASpecialValues", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAMissingCodeRows", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATASiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATADateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATADateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATABlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATACommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATANumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1DATAEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyLvl1MISSINGSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl1MISSINGSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGTransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl1ColumnNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGMissingColumn", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGSiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGDateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGDateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl1MISSINGEncapsulation", value = FALSE)
    }
  )
}

# [END]