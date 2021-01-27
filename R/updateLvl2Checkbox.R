# updateLvl2Checkbox.R
#
# Purpose: Create the server logic for the lvl 2 tab to select or deselect all
# checkboxes in the sidebar panel.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# ========================================================================================

updateLvl2Checkbox <- function(input, session){
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to TRUE only if "Select All" button is clicked.
    input$lvl2SelectButton, {
      updateCheckboxInput(session, inputId = "checkLvl2Required", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DirNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2FileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl2FileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2CompressedFiles", value = TRUE)
      
      updateCheckboxInput(session, inputId = "compareLvl2READMEFileNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2READMEBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2READMECommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2READMENumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2READMEWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2READMEEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkLvl2DICTColumnLabels", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTDataTypes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkLvl2DATAColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAColumnNameSyntax", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyLvl2DATASubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl2DATASubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATATransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAPrecisionLevels", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATASpecialValues", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAMissingCodeRows", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATASiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATADateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATADateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATABlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATACommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATANumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAEncapsulation", value = TRUE)
      
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGColumnNameFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "verifyLvl2MISSINGSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl2MISSINGSubjectIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGTransferIDs", value = TRUE)
      updateCheckboxInput(session, inputId = "compareLvl2ColumnNames", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGMissingColumn", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGVisitCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGSiteCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGDateFormat", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGDateRange", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGMissingCodes", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGBlankCells", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGCommas", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGNumOfCharacters", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGWhiteSpaces", value = TRUE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGEncapsulation", value = TRUE)
    }
  )
  
  # Reactive function.
  observeEvent(
    # Update value of all checkboxes to FALSE only if "Deselect All" button is clicked.
    input$lvl2DeselectButton, {
      updateCheckboxInput(session, inputId = "checkLvl2Required", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DirNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2FileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl2FileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2CompressedFiles", value = FALSE)
      
      updateCheckboxInput(session, inputId = "compareLvl2READMEFileNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2READMEBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2READMECommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2READMENumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2READMEWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2READMEEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkLvl2DICTColumnLabels", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTDataTypes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DICTEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkLvl2DATAColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAColumnNameSyntax", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyLvl2DATASubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl2DATASubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATATransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAPrecisionLevels", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATASpecialValues", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAMissingCodeRows", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATASiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATADateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATADateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATABlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATACommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATANumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2DATAEncapsulation", value = FALSE)
      
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGColumnNameFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "verifyLvl2MISSINGSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl2MISSINGSubjectIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGTransferIDs", value = FALSE)
      updateCheckboxInput(session, inputId = "compareLvl2ColumnNames", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGMissingColumn", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGVisitCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGSiteCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGDateFormat", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGDateRange", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGMissingCodes", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGBlankCells", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGCommas", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGNumOfCharacters", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGWhiteSpaces", value = FALSE)
      updateCheckboxInput(session, inputId = "checkLvl2MISSINGEncapsulation", value = FALSE)
    }
  )
}

# [END]