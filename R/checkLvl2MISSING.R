# checkLvl2MISSING.R
#
# Purpose: Create the server logic for level 2 MISSING file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# =======================================================================================

checkLvl2MISSING <- function(input, session, lvl1DirPath, numOfCheckboxes, participantIDDF,
                             transferIDDF, study_name, participantFile){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a MISSING file.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 2 DIRECTORY.
    missingFileName <- getFileName(lvl2DirPath, "MISSING.csv")
    if (length(missingFileName) == 1){
      missingFilePath <- paste0(lvl2DirPath, "/", missingFileName)
      missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE)
      missingDF <- convertDataFrame(missingDF)
      
      # "Column Name Format Check" checkbox.
      if (input$checkLvl2MISSINGColumnNameFormat & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                                            dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGColumnNameFormat/>")
        x <- identical(checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                             dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGColumnNameFormat", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGColumnNameFormat", "FAIL")
          XML_add_txt("checkLvl2MISSINGColumnNameFormat", "ERROR: File contains incorrect column name format.")
        }
      }
      
      # "Subject IDs Verification" checkbox.
      if (input$verifyLvl2MISSINGSubjectIDs & participantFile == TRUE){
        flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(missingDF, participantIDDF, 
                                                       basename(lvl1DirPath),
                                                       dirName, "MISSING"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("MISSING2","<verifyLvl2MISSINGSubjectIDs/>")
        x <- identical(verifySubjectIDs(missingDF, participantIDDF, basename(lvl1DirPath),
                                        dirName, "MISSING"), character(0))
        if (x == TRUE){
          XML_add_attr("verifyLvl2MISSINGSubjectIDs", "PASS")
        }else{
          XML_add_attr("verifyLvl2MISSINGSubjectIDs", "FAIL")
          XML_add_txt("verifyLvl2MISSINGSubjectIDs", "ERROR: File contains subject ID issues.")
        }
      }
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl2MISSINGTransferIDs & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkForTransferIDs(missingDF, transferIDDF,
                                                          dirName, "MISSING"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 7)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGTransferIDs/>")
        x <- identical(checkForTransferIDs(missingDF, transferIDDF,
                                           dirName, "MISSING"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGTransferIDs", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGTransferIDs", "FAIL")
          XML_add_txt("checkLvl2MISSINGTransferIDs", "ERROR: File contains transfer ID issues.")
        }
      }
      
      # "Column Names Comparison" checkbox. Only run algorithm if DATA file exists in level 2.
      if (input$compareLvl2ColumnNames & study_name != "Package only checks"){
        dataFileName <- getFileName(lvl2DirPath, "DATA.csv")
        if (length(dataFileName) == 1){
          dataFilePath <- paste0(lvl2DirPath, "/", dataFileName)
          dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
          dataDF <- convertDataFrame(dataDF)
          
          flaggedMsgs <- c(flaggedMsgs, compareColumnNames(missingDF, dataDF, dirName, "DATA"))
        }
        
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("MISSING2","<compareLvl2ColumnNames/>")
        x <- identical(compareColumnNames(missingDF, dataDF, dirName, "DATA"), character(0))
        if (x == TRUE){
          XML_add_attr("compareLvl2ColumnNames", "PASS")
        }else{
          XML_add_attr("compareLvl2ColumnNames", "FAIL")
          XML_add_txt("compareLvl2ColumnNames", "ERROR: File Name comparison failed.")
        }
      }
      
      # "MISSING_CODE Column Check" checkbox.
      if (input$checkLvl2MISSINGMissingColumn & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodeColumn(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGMissingColumn/>")
        x <- identical(checkMissingCodeColumn(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGMissingColumn", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGMissingColumn", "FAIL")
          XML_add_txt("checkLvl2MISSINGMissingColumn", "ERROR: File contains issues in the missing column codes.")
        }
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl2MISSINGVisitCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                                      dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 10)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGVisitCodes/>")
        x <- identical(checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                       dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGVisitCodes", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGVisitCodes", "FAIL")
          XML_add_txt("checkLvl2MISSINGVisitCodes", "ERROR: File contains visit code issues.")
        }
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl2MISSINGSiteCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 11)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGSiteCodes/>")
        x <- identical(checkSiteCodes(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGSiteCodes", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGSiteCodes", "FAIL")
          XML_add_txt("checkLvl2MISSINGSiteCodes", "ERROR: File contains site code issues.")
        }
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl2MISSINGDateFormat & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkDateFormat(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 12)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGDateFormat/>")
        x <- identical(checkDateFormat(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGDateFormat", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGDateFormat", "FAIL")
          XML_add_txt("checkLvl2MISSINGDateFormat", "ERROR: File contains date format issues.")
        }
      }
      
      # "Date Range Check" checkbox.
      if (input$checkLvl2MISSINGDateRange & participantFile == TRUE){
        # SPECIAL CASE: Only run date range check if subject IDs verification has passed.
        if (length(verifySubjectIDs(missingDF, participantIDDF, basename(lvl1DirPath),
                                    dirName, "MISSING")) != 0){
          line <- paste(tags$span(class = "bold-category", 
                                  "Date Range: Directory", dirName),
                        "- Cannot check date range as subject IDs verification did not pass.")
          flaggedMsgs <- c(flaggedMsgs, line)
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                         "<br/>"))
        }
        else{
          flaggedMsgs <- c(flaggedMsgs, checkDateRange(missingFilePath, participantIDDF, 
                                                       basename(lvl1DirPath), dirName, study_name))
          # Log input
          XML_add_child("MISSING2","<checkLvl2MISSINGDateRange/>")
          x <- identical(checkDateRange(missingFilePath, participantIDDF, 
                                        basename(lvl1DirPath), dirName, study_name), character(0))
          if (x == TRUE){
            XML_add_attr("checkLvl2MISSINGDateRange", "PASS")
          }else{
            XML_add_attr("checkLvl2MISSINGDateRange", "FAIL")
            XML_add_txt("checkLvl2MISSINGDateRange", "ERROR: File contains date range issues.")
          }
        }
        
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 13)
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl2MISSINGMissingCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 14)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGMissingCodes/>")
        x <- identical(checkMissingCodes(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGMissingCodes", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGMissingCodes", "FAIL")
          XML_add_txt("checkLvl2MISSINGMissingCodes", "ERROR: File contains missing code issues.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl2MISSINGBlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(missingFilePath, dirName, 
                                                         "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 15)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGBlankCells/>")
        x <- identical(checkForBlankCells(missingFilePath, dirName, 
                                          "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGBlankCells", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGBlankCells", "FAIL")
          XML_add_txt("checkLvl2MISSINGBlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl2MISSINGCommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 16)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGCommas/>")
        x <- identical(checkForCommas(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGCommas", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGCommas", "FAIL")
          XML_add_txt("checkLvl2MISSINGCommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkLvl2MISSINGNumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 17)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGNumOfCharacters/>")
        x <- identical(checkNumOfCharacters(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGNumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGNumOfCharacters", "FAIL")
          XML_add_txt("checkLvl2MISSINGNumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl2MISSINGWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 18)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGWhiteSpaces", "FAIL")
          XML_add_txt("checkLvl1MISSINGWhiteSpaces", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl2MISSINGEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100 / numOfCheckboxes * 19)
        # Log input
        XML_add_child("MISSING2","<checkLvl2MISSINGEncapsulation/>")
        x <- identical(checkForEncapsulation(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2MISSINGEncapsulation", "PASS")
        }else{
          XML_add_attr("checkLvl2MISSINGEncapsulation", "FAIL")
          XML_add_txt("checkLvl2MISSINGEncapsulation", "ERROR: File contains encapsulation issues.")
        }
      }
    }
  }
  
  return (flaggedMsgs)
}

# [END]