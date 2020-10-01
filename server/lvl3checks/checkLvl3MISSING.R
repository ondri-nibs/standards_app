# checkLvl3MISSING.R
#
# Purpose: Create the server logic for level 3 MISSING file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

checkLvl3MISSING <- function(input, session, lvl1DirPath, numOfCheckboxes, participantIDDF,
                             transferIDDF, study_name, participantFile){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 3 directories that
  # contain a MISSING file.
  for (dirName in lvl2Dirs) {
    lvl3DirPath <- paste0(lvl1DirPath, "/", dirName, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    
    # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 3 DIRECTORY.
    missingFileName <- getFileName(lvl3DirPath, "MISSING.csv")
    if (length(missingFileName) == 1){
      missingFilePath <- paste0(lvl3DirPath, "/", missingFileName)
      missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE)
      missingDF <- convertDataFrame(missingDF)
      
      filelistFilePath <- paste0(lvl3DirPath, "/", getFileName(lvl3DirPath, "FILELIST.csv"))
      filelistDF <- read.csv(filelistFilePath, stringsAsFactors = FALSE)
      filelistDF <- convertDataFrame(filelistDF)
      
      # "Column Name Format Check" checkbox.
      if (input$checkLvl3MISSINGColumnNameFormat & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                                            lvl3DirName))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGColumnNameFormat/>")
        x <- identical( checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                              lvl3DirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGColumnNameFormat", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGColumnNameFormat", "FAIL")
          XML_add_txt("checkLvl3MISSINGColumnNameFormat", "ERROR: File contains incorrect column name format.")
        }
      }
      
      # "Subject IDs Verification" checkbox.
      if (input$verifyLvl3MISSINGSubjectIDs & participantFile == TRUE){
        flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(missingDF, participantIDDF, 
                                                       basename(lvl1DirPath),
                                                       lvl3DirName, "MISSING"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("MISSING3","<verifyLvl3MISSINGSubjectIDs/>")
        x <- identical(verifySubjectIDs(missingDF, participantIDDF, 
                                        basename(lvl1DirPath),
                                        lvl3DirName, "MISSING"), character(0))
        if (x == TRUE){
          XML_add_attr("verifyLvl3MISSINGSubjectIDs", "PASS")
        }else{
          XML_add_attr("verifyLvl3MISSINGSubjectIDs", "FAIL")
          XML_add_txt("verifyLvl3MISSINGSubjectIDs", "ERROR: File contains subject ID issues.")
        }
      }
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl3MISSINGTransferIDs & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkForTransferIDs(missingDF, transferIDDF,
                                                          dirName, "MISSING"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 6)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGTransferIDs/>")
        x <- identical(checkForTransferIDs(missingDF, transferIDDF,
                                           dirName, "MISSING"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGTransferIDs", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGTransferIDs", "FAIL")
          XML_add_txt("checkLvl3MISSINGTransferIDs", "ERROR: File contains transfer ID issues.")
        }
      }
      
      # "Column Names Comparison" checkbox.
      if (input$compareLvl3ColumnNames & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, compareColumnNames(missingDF, filelistDF, lvl3DirName,
                                                         "FILELIST"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 7)
        # Log input
        XML_add_child("MISSING3","<compareLvl3ColumnNames/>")
        x <- identical(compareColumnNames(missingDF, filelistDF, lvl3DirName,
                                          "FILELIST"), character(0))
        if (x == TRUE){
          XML_add_attr("compareLvl3ColumnNames", "PASS")
        }else{
          XML_add_attr("compareLvl3ColumnNames", "FAIL")
          XML_add_txt("compareLvl3ColumnNames", "ERROR: File Name comparison failed.")
        }
      }
      
      # "MISSING_CODE Column Check" checkbox.
      if (input$checkLvl3MISSINGMissingColumn & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodeColumn(missingDF, lvl3DirName))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGMissingColumn/>")
        x <- identical(checkMissingCodeColumn(missingDF, lvl3DirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGMissingColumn", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGMissingColumn", "FAIL")
          XML_add_txt("checkLvl3MISSINGMissingColumn", "ERROR: File contains issues in the missing column codes.")
        }
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl3MISSINGVisitCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                                      lvl3DirName))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGVisitCodes/>")
        x <- identical( checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                        lvl3DirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGVisitCodes", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGVisitCodes", "FAIL")
          XML_add_txt("checkLvl3MISSINGVisitCodes", "ERROR: File contains visit code issues.")
        }
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl3MISSINGSiteCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(missingDF, lvl3DirName))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 10)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGSiteCodes/>")
        x <- identical(checkSiteCodes(missingDF, lvl3DirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGSiteCodes", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGSiteCodes", "FAIL")
          XML_add_txt("checkLvl3MISSINGSiteCodes", "ERROR: File contains site code issues.")
        }
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl3MISSINGDateFormat & participantFile == TRUE){
        flaggedMsgs <- c(flaggedMsgs, checkDateFormat(missingDF, lvl3DirName))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 11)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGDateFormat/>")
        x <- identical(checkDateFormat(missingDF, lvl3DirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGDateFormat", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGDateFormat", "FAIL")
          XML_add_txt("checkLvl3MISSINGDateFormat", "ERROR: File contains date format issues.")
        }
      }
      
      # "Date Range Check" checkbox.
      if (input$checkLvl3MISSINGDateRange & participantFile == TRUE){
        # SPECIAL CASE: Only run date range check if subject IDs verification has passed.
        if (length(verifySubjectIDs(missingDF, participantIDDF, basename(lvl1DirPath),
                                    lvl3DirName, "MISSING")) != 0){
          line <- paste(tags$span(class = "bold-category", 
                                  "Date Range: Directory", lvl3DirName),
                        "- Cannot check date range as subject IDs verification did not pass.")
          flaggedMsgs <- c(flaggedMsgs, line)
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                         "<br/>"))
        }
        else{
          flaggedMsgs <- c(flaggedMsgs, checkDateRange(missingFilePath, participantIDDF,
                                                       basename(lvl1DirPath), lvl3DirName, study_name))
          # Log input
          XML_add_child("MISSING3","<checkLvl3MISSINGDateRange/>")
          x <- identical(checkDateRange(missingFilePath, participantIDDF, 
                                        basename(lvl1DirPath), lvl3DirName, study_name), character(0))
          if (x == TRUE){
            XML_add_attr("checkLvl3MISSINGDateRange", "PASS")
          }else{
            XML_add_attr("checkLvl3MISSINGDateRange", "FAIL")
            XML_add_txt("checkLvl3MISSINGDateRange", "ERROR: File contains date range issues.")
          }
        }
        
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 12)
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl3MISSINGMissingCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(missingDF, lvl3DirName))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 13)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGMissingCodes/>")
        x <- identical(checkMissingCodes(missingDF, lvl3DirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGMissingCodes", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGMissingCodes", "FAIL")
          XML_add_txt("checkLvl3MISSINGMissingCodes", "ERROR: File contains missing code issues.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl3MISSINGBlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(missingFilePath, lvl3DirName,
                                                         "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 14)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGBlankCells/>")
        x <- identical(checkForBlankCells(missingFilePath, lvl3DirName,
                                          "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGBlankCells", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGBlankCells", "FAIL")
          XML_add_txt("checkLvl3MISSINGBlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl3MISSINGCommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(missingDF, lvl3DirName,
                                                     "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 15)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGCommas/>")
        x <- identical(checkForCommas(missingDF, lvl3DirName,
                                      "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGCommas", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGCommas", "FAIL")
          XML_add_txt("checkLvl3MISSINGCommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkLvl3MISSINGNumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(missingDF, lvl3DirName,
                                                           "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 16)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGNumOfCharacters/>")
        x <- identical(checkNumOfCharacters(missingDF, lvl3DirName,
                                            "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGNumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGNumOfCharacters", "FAIL")
          XML_add_txt("checkLvl3MISSINGNumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl3MISSINGWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(missingDF, lvl3DirName,
                                                          "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 17)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(missingDF, lvl3DirName,
                                           "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGWhiteSpaces", "FAIL")
          XML_add_txt("checkLvl3MISSINGWhiteSpaces", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl3MISSINGEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(missingDF, lvl3DirName,
                                                            "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 18)
        # Log input
        XML_add_child("MISSING3","<checkLvl3MISSINGEncapsulation/>")
        x <- identical(checkForEncapsulation(missingDF, lvl3DirName,
                                             "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl3MISSINGEncapsulation", "PASS")
        }else{
          XML_add_attr("checkLvl3MISSINGEncapsulation", "FAIL")
          XML_add_txt("checkLvl3MISSINGEncapsulation", "ERROR: File contains encapsulation issues.")
        }
      }
    }
  }
  
  return (flaggedMsgs)
}

# [END]