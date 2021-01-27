# checkLvl1MISSING.R
#
# Purpose: Create the server logic for level 1 MISSING file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# =======================================================================================

checkLvl1MISSING <- function(input, session, lvl1DirPath, numOfCheckboxes, participantIDDF,
                             transferIDDF, study_name, participantFile){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
    
  # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 1 DIRECTORY.
  missingFileName <- getFileName(lvl1DirPath, "MISSING.csv")
  if (length(missingFileName) == 1){
    missingFilePath <- paste0(lvl1DirPath, "/", missingFileName)
    missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE)
    missingDF <- convertDataFrame(missingDF)
    
    
    # For lvl1 MISSING =====================================================================
    if (checkIfTab(lvl1DirPath) == "non-tabular"){

      # "Column Name Format Check" checkbox.
      if (input$checkLvl1MISSINGColumnNameFormat & study_name != "Package only checks"){
        
        flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                                            dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGColumnNameFormat/>")
        x <- identical(checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                             dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGColumnNameFormat", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGColumnNameFormat", "FAIL")
          XML_add_txt("checkLvl1MISSINGColumnNameFormat", "ERROR: File contains incorrect column name format.")
        }
      }
      
      # "Subject IDs Verification" checkbox.
      if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
        flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(missingDF, participantIDDF, basename(lvl1DirPath),
                                                       dirName, "MISSING"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("MISSING","<verifyLvl1MISSINGSubjectIDs/>")
        x <- identical(verifySubjectIDs(missingDF, participantIDDF, basename(lvl1DirPath),
                                        dirName, "MISSING"), character(0))
        if (x == TRUE){
          XML_add_attr("verifyLvl1MISSINGSubjectIDs", "PASS")
        }else{
          XML_add_attr("verifyLvl1MISSINGSubjectIDs", "FAIL")
          XML_add_txt("verifyLvl1MISSINGSubjectIDs", "ERROR: File contains subject ID issues.")
        }
      }
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl1MISSINGTransferIDs & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkForTransferIDs(missingDF, transferIDDF,
                                                          dirName, "MISSING"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 7)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGTransferIDs/>")
        x <- identical(checkForTransferIDs(missingDF, transferIDDF,
                                           dirName, "MISSING"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGTransferIDs", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGTransferIDs", "FAIL")
          XML_add_txt("checkLvl1MISSINGTransferIDs", "ERROR: File contains transfer ID issues.")
        }
      }
      
      # "Column Names Comparison" checkbox. Only run algorithm if DATA file exists in level 1.
      if (input$compareLvl1ColumnNames & study_name != "Package only checks"){
        dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
        if (length(dataFileName) == 1){
          dataFilePath <- paste0(lvl1DirPath, "/", dataFileName)
          dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
          dataDF <- convertDataFrame(dataDF)
          
          flaggedMsgs <- c(flaggedMsgs, compareColumnNames(missingDF, dataDF, dirName, "DATA"))
        }
        
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("MISSING","<compareLvl1ColumnNames/>")
        x <- identical(compareColumnNames(missingDF, dataDF, dirName, "DATA"), character(0))
        if (x == TRUE){
          XML_add_attr("compareLvl1ColumnNames", "PASS")
        }else{
          XML_add_attr("compareLvl1ColumnNames", "FAIL")
          XML_add_txt("compareLvl1ColumnNames", "ERROR: File Name comparison failed.")
        }
      }
      
      # "MISSING_CODE Column Check" checkbox.
      if (input$checkLvl1MISSINGMissingColumn & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodeColumn(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGMissingColumn/>")
        x <- identical(checkMissingCodeColumn(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGMissingColumn", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGMissingColumn", "FAIL")
          XML_add_txt("checkLvl1MISSINGMissingColumn", "ERROR: File contains issues in the missing column codes.")
        }
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl1MISSINGVisitCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                                      dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 10)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGVisitCodes/>")
        x <- identical(checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                       dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGVisitCodes", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGVisitCodes", "FAIL")
          XML_add_txt("checkLvl1MISSINGVisitCodes", "ERROR: File contains visit code issues.")
        }
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl1MISSINGSiteCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 11)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGSiteCodes/>")
        x <- identical(checkSiteCodes(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGSiteCodes", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGSiteCodes", "FAIL")
          XML_add_txt("checkLvl1MISSINGSiteCodes", "ERROR: File contains site code issues.")
        }
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl1MISSINGDateFormat & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkDateFormat(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 12)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGDateFormat/>")
        x <- identical(checkDateFormat(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGDateFormat", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGDateFormat", "FAIL")
          XML_add_txt("checkLvl1MISSINGDateFormat", "ERROR: File contains date format issues.")
        }
      }
      
      # "Date Range Check" checkbox.
      if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
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
          XML_add_child("MISSING","<checkLvl1MISSINGDateRange/>")
          x <- identical(checkDateRange(missingFilePath, participantIDDF, 
                                        basename(lvl1DirPath), dirName, study_name), character(0))
          if (x == TRUE){
            XML_add_attr("checkLvl1MISSINGDateRange", "PASS")
          }else{
            XML_add_attr("checkLvl1MISSINGDateRange", "FAIL")
            XML_add_txt("checkLvl1MISSINGDateRange", "ERROR: File contains date range issues.")
          }
        }
        
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 13)
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl1MISSINGMissingCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(missingDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 14)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGMissingCodes/>")
        x <- identical(checkMissingCodes(missingDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGMissingCodes", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGMissingCodes", "FAIL")
          XML_add_txt("checkLvl1MISSINGMissingCodes", "ERROR: File contains missing code issues.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl1MISSINGBlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(missingFilePath, dirName, 
                                                         "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 15)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGBlankCells/>")
        x <- identical(checkForBlankCells(missingFilePath, dirName, 
                                          "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGBlankCells", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGBlankCells", "FAIL")
          XML_add_txt("checkLvl1MISSINGBlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl1MISSINGCommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 16)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGCommas/>")
        x <- identical(checkForCommas(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGCommas", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGCommas", "FAIL")
          XML_add_txt("checkLvl1MISSINGCommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkLvl1MISSINGNumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 17)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGNumOfCharacters/>")
        x <- identical(checkNumOfCharacters(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGNumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGNumOfCharacters", "FAIL")
          XML_add_txt("checkLvl1MISSINGNumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl1MISSINGWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 18)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGWhiteSpaces", "FAIL")
          XML_add_txt("checkLvl1MISSINGWhiteSpaces", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl1MISSINGEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(missingDF, dirName, "MISSING.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar",
                          value = 100 / numOfCheckboxes * 19)
        # Log input
        XML_add_child("MISSING","<checkLvl1MISSINGEncapsulation/>")
        x <- identical(checkForEncapsulation(missingDF, dirName, "MISSING.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1MISSINGEncapsulation", "PASS")
        }else{
          XML_add_attr("checkLvl1MISSINGEncapsulation", "FAIL")
          XML_add_txt("checkLvl1MISSINGEncapsulation", "ERROR: File contains encapsulation issues.")
        }
      }
    
    
    return (flaggedMsgs)
    
    # For tabular MISSING ==========================================================================
    
  }else if (checkIfTab(lvl1DirPath) == "tabular"){

    # "Column Name Format Check" checkbox.
    if (input$checkTabularMISSINGColumnNameFormat & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                                          dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGColumnNameFormat/>")
      x <- identical(checkColumnNameFormat(missingDF, basename(lvl1DirPath),
                                           dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGColumnNameFormat", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGColumnNameFormat", "FAIL")
        XML_add_txt("checkTabularMISSINGColumnNameFormat", "ERROR: File contains incorrect column name format.")
      }
    }
    
    # "Subject IDs Verification" checkbox.
    if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
      flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(missingDF, participantIDDF, basename(lvl1DirPath),
                                                     dirName, "MISSING"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 5)
      # Log input
      XML_add_child("MISSING","<verifyTabularMISSINGSubjectIDs/>")
      x <- identical(verifySubjectIDs(missingDF, participantIDDF, basename(lvl1DirPath),
                                      dirName, "MISSING"), character(0))
      if (x == TRUE){
        XML_add_attr("verifyTabularMISSINGSubjectIDs", "PASS")
      }else{
        XML_add_attr("verifyTabularMISSINGSubjectIDs", "FAIL")
        XML_add_txt("verifyTabularMISSINGSubjectIDs", "ERROR: File contains subject ID issues.")
      }
    }
    
    # "Transfer IDs Check" checkbox.
    if (input$checkTabularMISSINGTransferIDs & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkForTransferIDs(missingDF, transferIDDF,
                                                        dirName, "MISSING"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 7)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGTransferIDs/>")
      x <- identical(checkForTransferIDs(missingDF, transferIDDF,
                                         dirName, "MISSING"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGTransferIDs", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGTransferIDs", "FAIL")
        XML_add_txt("checkTabularMISSINGTransferIDs", "ERROR: File contains transfer ID issues.")
      }
    }
    
    # "Column Names Comparison" checkbox. Only run algorithm if DATA file exists in level 1.
    if (input$compareTabularColumnNames & study_name != "Package only checks"){
      dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
      if (length(dataFileName) == 1){
        dataFilePath <- paste0(lvl1DirPath, "/", dataFileName)
        dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
        dataDF <- convertDataFrame(dataDF)
        
        flaggedMsgs <- c(flaggedMsgs, compareColumnNames(missingDF, dataDF, dirName, "DATA"))
        
        # Log input
        XML_add_child("MISSING","<compareTabularColumnNames/>")
        x <- identical(compareColumnNames(missingDF, dataDF, dirName, "DATA"), character(0))
        if (x == TRUE){
          XML_add_attr("compareTabularColumnNames", "PASS")
        }else{
          XML_add_attr("compareTabularColumnNames", "FAIL")
          XML_add_txt("compareTabularColumnNames", "ERROR: File Name comparison failed.")
        }
      }
      
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 8)
    }
    
    # "MISSING_CODE Column Check" checkbox.
    if (input$checkTabularMISSINGMissingColumn & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkMissingCodeColumn(missingDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 9)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGMissingColumn/>")
      x <- identical(checkMissingCodeColumn(missingDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGMissingColumn", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGMissingColumn", "FAIL")
        XML_add_txt("checkTabularMISSINGMissingColumn", "ERROR: File contains issues in the missing column codes.")
      }
    }
    
    # "Visit Codes Check" checkbox.
    if (input$checkTabularMISSINGVisitCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                                    dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 10)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGVisitCodes/>")
      x <- identical(checkVisitCodes(missingFilePath, basename(lvl1DirPath),
                                     dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGVisitCodes", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGVisitCodes", "FAIL")
        XML_add_txt("checkTabularMISSINGVisitCodes", "ERROR: File contains visit code issues.")
      }
    }
    
    # "Site Codes Check" checkbox.
    if (input$checkTabularMISSINGSiteCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(missingDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 11)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGSiteCodes/>")
      x <- identical(checkSiteCodes(missingDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGSiteCodes", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGSiteCodes", "FAIL")
        XML_add_txt("checkTabularMISSINGSiteCodes", "ERROR: File contains site code issues.")
      }
    }
    
    # "Date Format Check" checkbox.
    if (input$checkTabularMISSINGDateFormat & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkDateFormat(missingDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 12)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGDateFormat/>")
      x <- identical(checkDateFormat(missingDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGDateFormat", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGDateFormat", "FAIL")
        XML_add_txt("checkTabularMISSINGDateFormat", "ERROR: File contains date format issues.")
      }
    }
    
    # "Date Range Check" checkbox.
    if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
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
        XML_add_child("MISSING","<checkTabularMISSINGDateRange/>")
        x <- identical(checkDateRange(missingFilePath, participantIDDF, 
                                      basename(lvl1DirPath), dirName, study_name), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularMISSINGDateRange", "PASS")
        }else{
          XML_add_attr("checkTabularMISSINGDateRange", "FAIL")
          XML_add_txt("checkTabularMISSINGDateRange", "ERROR: File contains date range issues.")
        }
      }
      
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 13)
    }
    
    # "Missing Codes Check" checkbox.
    if (input$checkTabularMISSINGMissingCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(missingDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 14)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGMissingCodes/>")
      x <- identical(checkMissingCodes(missingDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGMissingCodes", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGMissingCodes", "FAIL")
        XML_add_txt("checkTabularMISSINGMissingCodes", "ERROR: File contains missing code issues.")
      }
    }
    
    # "Blank Cells Check" checkbox.
    if (input$checkTabularMISSINGBlankCells){
      flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(missingFilePath, dirName, 
                                                       "MISSING.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 15)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGBlankCells/>")
      x <- identical(checkForBlankCells(missingFilePath, dirName, 
                                        "MISSING.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGBlankCells", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGBlankCells", "FAIL")
        XML_add_txt("checkTabularMISSINGBlankCells", "ERROR: File contains blank cells.")
      }
    }
    
    # "Commas Check" checkbox.
    if (input$checkTabularMISSINGCommas){
      flaggedMsgs <- c(flaggedMsgs, checkForCommas(missingDF, dirName, "MISSING.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 16)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGCommas/>")
      x <- identical(checkForCommas(missingDF, dirName, "MISSING.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGCommas", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGCommas", "FAIL")
        XML_add_txt("checkTabularMISSINGCommas", "ERROR: File contains commas.")
      }
    }
    
    # "Number Of Characters Check" checkbox.
    if (input$checkTabularMISSINGNumOfCharacters){
      flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(missingDF, dirName, "MISSING.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 17)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGNumOfCharacters/>")
      x <- identical(checkNumOfCharacters(missingDF, dirName, "MISSING.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGNumOfCharacters", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGNumOfCharacters", "FAIL")
        XML_add_txt("checkTabularMISSINGNumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
      }
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkTabularMISSINGWhiteSpaces){
      flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(missingDF, dirName, "MISSING.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 18)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGWhiteSpaces/>")
      x <- identical(checkForWhiteSpaces(missingDF, dirName, "MISSING.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGWhiteSpaces", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGWhiteSpaces", "FAIL")
        XML_add_txt("checkTabularMISSINGWhiteSpaces", "ERROR: File contains too many charaters in cell(s).")
      }
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkTabularMISSINGEncapsulation){
      flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(missingDF, dirName, "MISSING.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar",
                        value = 100 / numOfCheckboxes * 19)
      # Log input
      XML_add_child("MISSING","<checkTabularMISSINGEncapsulation/>")
      x <- identical(checkForEncapsulation(missingDF, dirName, "MISSING.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularMISSINGEncapsulation", "PASS")
      }else{
        XML_add_attr("checkTabularMISSINGEncapsulation", "FAIL")
        XML_add_txt("checkTabularMISSINGEncapsulation", "ERROR: File contains encapsulation issues.")
      }
    }
  
  return (flaggedMsgs)
  }
  }
}

# [END]