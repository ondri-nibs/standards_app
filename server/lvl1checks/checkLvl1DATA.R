# checkLvl1DATA.R
#
# Purpose: Create the server logic for level 1 DATA file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# =======================================================================================


checkLvl1DATA <- function(input, session, lvl1DirPath, numOfCheckboxes, participantIDDF,
                          transferIDDF, study_name, participantFile){

  
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
  
  # IMPORTANT: RUN CHECK ONLY IF DATA FILE EXISTS FOR THIS PARTICULAR LEVEL 1 DIRECTORY.
  dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
  if (length(dataFileName) == 1){
    dataFilePath <- paste0(lvl1DirPath, "/", dataFileName)
    dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
    dataDF <- convertDataFrame(dataDF)
    
    # For lvl1 DATA ======================================================================
    if(checkIfTab(lvl1DirPath) == "non-tabular"){
      
      # "Column Name Format Check" checkbox.
      if (input$checkLvl1DATAColumnNameFormat & study_name != "Package only checks"){
   
        flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(dataDF, basename(lvl1DirPath),
                                                            dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAColumnNameFormat/>")
        x <- identical(checkColumnNameFormat(dataDF, basename(lvl1DirPath),
                                             dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAColumnNameFormat", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAColumnNameFormat", "FAIL")
          XML_add_txt("checkLvl1DATAColumnNameFormat", "ERROR: File contains incorrect column name format.")
        }
        
      }
      
      # Create special DF for column name syntax check.
      specialDataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, check.names = FALSE)
      specialDataDF <- convertDataFrame(specialDataDF)
      # "Column Name Syntax Check" checkbox.
      
      if (input$checkLvl1DATAColumnNameSyntax){
        flaggedMsgs <- c(flaggedMsgs, checkColumnNameSyntax(specialDataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAColumnNameSyntax/>")
        x <- identical(checkColumnNameSyntax(specialDataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAColumnNameSyntax", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAColumnNameSyntax", "FAIL")
          XML_add_txt("checkLvl1DATAColumnNameSyntax", "ERROR: File contains incorrect column name syntax.")
        }
      }
      
      # "Subject IDs Verification" checkbox.
      if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
        flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(dataDF, participantIDDF,
                                                       basename(lvl1DirPath), 
                                                       dirName, "DATA", lvl1DirPath))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 6)
        # Log input
        XML_add_child("DATA","<verifyLvl1DATASubjectIDs/>")
        x <- identical(verifySubjectIDs(dataDF, participantIDDF,
                                        basename(lvl1DirPath), 
                                        dirName, "DATA", lvl1DirPath), character(0))
        if (x == TRUE){
          XML_add_attr("verifyLvl1DATASubjectIDs", "PASS")
        }else{
          XML_add_attr("verifyLvl1DATASubjectIDs", "FAIL")
          XML_add_txt("verifyLvl1DATASubjectIDs", "ERROR: File contains subject ID issues.")
        }
      }
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl1DATATransferIDs & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkForTransferIDs(dataDF, transferIDDF, 
                                                          dirName, "DATA"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("DATA","<checkLvl1DATATransferIDs/>")
        x <- identical(checkForTransferIDs(dataDF, transferIDDF, 
                                           dirName, "DATA"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATATransferIDs", "PASS")
        }else{
          XML_add_attr("checkLvl1DATATransferIDs", "FAIL")
          XML_add_txt("checkLvl1DATATransferIDs", "ERROR: File contains transfer ID issues.")
        }
      }
      
      # "Precision Levels Check" checkbox.
      if (input$checkLvl1DATAPrecisionLevels){
        flaggedMsgs <- c(flaggedMsgs, checkPrecisionLevels(dataFilePath, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAPrecisionLevels/>")
        x <- identical(checkPrecisionLevels(dataFilePath, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAPrecisionLevels", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAPrecisionLevels", "FAIL")
          XML_add_txt("checkLvl1DATAPrecisionLevels", "ERROR: File contains incorrect precision levels.")
        }
      }
      
      # "Special Values Check" checkbox.
      if (input$checkLvl1DATASpecialValues){
        flaggedMsgs <- c(flaggedMsgs, checkForSpecialValues(dataFilePath, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 10)
        # Log input
        XML_add_child("DATA","<checkLvl1DATASpecialValues/>")
        x <- identical(checkForSpecialValues(dataFilePath, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATASpecialValues", "PASS")
        }else{
          XML_add_attr("checkLvl1DATASpecialValues", "FAIL")
          XML_add_txt("checkLvl1DATASpecialValues", "ERROR: File contains special characters.")
        }
      }
      
      # "Missing Code Rows Check" checkbox.
      if (input$checkLvl1DATAMissingCodeRows& study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkForMissingCodeRows(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 11)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAMissingCodeRows/>")
        x <- identical(checkForMissingCodeRows(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAMissingCodeRows", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAMissingCodeRows", "FAIL")
          XML_add_txt("checkLvl1DATAMissingCodeRows", "ERROR: File contains missing code row issues.")
        }
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl1DATAVisitCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(dataFilePath, basename(lvl1DirPath),
                                                      dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 12)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAVisitCodes/>")
        x <- identical(checkVisitCodes(dataFilePath, basename(lvl1DirPath),
                                       dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAVisitCodes", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAVisitCodes", "FAIL")
          XML_add_txt("checkLvl1DATAVisitCodes", "ERROR: File contains visit code issues.")
        }
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl1DATASiteCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 13)
        # Log input
        XML_add_child("DATA","<checkLvl1DATASiteCodes/>")
        x <- identical(checkSiteCodes(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATASiteCodes", "PASS")
        }else{
          XML_add_attr("checkLvl1DATASiteCodes", "FAIL")
          XML_add_txt("checkLvl1DATASiteCodes", "ERROR: File contains site code issues.")
        }
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl1DATADateFormat){
        flaggedMsgs <- c(flaggedMsgs, checkDateFormat(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 14)
        # Log input
        XML_add_child("DATA","<checkLvl1DATADateFormat/>")
        x <- identical(checkDateFormat(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATADateFormat", "PASS")
        }else{
          XML_add_attr("checkLvl1DATADateFormat", "FAIL")
          XML_add_txt("checkLvl1DATADateFormat", "ERROR: File contains date format issues.")
        }
      }
      
      # "Date Range Check" checkbox.
      if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
        # SPECIAL CASE: Only run date range check if subject IDs verification has passed.
        if (length(verifySubjectIDs(dataDF, participantIDDF, basename(lvl1DirPath), 
                                    dirName, "DATA", lvl1DirPath)) != 0){
          line <- paste(tags$span(class = "bold-category", 
                                  "Date Range: Directory", dirName),
                        "- Cannot check date range as subject IDs verification did not pass.")
          flaggedMsgs <- c(flaggedMsgs, line)
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                         "<br/>"))
        }
        else{
          flaggedMsgs <- c(flaggedMsgs, checkDateRange(dataFilePath, participantIDDF, 
                                                       basename(lvl1DirPath), dirName, study_name))
          # Log input
          XML_add_child("DATA","<checkLvl1DATADateRange/>")
          x <- identical(checkDateRange(dataFilePath, participantIDDF, 
                                        basename(lvl1DirPath), dirName, study_name), character(0))
          if (x == TRUE){
            XML_add_attr("checkLvl1DATADateRange", "PASS")
          }else{
            XML_add_attr("checkLvl1DATADateRange", "FAIL")
            XML_add_txt("checkLvl1DATADateRange", "ERROR: File contains date range issues.")
          }
        }
        
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 15)
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl1DATAMissingCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 16)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAMissingCodes/>")
        x <- identical(checkMissingCodes(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAMissingCodes", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAMissingCodes", "FAIL")
          XML_add_txt("checkLvl1DATAMissingCodes", "ERROR: File contains missing code issues.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl1DATABlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(dataFilePath, dirName, 
                                                         "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 17)
        # Log input
        XML_add_child("DATA","<checkLvl1DATABlankCells/>")
        x <- identical(checkForBlankCells(dataFilePath, dirName, 
                                          "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATABlankCells", "PASS")
        }else{
          XML_add_attr("checkLvl1DATABlankCells", "FAIL")
          XML_add_txt("checkLvl1DATABlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl1DATACommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 18)
        # Log input
        XML_add_child("DATA","<checkLvl1DATACommas/>")
        x <- identical(checkForCommas(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATACommas", "PASS")
        }else{
          XML_add_attr("checkLvl1DATACommas", "FAIL")
          XML_add_txt("checkLvl1DATACommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkLvl1DATANumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 19)
        # Log input
        XML_add_child("DATA","<checkLvl1DATANumOfCharacters/>")
        x <- identical(checkNumOfCharacters(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATANumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkLvl1DATANumOfCharacters", "FAIL")
          XML_add_txt("checkLvl1DATANumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl1DATAWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 20)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAWhiteSpaces", "FAIL")
          XML_add_txt("checkLvl1DATAWhiteSpaces", "ERROR: File contains whitespace.")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl1DATAEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar",
                          value = 100 / numOfCheckboxes * 21)
        # Log input
        XML_add_child("DATA","<checkLvl1DATAEncapsulation/>")
        x <- identical(checkForEncapsulation(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DATAEncapsulation", "PASS")
        }else{
          XML_add_attr("checkLvl1DATAEncapsulation", "FAIL")
          XML_add_txt("checkLvl1DATAEncapsulation", "ERROR: File contains encapsulation issues.")
        }
      }
    
    
    return (flaggedMsgs)
  
    # For tabular DATA ==================================================================================
  }else if (checkIfTab(lvl1DirPath) == "tabular"){
    # "Column Name Format Check" checkbox.
    if (input$checkTabularDATAColumnNameFormat & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(dataDF, basename(lvl1DirPath),
                                                          dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
      # Log input
      XML_add_child("DATA","<checkTabularDATAColumnNameFormat/>")
      x <- identical(checkColumnNameFormat(dataDF, basename(lvl1DirPath),
                                           dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATAColumnNameFormat", "PASS")
      }else{
        XML_add_attr("checkTabularDATAColumnNameFormat", "FAIL")
        XML_add_txt("checkTabularDATAColumnNameFormat", "ERROR: File contains incorrect column name format.")
      }
    }
    
    # Create special DF for column name syntax check.
    specialDataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, check.names = FALSE)
    specialDataDF <- convertDataFrame(specialDataDF)
    # "Column Name Syntax Check" checkbox.
    
    if (input$checkTabularDATAColumnNameSyntax){
      flaggedMsgs <- c(flaggedMsgs, checkColumnNameSyntax(specialDataDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 5)
      # Log input
      XML_add_child("DATA","<checkTabularDATAColumnNameSyntax/>")
      x <- identical(checkColumnNameSyntax(specialDataDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATAColumnNameSyntax", "PASS")
      }else{
        XML_add_attr("checkTabularDATAColumnNameSyntax", "FAIL")
        XML_add_txt("checkTabularDATAColumnNameSyntax", "ERROR: File contains incorrect column name syntax.")
      }
    }
    
    # "Subject IDs Verification" checkbox.
    if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
      flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(dataDF, participantIDDF,
                                                     basename(lvl1DirPath), 
                                                     dirName, "DATA", lvl1DirPath))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 6)
      # Log input
      XML_add_child("DATA","<verifyTabularDATASubjectIDs/>")
      x <- identical(verifySubjectIDs(dataDF, participantIDDF,
                                      basename(lvl1DirPath), 
                                      dirName, "DATA", lvl1DirPath), character(0))
      if (x == TRUE){
        XML_add_attr("verifyTabularDATASubjectIDs", "PASS")
      }else{
        XML_add_attr("verifyTabularDATASubjectIDs", "FAIL")
        XML_add_txt("verifyTabularDATASubjectIDs", "ERROR: File contains subject ID issues.")
      } 
    }
    
    # "Transfer IDs Check" checkbox.
    if (input$checkTabularDATATransferIDs & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkForTransferIDs(dataDF, transferIDDF, 
                                                        dirName, "DATA"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 8)
      # Log input
      XML_add_child("DATA","<checkTabularDATATransferIDs/>")
      x <- identical(checkForTransferIDs(dataDF, transferIDDF, 
                                         dirName, "DATA"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATATransferIDs", "PASS")
      }else{
        XML_add_attr("checkTabularDATATransferIDs", "FAIL")
        XML_add_txt("checkTabularDATATransferIDs", "ERROR: File contains transfer ID issues.")
      }
    }
    
    # "Precision Levels Check" checkbox.
    if (input$checkTabularDATAPrecisionLevels){
      flaggedMsgs <- c(flaggedMsgs, checkPrecisionLevels(dataFilePath, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 9)
      # Log input
      XML_add_child("DATA","<checkTabularDATAPrecisionLevels/>")
      x <- identical(checkPrecisionLevels(dataFilePath, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATAPrecisionLevels", "PASS")
      }else{
        XML_add_attr("checkTabularDATAPrecisionLevels", "FAIL")
        XML_add_txt("checkTabularDATAPrecisionLevels", "ERROR: File contains incorrect precision levels.")
      }
    }
    
    # "Special Values Check" checkbox.
    if (input$checkTabularDATASpecialValues){
      flaggedMsgs <- c(flaggedMsgs, checkForSpecialValues(dataFilePath, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 10)
      # Log input
      XML_add_child("DATA","<checkTabularDATASpecialValues/>")
      x <- identical(checkForSpecialValues(dataFilePath, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATASpecialValues", "PASS")
      }else{
        XML_add_attr("checkTabularDATASpecialValues", "FAIL")
        XML_add_txt("checkTabularDATASpecialValues", "ERROR: File contains special characters.")
      }
    }
    
    # "Missing Code Rows Check" checkbox.
    if (input$checkTabularDATAMissingCodeRows & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkForMissingCodeRows(dataDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 11)
      # Log input
      XML_add_child("DATA","<checkForMissingCodeRows/>")
      x <- identical(checkForMissingCodeRows(dataDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkForMissingCodeRows", "PASS")
      }else{
        XML_add_attr("checkForMissingCodeRows", "FAIL")
        XML_add_txt("checkForMissingCodeRows", "ERROR: File contains missing code row issues.")
      }
    }
    
    # "Visit Codes Check" checkbox.
    if (input$checkTabularDATAVisitCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(dataFilePath, basename(lvl1DirPath),
                                                    dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 12)
      # Log input
      XML_add_child("DATA","<checkTabularDATAVisitCodes/>")
      x <- identical(checkVisitCodes(dataFilePath, basename(lvl1DirPath),
                                     dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATAVisitCodes", "PASS")
      }else{
        XML_add_attr("checkTabularDATAVisitCodes", "FAIL")
        XML_add_txt("checkTabularDATAVisitCodes", "ERROR: File contains visit code issues.")
      }
    }
    
    # "Site Codes Check" checkbox.
    if (input$checkTabularDATASiteCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(dataDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 13)
      # Log input
      XML_add_child("DATA","<checkTabularDATASiteCodes/>")
      x <- identical(checkSiteCodes(dataDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATASiteCodes", "PASS")
      }else{
        XML_add_attr("checkTabularDATASiteCodes", "FAIL")
        XML_add_txt("checkTabularDATASiteCodes", "ERROR: File contains site code issues.")
      }
    }
    
    # "Date Format Check" checkbox.
    if (input$checkTabularDATADateFormat){
      flaggedMsgs <- c(flaggedMsgs, checkDateFormat(dataDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 14)
      # Log input
      XML_add_child("DATA","<checkTabularDATADateFormat/>")
      x <- identical(checkDateFormat(dataDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATADateFormat", "PASS")
      }else{
        XML_add_attr("checkTabularDATADateFormat", "FAIL")
        XML_add_txt("checkTabularDATADateFormat", "ERROR: File contains date format issues.")
      }
    }
    
    # "Date Range Check" checkbox.
    if (input$verifyLvl1MISSINGSubjectIDs & participantFile == TRUE){
      # SPECIAL CASE: Only run date range check if subject IDs verification has passed.
      if (length(verifySubjectIDs(dataDF, participantIDDF, basename(lvl1DirPath), 
                                  dirName, "DATA", lvl1DirPath)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "Date Range: Directory", dirName),
                      "- Cannot check date range as subject IDs verification did not pass.")
        flaggedMsgs <- c(flaggedMsgs, line)
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                       "<br/>"))
      }
      else{
        flaggedMsgs <- c(flaggedMsgs, checkDateRange(dataFilePath, participantIDDF, 
                                                     basename(lvl1DirPath), dirName, study_name))
        # Log input
        XML_add_child("DATA","<checkTabularDATADateRange/>")
        x <- identical(checkDateRange(dataFilePath, participantIDDF, 
                                      basename(lvl1DirPath), dirName, study_name), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDATADateRange", "PASS")
        }else{
          XML_add_attr("checkTabularDATADateRange", "FAIL")
          XML_add_txt("checkTabularDATADateRange", "ERROR: File contains date range issues.")
        }
      }
      
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 15)
      
    }
    
    # "Missing Codes Check" checkbox.
    if (input$checkTabularDATAMissingCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(dataDF, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 16)
      # Log input
      XML_add_child("DATA","<checkTabularDATAMissingCodes/>")
      x <- identical(checkMissingCodes(dataDF, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATAMissingCodes", "PASS")
      }else{
        XML_add_attr("checkTabularDATAMissingCodes", "FAIL")
        XML_add_txt("checkTabularDATAMissingCodes", "ERROR: File contains missing code issues.")
      }
    }
    
    # "Blank Cells Check" checkbox.
    if (input$checkTabularDATABlankCells){
      flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(dataFilePath, dirName, 
                                                       "DATA.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 17)
      # Log input
      XML_add_child("DATA","<checkTabularDATABlankCells/>")
      x <- identical(checkForBlankCells(dataFilePath, dirName, 
                                        "DATA.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATABlankCells", "PASS")
      }else{
        XML_add_attr("checkTabularDATABlankCells", "FAIL")
        XML_add_txt("checkTabularDATABlankCells", "ERROR: File contains blank cells.")
      }
    }
    
    # "Commas Check" checkbox.
    if (input$checkTabularDATACommas){
      flaggedMsgs <- c(flaggedMsgs, checkForCommas(dataDF, dirName, "DATA.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 18)
      # Log input
      XML_add_child("DATA","<checkTabularDATACommas/>")
      x <- identical(checkForCommas(dataDF, dirName, "DATA.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATACommas", "PASS")
      }else{
        XML_add_attr("checkTabularDATACommas", "FAIL")
        XML_add_txt("checkTabularDATACommas", "ERROR: File contains commas.")
      }
    }
    
    # "Number Of Characters Check" checkbox.
    if (input$checkTabularDATANumOfCharacters){
      flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(dataDF, dirName, "DATA.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 19)
      # Log input
      XML_add_child("DATA","<checkTabularDATANumOfCharacters/>")
      x <- identical(checkNumOfCharacters(dataDF, dirName, "DATA.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATANumOfCharacters", "PASS")
      }else{
        XML_add_attr("checkTabularDATANumOfCharacters", "FAIL")
        XML_add_txt("checkTabularDATANumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
      }
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkTabularDATAWhiteSpaces){
      flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(dataDF, dirName, "DATA.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 20)
      # Log input
      XML_add_child("DATA","<checkTabularDATAWhiteSpaces/>")
      x <- identical(checkForWhiteSpaces(dataDF, dirName, "DATA.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATAWhiteSpaces", "PASS")
      }else{
        XML_add_attr("checkTabularDATAWhiteSpaces", "FAIL")
        XML_add_txt("checkTabularDATAWhiteSpaces", "ERROR: File contains whitespace.")
      }
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkTabularDATAEncapsulation){
      flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(dataDF, dirName, "DATA.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar",
                        value = 100 / numOfCheckboxes * 21)
      # Log input
      XML_add_child("DATA","<checkTabularDATAEncapsulation/>")
      x <- identical(checkForEncapsulation(dataDF, dirName, "DATA.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDATAEncapsulation", "PASS")
      }else{
        XML_add_attr("checkTabularDATAEncapsulation", "FAIL")
        XML_add_txt("checkTabularDATAEncapsulation", "ERROR: File contains encapsulation issues.")
      }
    }
  
  
  return (flaggedMsgs)
  }
  }
}


# [END]