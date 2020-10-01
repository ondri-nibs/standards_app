# checkLvl2DATA.R
#
# Purpose: Create the server logic for level 2 DATA file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# =======================================================================================

checkLvl2DATA <- function(input, session, lvl1DirPath, numOfCheckboxes, participantIDDF,
                          transferIDDF, study_name, participantFile){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a DATA file.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF DATA FILE EXISTS FOR THIS PARTICULAR LEVEL 2 DIRECTORY.
    dataFileName <- getFileName(lvl2DirPath, "DATA.csv")
    if (length(dataFileName) == 1){
      dataFilePath <- paste0(lvl2DirPath, "/", dataFileName)
      dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
      dataDF <- convertDataFrame(dataDF)
      
      # "Column Name Format Check" checkbox.
      if (input$checkLvl2DATAColumnNameFormat & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(dataDF, basename(lvl1DirPath),
                                                            dirName))
        updateProgressBar(session = session,
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATAColumnNameFormat/>")
        x <- identical(checkColumnNameFormat(dataDF, basename(lvl1DirPath),
                                             dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAColumnNameFormat", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAColumnNameFormat", "FAIL")
          XML_add_txt("checkLvl1DATAColumnNameFormat", "ERROR: File contains incorrect column name format.")
        }
      }
      
      # Create special DF for column name syntax check.
      specialDataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, check.names = FALSE)
      specialDataDF <- convertDataFrame(specialDataDF)
      # "Column Name Syntax Check" checkbox.
      if (input$checkLvl2DATAColumnNameSyntax){
        flaggedMsgs <- c(flaggedMsgs, checkColumnNameSyntax(specialDataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("DATA2","<checkLvl1DATAColumnNameSyntax/>")
        x <- identical(checkColumnNameSyntax(specialDataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAColumnNameSyntax", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAColumnNameSyntax", "FAIL")
          XML_add_txt("checkLvl2DATAColumnNameSyntax", "ERROR: File contains incorrect column name syntax.")
        }
      }
        
      # "Subject IDs Verification" checkbox.
      if (input$verifyLvl2DATASubjectIDs & participantFile == TRUE){
        flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(dataDF, participantIDDF, 
                                                       basename(lvl1DirPath), dirName, 
                                                       "DATA", lvl1DirPath, lvl2DirPath))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 6)
        # Log input
        XML_add_child("DATA2","<verifyLvl2DATASubjectIDs/>")
        x <- identical( verifySubjectIDs(dataDF, participantIDDF, 
                                         basename(lvl1DirPath), dirName, 
                                         "DATA", lvl1DirPath, lvl2DirPath), character(0))
        if (x == TRUE){
          XML_add_attr("verifyLvl2DATASubjectIDs", "PASS")
        }else{
          XML_add_attr("verifyLvl2DATASubjectIDs", "FAIL")
          XML_add_txt("verifyLvl2DATASubjectIDs", "ERROR: File contains subject ID issues.")
        }
      }
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl2DATATransferIDs & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkForTransferIDs(dataDF, transferIDDF, 
                                                          dirName, "DATA"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATATransferIDs/>")
        x <- identical(checkForTransferIDs(dataDF, transferIDDF, 
                                           dirName, "DATA"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATATransferIDs", "PASS")
        }else{
          XML_add_attr("checkLvl2DATATransferIDs", "FAIL")
          XML_add_txt("checkLvl2DATATransferIDs", "ERROR: File contains transfer ID issues.")
        }
      }
      
      # "Precision Levels Check" checkbox.
      if (input$checkLvl2DATAPrecisionLevels){
        flaggedMsgs <- c(flaggedMsgs, checkPrecisionLevels(dataFilePath, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATAPrecisionLevels/>")
        x <- identical(checkPrecisionLevels(dataFilePath, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAPrecisionLevels", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAPrecisionLevels", "FAIL")
          XML_add_txt("checkLvl2DATAPrecisionLevels", "ERROR: File contains incorrect precision levels.")
        }
      }
      
      # "Special Values Check" checkbox.
      if (input$checkLvl2DATASpecialValues){
        flaggedMsgs <- c(flaggedMsgs, checkForSpecialValues(dataFilePath, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 10)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATASpecialValues/>")
        x <- identical(checkForSpecialValues(dataFilePath, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATASpecialValues", "PASS")
        }else{
          XML_add_attr("checkLvl2DATASpecialValues", "FAIL")
          XML_add_txt("checkLvl2DATASpecialValues", "ERROR: File contains special characters.")
        }
      }
      
      # "Missing Code Rows Check" checkbox.
      if (input$checkLvl2DATAMissingCodeRows & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkForMissingCodeRows(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 11)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATAMissingCodeRows/>")
        x <- identical(checkForMissingCodeRows(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAMissingCodeRows", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAMissingCodeRows", "FAIL")
          XML_add_txt("checkLvl2DATAMissingCodeRows", "ERROR: File contains missing code row issues.")
        }
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl2DATAVisitCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(dataFilePath, basename(lvl1DirPath),
                                                      dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 12)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATAVisitCodes/>")
        x <- identical(checkVisitCodes(dataFilePath, basename(lvl1DirPath),
                                       dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAVisitCodes", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAVisitCodes", "FAIL")
          XML_add_txt("checkLvl2DATAVisitCodes", "ERROR: File contains visit code issues.")
        }
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl2DATASiteCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 13)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATASiteCodes/>")
        x <- identical(checkSiteCodes(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATASiteCodes", "PASS")
        }else{
          XML_add_attr("checkLvl2DATASiteCodes", "FAIL")
          XML_add_txt("checkLvl2DATASiteCodes", "ERROR: File contains site code issues.")
        }
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl2DATADateFormat){
        flaggedMsgs <- c(flaggedMsgs, checkDateFormat(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 14)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATADateFormat/>")
        x <- identical(checkDateFormat(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATADateFormat", "PASS")
        }else{
          XML_add_attr("checkLvl2DATADateFormat", "FAIL")
          XML_add_txt("checkLvl2DATADateFormat", "ERROR: File contains date format issues.")
        }
      }
      
      # "Date Range Check" checkbox.
      if (input$checkLvl2DATADateRange & participantFile == TRUE){
        # SPECIAL CASE: Only run date range check if subject IDs verification has passed.
        if (length(verifySubjectIDs(dataDF, participantIDDF, basename(lvl1DirPath), 
                                    dirName, "DATA", lvl1DirPath, lvl2DirPath)) != 0){
          line <- paste(tags$span(class = "bold-category", 
                                  "Date Range: Directory", dirName),
                        "- Cannot check date range as subject IDs verification did not pass.")
          flaggedMsgs <- c(flaggedMsgs, line)
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                         "<br/>"))
        }
        else{
          flaggedMsgs <- c(flaggedMsgs, checkDateRange(dataFilePath, participantIDDF, 
                                                       basename(lvl1DirPath), dirName))
          # Log input
          XML_add_child("DATA2","<checkLvl2DATADateRange/>")
          x <- identical(checkDateRange(dataFilePath, participantIDDF, 
                                        basename(lvl1DirPath)), character(0))
          if (x == TRUE){
            XML_add_attr("checkLvl2DATADateRange", "PASS")
          }else{
            XML_add_attr("checkLvl2DATADateRange", "FAIL")
            XML_add_txt("checkLvl2DATADateRange", "ERROR: File contains date range issues.")
          }
        }
        
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 15)
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl2DATAMissingCodes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 16)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATAMissingCodes/>")
        x <- identical(checkMissingCodes(dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAMissingCodes", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAMissingCodes", "FAIL")
          XML_add_txt("checkLvl2DATAMissingCodes", "ERROR: File contains missing code issues.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl2DATABlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(dataFilePath, dirName, 
                                                         "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 17)
        # Log input
        XML_add_child("DATA2","<checkLvl1DATABlankCells/>")
        x <- identical(checkForBlankCells(dataFilePath, dirName, 
                                          "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATABlankCells", "PASS")
        }else{
          XML_add_attr("checkLvl2DATABlankCells", "FAIL")
          XML_add_txt("checkLvl2DATABlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl2DATACommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 18)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATACommas/>")
        x <- identical(checkForCommas(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATACommas", "PASS")
        }else{
          XML_add_attr("checkLvl2DATACommas", "FAIL")
          XML_add_txt("checkLvl2DATACommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkLvl2DATANumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 19)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATANumOfCharacters/>")
        x <- identical(checkNumOfCharacters(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATANumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkLvl2DATANumOfCharacters", "FAIL")
          XML_add_txt("checkLvl2DATANumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl2DATAWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 20)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATAWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAWhiteSpaces", "FAIL")
          XML_add_txt("checkLvl2DATAWhiteSpaces", "ERROR: File contains whitespace.")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl2DATAEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(dataDF, dirName, "DATA.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100 / numOfCheckboxes * 21)
        # Log input
        XML_add_child("DATA2","<checkLvl2DATAEncapsulation/>")
        x <- identical(checkForEncapsulation(dataDF, dirName, "DATA.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DATAEncapsulation", "PASS")
        }else{
          XML_add_attr("checkLvl2DATAEncapsulation", "FAIL")
          XML_add_txt("checkLvl2DATAEncapsulation", "ERROR: File contains encapsulation issues.")
        }
      }
    }
  }
  
  return (flaggedMsgs)
}

# [END]