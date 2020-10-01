# checkFILELIST.R
#
# Purpose: Create the server logic for all FILELIST file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# =======================================================================================

checkFILELIST <- function(input, session, lvl1DirPath, numOfCheckboxes, participantIDDF, transferIDDF, study_name, participantFile){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    lvl3DirPath <- paste0(lvl2DirPath, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    
    filelistFilePath <- paste0(lvl3DirPath, "/", getFileName(lvl3DirPath, "FILELIST.csv"))
    filelistDF <- read.csv(filelistFilePath, stringsAsFactors = FALSE)
    filelistDF <- convertDataFrame(filelistDF)
    
    # "Column Name Format Check" checkbox.
    if (input$checkFILELISTColumnNameFormat & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkColumnNameFormat(filelistDF, basename(lvl1DirPath),
                                                          lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTColumnNameFormat/>")
      x <- identical(checkColumnNameFormat(filelistDF, basename(lvl1DirPath),
                                           lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTColumnNameFormat", "PASS")
      }else{
        XML_add_attr("checkFILELISTColumnNameFormat", "FAIL")
        XML_add_txt("checkFILELISTColumnNameFormat", "ERROR: File contains incorrect column name format.")
      }
    }
    
    # "Subject IDs Verification" checkbox.
    if (input$verifyLvl3FILELISTSubjectIDs & participantFile == TRUE){
      flaggedMsgs <- c(flaggedMsgs, verifySubjectIDs(filelistDF, participantIDDF, 
                                                     basename(lvl1DirPath),
                                                     lvl3DirName, "FILELIST", lvl1DirPath, 
                                                     lvl2DirPath, lvl3DirPath))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 5)
      # Log input
      XML_add_child("FILELIST3","<verifyLvl3FILELISTSubjectIDs/>")
      x <- identical(verifySubjectIDs(filelistDF, participantIDDF, 
                                      basename(lvl1DirPath),
                                      lvl3DirName, "FILELIST", lvl1DirPath, 
                                      lvl2DirPath, lvl3DirPath), character(0))
      if (x == TRUE){
        XML_add_attr("verifyLvl3FILELISTSubjectIDs", "PASS")
      }else{
        XML_add_attr("verifyLvl3FILELISTSubjectIDs", "FAIL")
        XML_add_txt("verifyLvl3FILELISTSubjectIDs", "ERROR: File contains subject ID issues.")
      }
    }
    
    # "Subject IDs Comparison" checkbox.
    if (input$compareLvl3SubjectIDs & participantFile == TRUE){
      flaggedMsgs <- c(flaggedMsgs, compareLvl3SubjectIDs(filelistDF, lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 6)
      # Log input
      XML_add_child("FILELIST3","<compareLvl3SubjectIDs/>")
      x <- identical(compareLvl3SubjectIDs(filelistDF, lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("compareLvl3SubjectIDs", "PASS")
      }else{
        XML_add_attr("compareLvl3SubjectIDs", "FAIL")
        XML_add_txt("compareLvl3SubjectIDs", "ERROR: File contains subject ID comparison issues.")
      }
    }
    
    # "Participant File Names Comparison" checkbox.
    if (input$compareParticipantFileNames & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, compareParticipantFileNames(filelistDF, lvl3DirPath,
                                                                lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 8)
      # Log input
      XML_add_child("FILELIST3","<compareParticipantFileNames/>")
      x <- identical(compareParticipantFileNames(filelistDF, lvl3DirPath,
                                                 lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("compareParticipantFileNames", "PASS")
      }else{
        XML_add_attr("compareParticipantFileNames", "FAIL")
        XML_add_txt("compareParticipantFileNames", "ERROR: File contains participant file name issues.")
      }
    }
    
    # "ONDRI ID Verification" checkbox.
    if (input$verifyOndriID & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, verifyOndriID(lvl3DirPath, lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 9)
      # Log input
      XML_add_child("FILELIST3","<verifyOndriID/>")
      x <- identical(verifyOndriID(lvl3DirPath, lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("verifyOndriID", "PASS")
      }else{
        XML_add_attr("verifyOndriID", "FAIL")
        XML_add_txt("verifyOndriID", "ERROR: File contains Ondri ID issues.")
      }
      
    }

    # "Visit Codes Check" checkbox.
    if (input$checkFILELISTVisitCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkVisitCodes(filelistFilePath, basename(lvl1DirPath),
                                                    lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 10)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTVisitCodes/>")
      x <- identical(checkVisitCodes(filelistFilePath, basename(lvl1DirPath),
                                     lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTVisitCodes", "PASS")
      }else{
        XML_add_attr("checkFILELISTVisitCodes", "FAIL")
        XML_add_txt("checkFILELISTVisitCodes", "ERROR: File contains visit code issues.")
      }
    }
    
    # "Site Codes Check" checkbox.
    if (input$checkFILELISTSiteCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkSiteCodes(filelistDF, lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 11)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTSiteCodes/>")
      x <- identical(checkSiteCodes(filelistDF, lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTSiteCodes", "PASS")
      }else{
        XML_add_attr("checkFILELISTSiteCodes", "FAIL")
        XML_add_txt("checkFILELISTSiteCodes", "ERROR: File contains site code issues.")
      }
    }
    
    # "Date Format Check" checkbox.
    if (input$checkFILELISTDateFormat){
      flaggedMsgs <- c(flaggedMsgs, checkDateFormat(filelistDF, lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 12)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTDateFormat/>")
      x <- identical(checkDateFormat(filelistDF, lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTDateFormat", "PASS")
      }else{
        XML_add_attr("checkFILELISTDateFormat", "FAIL")
        XML_add_txt("checkFILELISTDateFormat", "ERROR: File contains date format issues.")
      }
    }
    
    # "Date Range Check" checkbox.
    if (input$checkFILELISTDateRange & participantFile == TRUE){
      # SPECIAL CASE: Only run date range check if subject IDs verification has passed.
      if (length(verifySubjectIDs(filelistDF, participantIDDF, basename(lvl1DirPath), lvl3DirName, 
                                  "FILELIST", lvl1DirPath, lvl2DirPath, lvl3DirPath)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "Date Range: Directory", lvl3DirName),
                      "- Cannot check date range as subject IDs verification did not pass.")
        flaggedMsgs <- c(flaggedMsgs, line)
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                       "<br/>"))
      }
      else{
        flaggedMsgs <- c(flaggedMsgs, checkDateRange(filelistFilePath, participantIDDF,
                                                     basename(lvl1DirPath), lvl3DirName, study_name))
        # Log input
        XML_add_child("FILESLIST3","<checkFILELISTDateRange/>")
        x <- identical(checkDateRange(filelistFilePath, participantIDDF,
                                      basename(lvl1DirPath), lvl3DirName, study_name), character(0))
        if (x == TRUE){
          XML_add_attr("checkFILELISTDateRange", "PASS")
        }else{
          XML_add_attr("checkFILELISTDateRange", "FAIL")
          XML_add_txt("checkFILELISTDateRange", "ERROR: File contains date range issues.")
        }
      }
      
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 13)
    }
    
    # "Missing Codes Check" checkbox.
    if (input$checkFILELISTMissingCodes & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkMissingCodes(filelistDF, lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 14)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTMissingCodes/>")
      x <- identical( checkMissingCodes(filelistDF, lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTMissingCodes", "PASS")
      }else{
        XML_add_attr("checkFILELISTMissingCodes", "FAIL")
        XML_add_txt("checkFILELISTMissingCodes", "ERROR: File contains missing code issues.")
      }
    }
    
    # "Blank Cells Check" checkbox.
    if (input$checkFILELISTBlankCells){
      flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(filelistFilePath, lvl3DirName,
                                                       "FILELIST.csv"))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 15)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTBlankCells/>")
      x <- identical( checkForBlankCells(filelistFilePath, lvl3DirName,
                                         "FILELIST.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTBlankCells", "PASS")
      }else{
        XML_add_attr("checkFILELISTBlankCells", "FAIL")
        XML_add_txt("checkFILELISTBlankCells", "ERROR: File contains blank cells.")
      }
    }
    
    # "Commas Check" checkbox.
    if (input$checkFILELISTCommas){
      flaggedMsgs <- c(flaggedMsgs, checkForCommas(filelistDF, lvl3DirName,
                                                   "FILELIST.csv"))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 16)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTCommas/>")
      x <- identical(checkForCommas(filelistDF, lvl3DirName,
                                    "FILELIST.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTCommas", "PASS")
      }else{
        XML_add_attr("checkFILELISTCommas", "FAIL")
        XML_add_txt("checkFILELISTCommas", "ERROR: File contains commas.")
      }
      
      
    }
    
    # "Number Of Characters Check" checkbox.
    if (input$checkFILELISTNumOfCharacters){
      flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(filelistDF, lvl3DirName,
                                                         "FILELIST.csv"))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 17)
      # Log input
      XML_add_child("FILELIST","<checkFILELISTNumOfCharacters/>")
      x <- identical(checkNumOfCharacters(filelistDF, lvl3DirName,
                                          "FILELIST.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTNumOfCharacters", "PASS")
      }else{
        XML_add_attr("checkFILELISTNumOfCharacters", "FAIL")
        XML_add_txt("checkFILELISTNumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
      }
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkFILELISTWhiteSpaces){
      flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(filelistDF, lvl3DirName,
                                                        "FILELIST.csv"))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 18)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTWhiteSpaces/>")
      x <- identical(checkForWhiteSpaces(filelistDF, lvl3DirName,
                                         "FILELIST.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTWhiteSpaces", "PASS")
      }else{
        XML_add_attr("checkFILELISTWhiteSpaces", "FAIL")
        XML_add_txt("checkFILELISTWhiteSpaces", "ERROR: File contains whitespace.")
      }
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkFILELISTEncapsulation){
      flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(filelistDF, lvl3DirName,
                                                          "FILELIST.csv"))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 19)
      # Log input
      XML_add_child("FILELIST3","<checkFILELISTEncapsulation/>")
      x <- identical(checkForEncapsulation(filelistDF, lvl3DirName,
                                           "FILELIST.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkFILELISTEncapsulation", "PASS")
      }else{
        XML_add_attr("checkFILELISTEncapsulation", "FAIL")
        XML_add_txt("checkFILELISTEncapsulation", "ERROR: File contains encapsulation issues.")
      }
    }
  }
  
  return (flaggedMsgs)
}

# [END]