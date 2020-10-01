# checkLvl1DICT.R
#
# Purpose: Create the server logic for level 1 DICT file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# =======================================================================================

checkLvl1DICT <- function(input, session, lvl1DirPath, numOfCheckboxes, study_name){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
    
  # IMPORTANT: RUN CHECK ONLY IF DICT FILE and DATA FILE EXIST FOR THIS PARTICULAR
  # LEVEL 1 DIRECTORY.
  dictFileName <- getFileName(lvl1DirPath, "DICT.csv")
  dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
  if (length(dictFileName) == 1 && length(dataFileName) == 1){
    dictFilePath <- paste0(lvl1DirPath, "/", dictFileName)
    dictDF <- read.csv(dictFilePath, stringsAsFactors = FALSE)
    dictDF <- convertDataFrame(dictDF)
    
    dataFilePath <- paste0(lvl1DirPath, "/", dataFileName)
    dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
    dataDF <- convertDataFrame(dataDF)
    
    # For lvl1 Dict ======================================================================
    # "Column Labels Check" checkbox.
    if (checkIfTab(lvl1DirPath) == "non-tabular"){
      if (input$checkLvl1DICTColumnLabels & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkColumnLabels(dictDF, dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        # Log input
        XML_add_child("DICT","<checkLvl1DICTColumnLabels/>")
        x <- identical(checkColumnLabels(dictDF, dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DICTColumnLabels", "PASS")
        }else{
          XML_add_attr("checkLvl1DICTColumnLabels", "FAIL")
          XML_add_txt("checkLvl1DICTColumnLabels", "ERROR: File contains incorrect column labels.")
        }
      }
      
      # "Data Types Check" checkbox.
      if (input$checkLvl1DICTDataTypes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkDataTypes(dictDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("DICT","<checkLvl1DICTDataTypes/>")
        x <- identical(checkDataTypes(dictDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DICTDataTypes", "PASS")
        }else{
          XML_add_attr("checkLvl1DICTDataTypes", "FAIL")
          XML_add_txt("checkLvl1DICTDataTypes", "ERROR: File contains incorrect data types.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl1DICTBlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(dictFilePath, dirName, 
                                                         "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("DICT","<checkLvl1DICTBlankCells/>")
        x <- identical(checkForBlankCells(dictFilePath, dirName, 
                                          "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DICTBlankCells", "PASS")
        }else{
          XML_add_attr("checkLvl1DICTBlankCells", "FAIL")
          XML_add_txt("checkLvl1DICTBlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl1DICTCommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 6)
        # Log input
        XML_add_child("DICT","<checkLvl1DICTCommas/>")
        x <- identical(checkForCommas(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DICTCommas", "PASS")
        }else{
          XML_add_attr("checkLvl1DICTCommas", "FAIL")
          XML_add_txt("checkLvl1DICTCommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkLvl1DICTNumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          
                          value = 100 / numOfCheckboxes * 7)
        # Log input
        XML_add_child("DICT","<checkLvl1DICTNumOfCharacters/>")
        x <- identical(checkNumOfCharacters(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DICTNumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkLvl1DICTNumOfCharacters", "FAIL")
          XML_add_txt("checkLvl1DICTNumOfCharacters", "ERROR: Too many charaters in cell(s)")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl1DICTWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("DICT","<checkLvl1DICTWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DICTWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkLvl1DICTWhiteSpaces", "FAIL")
          XML_add_txt("checkLvl1DICTWhiteSpaces", "ERROR: File contains whitespace.")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl1DICTEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar",
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("DICT","<checkLvl1DICTEncapsulation/>")
        x <- identical(checkForEncapsulation(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl1DICTEncapsulation", "PASS")
        }else{
          XML_add_attr("checkLvl1DICTEncapsulation", "FAIL")
          XML_add_txt("checkLvl1DICTEncapsulation", "ERROR: File has encapsulation issues.")
        }
      }
    
    return (flaggedMsgs)
  
  # For Tabular Dict ===============================================================================
    
  }else if(checkIfTab(lvl1DirPath) == "tabular"){
    # "Column Labels Check" checkbox.
      if (input$checkTabularDICTColumnLabels & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkColumnLabels(dictDF, dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        # Log input
        XML_add_child("DICT","<checkTabularDICTColumnLabels/>")
        x <- identical(checkColumnLabels(dictDF, dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDICTColumnLabels", "PASS")
        }else{
          XML_add_attr("checkTabularDICTColumnLabels", "FAIL")
          XML_add_txt("checkTabularDICTColumnLabels", "ERROR: File contains incorrect column labels.")
        }
        
      }
      
      # "Data Types Check" checkbox.
      if (input$checkTabularDICTDataTypes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkDataTypes(dictDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("DICT","<checkTabularDICTDataTypes/>")
        x <- identical(checkDataTypes(dictDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDICTDataTypes", "PASS")
        }else{
          XML_add_attr("checkTabularDICTDataTypes", "FAIL")
          XML_add_txt("checkTabularDICTDataTypes", "ERROR: File contains incorrect data types.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkTabularDICTBlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(dictFilePath, dirName, 
                                                         "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("DICT","<checkTabularDICTBlankCells/>")
        x <- identical(checkForBlankCells(dictFilePath, dirName, 
                                          "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDICTBlankCells", "PASS")
        }else{
          XML_add_attr("checkTabularDICTBlankCells", "FAIL")
          XML_add_txt("checkTabularDICTBlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkTabularDICTCommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 6)
        # Log input
        XML_add_child("DICT","<checkTabularDICTCommas/>")
        x <- identical(checkForCommas(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDICTCommas", "PASS")
        }else{
          XML_add_attr("checkTabularDICTCommas", "FAIL")
          XML_add_txt("checkTabularDICTCommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkTabularDICTNumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 7)
        # Log input
        XML_add_child("DICT","<checkTabularDICTNumOfCharacters/>")
        x <- identical(checkNumOfCharacters(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDICTNumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkTabularDICTNumOfCharacters", "FAIL")
          XML_add_txt("checkTabularDICTNumOfCharacters", "ERROR: Too many charaters in cell(s)")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkTabularDICTWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("DICT","<checkTabularDICTWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDICTWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkTabularDICTWhiteSpaces", "FAIL")
          XML_add_txt("checkTabularDICTWhiteSpaces", "ERROR: File contains whitespace.")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkTabularDICTEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar",
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("DICT","<checkTabularDICTEncapsulation/>")
        x <- identical(checkForEncapsulation(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkTabularDICTEncapsulation", "PASS")
        }else{
          XML_add_attr("checkTabularDICTEncapsulation", "FAIL")
          XML_add_txt("checkTabularDICTEncapsulation", "ERROR: File has encapsulation issues.")
        }
      }
    
    
    return (flaggedMsgs)
   
  } 
  }
}


# [END]