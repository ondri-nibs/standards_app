# checkLvl2DICT.R
#
# Purpose: Create the server logic for level 2 DICT file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# =======================================================================================

checkLvl2DICT <- function(input, session, lvl1DirPath, numOfCheckboxes, study_name){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a DICT file.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF DICT FILE and DATA FILE EXIST FOR THIS PARTICULAR 
    # LEVEL 2 DIRECTORY.
    dictFileName <- getFileName(lvl2DirPath, "DICT.csv")
    dataFileName <- getFileName(lvl2DirPath, "DATA.csv")
    if (length(dictFileName) == 1 && length(dataFileName) == 1){
      dictFilePath <- paste0(lvl2DirPath, "/", dictFileName)
      dictDF <- read.csv(dictFilePath, stringsAsFactors = FALSE)
      dictDF <- convertDataFrame(dictDF)
      
      dataFilePath <- paste0(lvl2DirPath, "/", dataFileName)
      dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
      dataDF <- convertDataFrame(dataDF)
      
      # "Column Labels Check" checkbox.
      if (input$checkLvl2DICTColumnLabels & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkColumnLabels(dictDF, dataDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        # Log input
        XML_add_child("DICT2","<checkLvl2DICTColumnLabels/>")
        x <- identical(checkColumnLabels(dictDF, dataDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DICTColumnLabels", "PASS")
        }else{
          XML_add_attr("checkLvl2DICTColumnLabels", "FAIL")
          XML_add_txt("checkLvl2DICTColumnLabels", "ERROR: File contains incorrect column labels.")
        }
      }
      
      # "Data Types Check" checkbox.
      if (input$checkLvl2DICTDataTypes & study_name != "Package only checks"){
        flaggedMsgs <- c(flaggedMsgs, checkDataTypes(dictDF, dirName))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("DICT2","<checkLvl2DICTDataTypes/>")
        x <- identical(checkDataTypes(dictDF, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DICTDataTypes", "PASS")
        }else{
          XML_add_attr("checkLvl2DICTDataTypes", "FAIL")
          XML_add_txt("checkLvl2DICTDataTypes", "ERROR: File contains incorrect data types.")
        }
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl2DICTBlankCells){
        flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(dictFilePath, dirName, 
                                                         "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 5)
        # Log input
        XML_add_child("DICT2","<checkLvl2DICTBlankCells/>")
        x <- identical(checkForBlankCells(dictFilePath, dirName, 
                                          "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DICTBlankCells", "PASS")
        }else{
          XML_add_attr("checkLvl2DICTBlankCells", "FAIL")
          XML_add_txt("checkLvl2DICTBlankCells", "ERROR: File contains blank cells.")
        }
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl2DICTCommas){
        flaggedMsgs <- c(flaggedMsgs, checkForCommas(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 6)
        # Log input
        XML_add_child("DICT2","<checkLvl2DICTCommas/>")
        x <- identical(checkForCommas(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DICTCommas", "PASS")
        }else{
          XML_add_attr("checkLvl2DICTCommas", "FAIL")
          XML_add_txt("checkLvl2DICTCommas", "ERROR: File contains commas.")
        }
      }
      
      # "Number Of Characters Check" checkbox.
      if (input$checkLvl2DICTNumOfCharacters){
        flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 7)
        # Log input
        XML_add_child("DICT2","<checkLvl2DICTNumOfCharacters/>")
        x <- identical(checkNumOfCharacters(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DICTNumOfCharacters", "PASS")
        }else{
          XML_add_attr("checkLvl2DICTNumOfCharacters", "FAIL")
          XML_add_txt("checkLvl2DICTNumOfCharacters", "ERROR: Too many charaters in cell(s)")
        }
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl2DICTWhiteSpaces){
        flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 8)
        # Log input
        XML_add_child("DICT2","<checkLvl2DICTWhiteSpaces/>")
        x <- identical(checkForWhiteSpaces(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DICTWhiteSpaces", "PASS")
        }else{
          XML_add_attr("checkLvl2DICTWhiteSpaces", "FAIL")
          XML_add_txt("checkLvl2DICTWhiteSpaces", "ERROR: File contains whitespace.")
        }
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl2DICTEncapsulation){
        flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(dictDF, dirName, "DICT.csv"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100 / numOfCheckboxes * 9)
        # Log input
        XML_add_child("DICT2","<checkLvl2DICTEncapsulation/>")
        x <- identical(checkForEncapsulation(dictDF, dirName, "DICT.csv"), character(0))
        if (x == TRUE){
          XML_add_attr("checkLvl2DICTEncapsulation", "PASS")
        }else{
          XML_add_attr("checkLvl2DICTEncapsulation", "FAIL")
          XML_add_txt("checkLvl2DICTEncapsulation", "ERROR: File has encapsulation issues.")
        }
      }
    }
  }
  
  return (flaggedMsgs)
}

# [END]