# checkLvl1README.R
#
# Purpose: Create the server logic for level 1 README file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# =======================================================================================

checkLvl1README <- function(input, session, lvl1DirPath, numOfCheckboxes,
                            lvl1RequiredFileTypes, study_name){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
  
  # We already know that the level 1 directory will have a README file
  # (as a result of required files check).
  readmeFilePath <- paste0(lvl1DirPath, "/", getFileName(lvl1DirPath, "README.csv"))
  readmeDF <- read.csv(readmeFilePath, stringsAsFactors = FALSE)
  readmeDF <- convertDataFrame(readmeDF)
  
  # Check if METHODS file exists in level 1 directory. This is not a hard/stopping check,
  # which is why it is in this function.
  flaggedMsgs <- c(flaggedMsgs, checkRequired(lvl1DirPath, dirName, lvl1RequiredFileTypes))
  # Log input
  XML_add_child("README", "<lvl1RequiredFileTypes/>")
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
    # Log input
    XML_add_attr("lvl1RequiredFileTypes", "FAIL")
    XML_add_txt("lvl1RequiredFileTypes", "ERROR: METHODS file does not exist in directory.")
  }else{
    # Log input
    XML_add_attr("lvl1RequiredFileTypes", "PASS")
  }
  
  # For lvl1 README =========================================================================
  if (checkIfTab(lvl1DirPath) == "non-tabular") {
    # "README File Names Comparison" checkbox.
    if (input$compareLvl1READMEFileNames & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, compareREADMEFileNames(readmeDF, lvl1DirPath, dirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 3)
      # Log input
      XML_add_child("README","<compareLvl1READMEFileNames/>")
      x <- identical(compareREADMEFileNames(readmeDF, lvl1DirPath, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("compareLvl1READMEFileNames", "PASS")
      }else{
        XML_add_attr("compareLvl1READMEFileNames", "FAIL")
        XML_add_txt("compareLvl1READMEFileNames", "ERROR: File Name comparison failed.")
      }
      
      
    }
    
    # "Blank Cells Check" checkbox.
    if (input$checkLvl1READMEBlankCells){
      flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(readmeFilePath, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
      # Log input
      XML_add_child("README","<checkLvl1READMEBlankCells/>")
      x <- identical(checkForBlankCells(readmeFilePath, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1READMEBlankCells", "PASS")
      }else{
        XML_add_attr("checkLvl1READMEBlankCells", "FAIL")
        XML_add_txt("checkLvl1READMEBlankCells", "ERROR: File contains blank cells.")
      }
    }
    
    # "Commas Check" checkbox.
    if (input$checkLvl1READMECommas){
      flaggedMsgs <- c(flaggedMsgs, checkForCommas(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 5)
      # Log input
      XML_add_child("README","<checkLvl1READMECommas/>")
      x <- identical(checkForCommas(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1READMECommas", "PASS")
      }else{
        XML_add_attr("checkLvl1READMECommas", "FAIL")
        XML_add_txt("checkLvl1READMECommas", "ERROR: File contains commas.")
      }
    }
    
    # "Number Of Characters Check" checkbox.
    if (input$checkLvl1READMENumOfCharacters){
      flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 6)
      # Log input
      XML_add_child("README","<checkLvl1READMENumOfCharacters/>")
      x <- identical(checkNumOfCharacters(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1READMENumOfCharacters", "PASS")
      }else{
        XML_add_attr("checkLvl1READMENumOfCharacters", "FAIL")
        XML_add_txt("checkLvl1READMENumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
      }
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkLvl1READMEWhiteSpaces){
      flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 7)
      # Log input
      XML_add_child("README","<checkLvl1READMEWhiteSpaces/>")
      x <- identical(checkForWhiteSpaces(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1READMEWhiteSpaces", "PASS")
      }else{
        XML_add_attr("checkLvl1READMEWhiteSpaces", "FAIL")
        XML_add_txt("checkLvl1READMEWhiteSpaces", "ERROR: File contains whitespace.")
      }
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkLvl1READMEEncapsulation){
      flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar",
                        value = 100 / numOfCheckboxes * 8)
      # Log input
      XML_add_child("README","<checkLvl1READMEEncapsulation/>")
      x <- identical(checkForEncapsulation(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1READMEEncapsulation", "PASS")
      }else{
        XML_add_attr("checkLvl1READMEEncapsulation", "FAIL")
        XML_add_txt("checkLvl1READMEEncapsulation", "ERROR: File has encapsulation issues.")
      }
    }
    
    return (flaggedMsgs)


  # For tabular README =========================================================================
} else if (checkIfTab(lvl1DirPath) == "tabular"){
  
  # "README File Names Comparison" checkbox.
  if (input$compareLvl1READMEFileNames & study_name != "Package only checks"){
    flaggedMsgs <- c(flaggedMsgs, compareREADMEFileNames(readmeDF, lvl1DirPath, dirName))
    updateProgressBar(session = session, 
                      id = "lvl1ProgressBar", 
                      value = 100 / numOfCheckboxes * 3)
    # Log input
    XML_add_child("README","<compareLvl1READMEFileNames/>")
    x <- identical(compareREADMEFileNames(readmeDF, lvl1DirPath, dirName), character(0))
    if (x == TRUE){
      XML_add_attr("compareLvl1READMEFileNames", "PASS")
    }else{
      XML_add_attr("compareLvl1READMEFileNames", "FAIL")
      XML_add_txt("compareLvl1READMEFileNames", "ERROR: File Name comparison failed.")
    }
  }
  
  # "Blank Cells Check" checkbox.
  if (input$checkLvl1READMEBlankCells){
    flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(readmeFilePath, dirName, "README.csv"))
    updateProgressBar(session = session, 
                      id = "lvl1ProgressBar", 
                      value = 100 / numOfCheckboxes * 4)
    # Log input
    XML_add_child("README","<checkLvl1READMEBlankCells/>")
    x <- identical(checkForBlankCells(readmeFilePath, dirName, "README.csv"), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl1READMEBlankCells", "PASS")
    }else{
      XML_add_attr("checkLvl1READMEBlankCells", "FAIL")
      XML_add_txt("checkLvl1READMEBlankCells", "ERROR: File contains blank cells.")
    }
  }
  
  # "Commas Check" checkbox.
  if (input$checkLvl1READMECommas){
    flaggedMsgs <- c(flaggedMsgs, checkForCommas(readmeDF, dirName, "README.csv"))
    updateProgressBar(session = session, 
                      id = "lvl1ProgressBar", 
                      value = 100 / numOfCheckboxes * 5)
    # Log input
    XML_add_child("README","<checkLvl1READMECommas/>")
    x <- identical(checkForCommas(readmeDF, dirName, "README.csv"), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl1READMECommas", "PASS")
    }else{
      XML_add_attr("checkLvl1READMECommas", "FAIL")
      XML_add_txt("checkLvl1READMECommas", "ERROR: File contains commas.")
    }
  }
  
  # "Number Of Characters Check" checkbox.
  if (input$checkLvl1READMENumOfCharacters){
    flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(readmeDF, dirName, "README.csv"))
    updateProgressBar(session = session, 
                      id = "lvl1ProgressBar", 
                      value = 100 / numOfCheckboxes * 6)
    # Log input
    XML_add_child("README","<checkLvl1READMENumOfCharacters/>")
    x <- identical(checkNumOfCharacters(readmeDF, dirName, "README.csv"), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl1READMENumOfCharacters", "PASS")
    }else{
      XML_add_attr("checkLvl1READMENumOfCharacters", "FAIL")
      XML_add_txt("checkLvl1READMENumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
    }
  }
  
  # "White Spaces Check" checkbox.
  if (input$checkLvl1READMEWhiteSpaces){
    flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(readmeDF, dirName, "README.csv"))
    updateProgressBar(session = session, 
                      id = "lvl1ProgressBar", 
                      value = 100 / numOfCheckboxes * 7)
    # Log input
    XML_add_child("README","<checkLvl1READMEWhiteSpaces/>")
    x <- identical(checkForWhiteSpaces(readmeDF, dirName, "README.csv"), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl1READMEWhiteSpaces", "PASS")
    }else{
      XML_add_attr("checkLvl1READMEWhiteSpaces", "FAIL")
      XML_add_txt("checkLvl1READMEWhiteSpaces", "ERROR: File contains whitespace.")
    }
  }
  
  # "Encapsulation Check" checkbox.
  if (input$checkLvl1READMEEncapsulation){
    flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(readmeDF, dirName, "README.csv"))
    updateProgressBar(session = session, 
                      id = "lvl1ProgressBar",
                      value = 100 / numOfCheckboxes * 8)
    # Log input
    XML_add_child("README","<checkLvl1READMEEncapsulation/>")
    x <- identical(checkForEncapsulation(readmeDF, dirName, "README.csv"), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl1READMEEncapsulation", "PASS")
    }else{
      XML_add_attr("checkLvl1READMEEncapsulation", "FAIL")
      XML_add_txt("checkLvl1READMEEncapsulation", "ERROR: File has encapsulation issues.")
    }
  }
  
  return (flaggedMsgs)
}
}
# [END]