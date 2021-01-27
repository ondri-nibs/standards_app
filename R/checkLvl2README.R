# checkLvl2README.R
#
# Purpose: Create the server logic for level 2 README file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# =======================================================================================

checkLvl2README <- function(input, session, lvl1DirPath, numOfCheckboxes, study_name){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory. We already know that each directory will have
  # a README file (as a result of required files check).
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    readmeFilePath <- paste0(lvl2DirPath, "/", getFileName(lvl2DirPath, "README.csv"))
    readmeDF <- read.csv(readmeFilePath, stringsAsFactors = FALSE)
    readmeDF <- convertDataFrame(readmeDF)

    
    # "README File Names Comparison" checkbox.
    if (input$compareLvl2READMEFileNames & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, compareREADMEFileNames(readmeDF, lvl2DirPath, dirName))
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 100 / numOfCheckboxes * 3)
      # Log input
      XML_add_child("README2","<compareLvl2READMEFileNames/>")
      x <- identical(compareREADMEFileNames(readmeDF, lvl2DirPath, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("compareLvl2READMEFileNames", "PASS")
      }else{
        XML_add_attr("compareLvl2READMEFileNames", "FAIL")
        XML_add_txt("compareLvl2READMEFileNames", "ERROR: File Name comparison failed.")
      }
    }
    
    # "Blank Cells Check" checkbox.
    if (input$checkLvl2READMEBlankCells){
      flaggedMsgs <- c(flaggedMsgs, checkForBlankCells(readmeFilePath, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
      # Log input
      XML_add_child("README2","<checkLvl2READMEBlankCells/>")
      x <- identical(checkForBlankCells(readmeFilePath, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl2READMEBlankCells", "PASS")
      }else{
        XML_add_attr("checkLvl2READMEBlankCells", "FAIL")
        XML_add_txt("checkLvl2READMEBlankCells", "ERROR: File contains blank cells.")
      }
    }
    
    # "Commas Check" checkbox.
    if (input$checkLvl2READMECommas){
      flaggedMsgs <- c(flaggedMsgs, checkForCommas(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 100 / numOfCheckboxes * 5)
      # Log input
      XML_add_child("README2","<checkLvl2READMECommas/>")
      x <- identical(checkForCommas(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl2READMECommas", "PASS")
      }else{
        XML_add_attr("checkLvl2READMECommas", "FAIL")
        XML_add_txt("checkLvl2READMECommas", "ERROR: File contains commas.")
      }
    }
    
    # "Number Of Characters Check" checkbox.
    if (input$checkLvl2READMENumOfCharacters){
      flaggedMsgs <- c(flaggedMsgs, checkNumOfCharacters(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 100 / numOfCheckboxes * 6)
      # Log input
      XML_add_child("README2","<checkLvl2READMENumOfCharacters/>")
      x <- identical(checkNumOfCharacters(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl2READMENumOfCharacters", "PASS")
      }else{
        XML_add_attr("checkLvl2READMENumOfCharacters", "FAIL")
        XML_add_txt("checkLvl2READMENumOfCharacters", "ERROR: File contains too many charaters in cell(s).")
      }
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkLvl2READMEWhiteSpaces){
      flaggedMsgs <- c(flaggedMsgs, checkForWhiteSpaces(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 100 / numOfCheckboxes * 7)
      # Log input
      XML_add_child("README2","<checkLvl2READMEWhiteSpaces/>")
      x <- identical(checkForWhiteSpaces(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl2READMEWhiteSpaces", "PASS")
      }else{
        XML_add_attr("checkLvl2READMEWhiteSpaces", "FAIL")
        XML_add_txt("checkLvl2READMEWhiteSpaces", "ERROR: File contains whitespace.")
      }
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkLvl2READMEEncapsulation){
      flaggedMsgs <- c(flaggedMsgs, checkForEncapsulation(readmeDF, dirName, "README.csv"))
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar",
                        value = 100 / numOfCheckboxes * 8)
      # Log input
      XML_add_child("README2","<checkLvl2READMEEncapsulation/>")
      x <- identical(checkForEncapsulation(readmeDF, dirName, "README.csv"), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl2READMEEncapsulation", "PASS")
      }else{
        XML_add_attr("checkLvl2READMEEncapsulation", "FAIL")
        XML_add_txt("checkLvl2READMEEncapsulation", "ERROR: File has encapsulation issues.")
      }
    }
  }
  
  return (flaggedMsgs)
}

# [END]