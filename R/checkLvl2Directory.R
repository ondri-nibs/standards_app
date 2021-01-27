# checkLvl2Directory.R
#
# Purpose: Create the server logic for level 2 directory checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-24
#
# =======================================================================================

checkLvl2Directory <- function(input, session, lvl1DirPath, numOfCheckboxes,
                               lvl2RequiredFileTypes, study_name){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  lvl1DirName <- basename(lvl1DirPath)
  
  
  # "Required Files Check" checkbox.
  if (input$checkLvl2Required & study_name != "Package only checks"){
    flaggedMsgs <- c(flaggedMsgs, checkLvl2Required(lvl1DirPath, "DIR",
                                                    lvl2RequiredFileTypes))
    updateProgressBar(session = session, 
                      id = "lvl2ProgressBar", 
                      value = 100 / numOfCheckboxes)
    # Log input
    XML_add_child("DIR2","<checkLvl2Required/>")
    x <- identical(checkLvl2Required(lvl1DirPath, "DIR",
                                     lvl2RequiredFileTypes), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl2Required", "PASS")
    }else{
      XML_add_attr("checkLvl2Required", "FAIL")
      XML_add_txt("checkLvl2Required", "Error: Missing a required file in the level 2 directory.")
    }
  }
  
  # "Directory Name Check" checkbox.
  if (input$checkLvl2DirNames & study_name != "Package only checks"){
    flaggedMsgs <- c(flaggedMsgs, checkLvl2DirNames(lvl2Dirs))
    updateProgressBar(session = session, 
                      id = "lvl2ProgressBar", 
                      value = 100 / numOfCheckboxes * 2)
    # Log input
    XML_add_child("DIR2","<checkLvl2DirNames/>")
    x <- identical(checkLvl2DirNames(lvl2Dirs), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl2DirNames", "PASS")
    }else{
      XML_add_attr("checkLvl2DirNames", "FAIL")
      XML_add_txt("checkLvl2DirNames", "Error: A Level 2 Directory does not follow proper naming convention.")
    }
  }
  
  # Iterate through each lvl 2 directory.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    readmeFileName <- getFileName(lvl2DirPath, "README.csv")
    methodsFileName <- getFileName(lvl2DirPath, "METHODS.pdf")
    dictFileName <- getFileName(lvl2DirPath, "DICT.csv")
    dataFileName <- getFileName(lvl2DirPath, "DATA.csv")
    missingFileName <- getFileName(lvl2DirPath, "MISSING.csv")
    supFileNames <- c(getFileName(lvl2DirPath, "SUP.csv"),
                      getFileName(lvl2DirPath, "SUP.txt"),
                      getFileName(lvl2DirPath, "SUP.pdf"))
    glossaryFileNames <- c(getFileName(lvl2DirPath, "GLOSSARY.csv"),
                           getFileName(lvl2DirPath, "GLOSSARY.txt"),
                           getFileName(lvl2DirPath, "GLOSSARY.pdf"))
    
    listOfFileNames <- c(readmeFileName, methodsFileName, dictFileName, dataFileName,
                         missingFileName, supFileNames, glossaryFileNames)

    # "File Names Check" checkbox.
    if (input$checkLvl2FileNames & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkFileNames(listOfFileNames, dirName))
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 100 / numOfCheckboxes * 3)
      # Log input
      XML_add_child("DIR2","<checkLvl2FileNames/>")
      x <- identical( checkFileNames(listOfFileNames, dirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl2FileNames", "PASS")
      }else{
        XML_add_attr("checkLvl2FileNames", "FAIL")
        XML_add_txt("checkLvl2FileNames", "Error: A Level 2 file name does not follow proper naming convention.")
      }
    }
    
    # "File Names Comparison" checkbox.
    if (input$compareLvl2FileNames & study_name != "Package only checks"){
      if (length(checkLvl1DirName(lvl1DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", dirName),
                      "- The level 1 directory name needs to be valid to run this check.
                      Please refer to directory name check in level 1 tab.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else if (length(checkFileNames(listOfFileNames, dirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", dirName),
                      "- All Level 2 file names need to be valid to run this check.
                    Please refer to file names check.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else{
        flaggedMsgs <- c(flaggedMsgs, compareFileNames(listOfFileNames, lvl1DirName, dirName,
                                                       2, dirName))
        # Log input
        XML_add_child("DIR2","<compareLvl2FileNames/>")
        x <- identical(compareFileNames(listOfFileNames, lvl1DirName, dirName,
                                        2, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("compareLvl2FileNames", "PASS")
        }else{
          XML_add_attr("compareLvl2FileNames", "FAIL")
          XML_add_txt("compareLvl2FileNames", "ERROR: File Name comparisons failed.")
        }
      }
      
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
    }
    
    # EXTRA CHECK: Acknowledge if SUP file exists in any level 2 directory.
    if (length(supFileNames) > 0){
      line <- paste(tags$span(class = "bold-category", 
                              "Please note that the level 2 directory",
                              tags$span(class = "colour-category", dirName),
                              "contains a SUP file. It is strongly recommended that this 
                              file be checked manually as there are no computerized checks
                              in place."))
      flaggedMsgs <- c(flaggedMsgs, line)
      if (length(flaggedMsgs) > 0){
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                       "<br/>"))
      }
    }
    
    # EXTRA CHECK: "Compressed Files" check box.
    if (input$checkLvl2CompressedFiles){
      flaggedMsgs <- c(flaggedMsgs, checkForCompressedFiles(lvl1DirPath))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 2)
      # Log input
      XML_add_child("DIR2","<checkLvl2CompressedFiles/>")
      x <- identical(checkForCompressedFiles(lvl1DirPath), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl2CompressedFiles", "PASS")
      }else{
        XML_add_attr("checkLvl2CompressedFiles", "WARNING")
        XML_add_txt("checkLvl2CompressedFiles", "Warning: A file in the level 1 directory is compressed
                    (needs to be checked manually).")
      }
    }
    
  }
  
  
  return (flaggedMsgs)
}

# [END]