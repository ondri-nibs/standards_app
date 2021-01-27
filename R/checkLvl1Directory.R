# checkLvl1Directory.R
#
# Purpose: Create the server logic for level 1 directory checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-23
#
# =======================================================================================

checkLvl1Directory <- function(input, session, lvl1DirPath, numOfCheckboxes,
                               lvl1RequiredFileTypes, study_name){
  flaggedMsgs <- character()
  lvl1DirName <- basename(lvl1DirPath)
  
  readmeFileName <- getFileName(lvl1DirPath, "README.csv")
  methodsFileName <- getFileName(lvl1DirPath, "METHODS.pdf")
  dictFileName <- getFileName(lvl1DirPath, "DICT.csv")
  dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
  missingFileName <- getFileName(lvl1DirPath, "MISSING.csv")
  supFileNames <- c(getFileName(lvl1DirPath, "SUP.csv"),
                    getFileName(lvl1DirPath, "SUP.txt"),
                    getFileName(lvl1DirPath, "SUP.pdf"))
  glossaryFileNames <- c(getFileName(lvl1DirPath, "GLOSSARY.csv"),
                         getFileName(lvl1DirPath, "GLOSSARY.txt"),
                         getFileName(lvl1DirPath, "GLOSSARY.pdf"))
  
  listOfFileNames <- c(readmeFileName, methodsFileName, dictFileName, dataFileName,
                       missingFileName, supFileNames, glossaryFileNames)
  
  # For lvl1 Directory =======================================================================================
  if (checkIfTab(lvl1DirPath) == "non-tabular"){
  
    # "Required Files Check" checkbox.
    if (input$checkLvl1Required & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkLvl1Required(lvl1DirPath, "DIR",
                                                      lvl1RequiredFileTypes))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes)
      # Log input
      XML_add_child("DIR","<checkLvl1Required/>")
      x <- identical(checkLvl1Required(lvl1DirPath, "DIR",
                                       lvl1RequiredFileTypes), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1Required", "PASS")
      }else{
        XML_add_attr("checkLvl1Required", "FAIL")
        XML_add_txt("checkLvl1Required", "Error: Missing a required file in the level 1 directory.")
      }
    }
    
    # "Directory Name Check" checkbox.
    if (input$checkLvl1DirName & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(lvl1DirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 2)
      # Log input
      XML_add_child("DIR","<checkLvl1DirName/>")
      x <- identical(checkLvl1DirName(lvl1DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1DirName", "PASS")
      }else{
        XML_add_attr("checkLvl1DirName", "FAIL")
        XML_add_txt("checkLvl1DirName", "Error: A Level 1 Directory does not follow proper naming convention.")
      }
    }
    
    # "File Names Check" checkbox.
    if (input$checkLvl1FileNames & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkFileNames(listOfFileNames, lvl1DirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 3)
      # Log input
      XML_add_child("DIR","<checkLvl1FileNames/>")
      x <- identical(checkFileNames(listOfFileNames, lvl1DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1FileNames", "PASS")
      }else{
        XML_add_attr("checkLvl1FileNames", "FAIL")
        XML_add_txt("checkLvl1FileNames", "Error: A Level 1 file name does not follow proper naming convention.")
      }
    }
    
    # "File Names Comparison" checkbox.
    if (input$compareLvl1FileNames & study_name != "Package only checks"){
      if (length(checkLvl1DirName(lvl1DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", lvl1DirName),
                      "- The level 1 directory name needs to be valid to run this check.
                      Please refer to directory name check.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else if (length(checkFileNames(listOfFileNames, lvl1DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", lvl1DirName),
                      "- All Level 1 file names need to be valid to run this check.
                      Please refer to file names check.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else{
        flaggedMsgs <- c(flaggedMsgs, compareFileNames(listOfFileNames, lvl1DirName, lvl1DirName))
      
      
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
      # Log input
      XML_add_child("DIR","<compareLvl1FileNames/>")
      x <- identical( compareFileNames(listOfFileNames, lvl1DirName, lvl1DirName), character(0))
      if (x == TRUE){
        XML_add_attr("compareLvl1FileNames", "PASS")
      }else{
        XML_add_attr("compareLvl1FileNames", "FAIL")
        XML_add_txt("compareLvl1FileNames", "ERROR: File Name comparisons failed.")
      }
      }
    }
    
    # "File Extensions Check" checkbox.
    if (input$checkLvl1FileExt & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkFileExt(lvl1DirPath))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 5)
      # Log input
      XML_add_child("DIR","<checkLvl1FileExt/>")
      x <- identical(checkFileExt(lvl1DirPath), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1FileExt", "PASS")
      }else{
        XML_add_attr("checkLvl1FileExt", "FAIL")
        XML_add_txt("checkLvl1FileExt", "ERROR: Files do not meet the required file extensions.")
      }
    }
    
    # EXTRA CHECK: Acknowledge if SUP file exists in level 1 directory.
    if (length(supFileNames) > 0){
      line <- paste(tags$span(class = "bold-category", 
                              "Please note that the level 1 directory contains a SUP file. 
                              It is strongly recommended that this file be checked manually
                              as there are no computerized checks in place."))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # EXTRA CHECK: "Compressed Files" check box.
    if (input$checkLvl1CompressedFiles){
      flaggedMsgs <- c(flaggedMsgs, checkForCompressedFiles(lvl1DirPath))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 2)
      # Log input
      XML_add_child("DIR","<checkLvl1CompressedFiles/>")
      x <- identical(checkForCompressedFiles(lvl1DirPath), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl1CompressedFiles", "PASS")
      }else{
        XML_add_attr("checkLvl1CompressedFiles", "WARNING")
        XML_add_txt("checkLvl1CompressedFiles", "Warning: A file in the directory is compressed
                    (needs to be checked manually).")
      }
    }
    
    # Extra: "Duplicate File Names" check box.
    if (input$checkLvl1DuplicateFileNames){
      flaggedMsgs <- c(flaggedMsgs, checkForDuplicateFileNames(lvl1DirPath))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 2)
    # Log input
    XML_add_child("DIR","<checkLvl1DuplicateFileNames/>")
    x <- identical(checkForDuplicateFileNames(lvl1DirPath), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl1DuplicateFileNames", "PASS")
    }else{
      XML_add_attr("checkLvl1DuplicateFileNames", "WARNING")
      XML_add_txt("checkLvl1DuplicateFileNames", "Warning: The data package contains duplicate file names.")
    }
  }
    
    return (flaggedMsgs)
    
    
    # For tabular directory =======================================================================================  
} else if (checkIfTab(lvl1DirPath) == "tabular"){


    
    if (input$checkTabularRequired & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkLvl1Required(lvl1DirPath, "DIR",
                                                    lvl1RequiredFileTypes))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes)
      
      # Log input
      XML_add_child("DIR","<checkTabularRequired/>")
      x <- identical(checkLvl1Required(lvl1DirPath, "DIR",
                                       lvl1RequiredFileTypes), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularRequired", "PASS")
      }else{
        XML_add_attr("checkTabularRequired", "FAIL")
        XML_add_txt("checkTabularRequired", "Error: Missing a required file in the level 1 directory.")
      }
    }
  
    # "Directory Name Check" checkbox.
    if (input$checkTabularDirName & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(lvl1DirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 2)
      # Log input
      XML_add_child("DIR","<checkTabularDirName/>")
      x <- identical(checkLvl1DirName(lvl1DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularDirName", "PASS")
      }else{
        XML_add_attr("checkTabularDirName", "FAIL")
        XML_add_txt("checkTabularDirName", "Error: A Level 1 Directory does not follow proper naming convention.")
      }
    }
    
    # "File Names Check" checkbox.
    if (input$checkTabularFileNames & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkFileNames(listOfFileNames, lvl1DirName))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 3)
      # Log input
      XML_add_child("DIR","<checkTabularFileNames/>")
      x <- identical(checkFileNames(listOfFileNames, lvl1DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularFileNames", "PASS")
      }else{
        XML_add_attr("checkTabularFileNames", "FAIL")
        XML_add_txt("checkTabularFileNames", "Error: A Level 1 file name does not follow proper naming convention.")
      }
      
    }
    
    # "File Names Comparison" checkbox.
    if (input$compareTabularFileNames & study_name != "Package only checks"){
      if (length(checkLvl1DirName(lvl1DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", lvl1DirName),
                      "- The level 1 directory name needs to be valid to run this check.
                        Please refer to directory name check.")
        flaggedMsgs <- c(flaggedMsgs, line)
        
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else if (length(checkFileNames(listOfFileNames, lvl1DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", lvl1DirName),
                      "- All Level 1 file names need to be valid to run this check.
                        Please refer to file names check.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else{
        flaggedMsgs <- c(flaggedMsgs, compareFileNames(listOfFileNames, lvl1DirName, lvl1DirName))
        
        updateProgressBar(session = session, 
                          id = "lvl1ProgressBar", 
                          value = 100 / numOfCheckboxes * 4)
        # Log input
        XML_add_child("DIR","<compareTabularFileNames/>")
        x <- identical( compareFileNames(listOfFileNames, lvl1DirName, lvl1DirName), character(0))
        if (x == TRUE){
          XML_add_attr("compareTabularFileNames", "PASS")
        }else{
          XML_add_attr("compareTabularFileNames", "FAIL")
          XML_add_txt("compareTabularFileNames", "ERROR: File Name comparisons failed.")
        }
      }
      
    }
    
    # "File Extensions Check" checkbox.
    if (input$checkTabularFileExt & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkFileExt(lvl1DirPath))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 5)
      # Log input
      XML_add_child("DIR","<checkTabularFileExt/>")
      x <- identical(checkFileExt(lvl1DirPath), character(0))
      if (x == TRUE){
        XML_add_attr("checkTabularFileExt", "PASS")
      }else{
        XML_add_attr("checkTabularFileExt", "FAIL")
        XML_add_txt("checkTabularFileExt", "ERROR: Files do not meet the required file extensions.")
      }
      
    }
    
    # EXTRA CHECK: Acknowledge if SUP file exists in level 1 directory.
    if (length(supFileNames) > 0){
      line <- paste(tags$span(class = "bold-category", 
                              "Please note that the level 1 directory contains a SUP file. 
                                It is strongly recommended that this file be checked manually
                                as there are no computerized checks in place."))
      flaggedMsgs <- c(flaggedMsgs, line)
      
    }
    
  # EXTRA CHECK: "Compressed Files" check box.
  if (input$checkTabularCompressedFiles){
    flaggedMsgs <- c(flaggedMsgs, checkForCompressedFiles(lvl1DirPath))
    updateProgressBar(session = session, 
                      id = "lvl1ProgressBar", 
                      value = 100 / numOfCheckboxes * 2)
    # Log input
    XML_add_child("DIR","<checkTabularCompressedFiles/>")
    x <- identical(checkForCompressedFiles(lvl1DirPath), character(0))
    if (x == TRUE){
      XML_add_attr("checkTabularCompressedFiles", "PASS")
    }else{
      XML_add_attr("checkTabularCompressedFiles", "WARNING")
      XML_add_txt("checkTabularCompressedFiles", "Warning: A file in the level 1 directory is compressed
                    (needs to be checked manually).")
    }
  }
    
    return (flaggedMsgs)
  }
}

# [END]