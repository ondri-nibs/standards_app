# checkLvl3Directory.R
#
# Purpose: Create the server logic for level 3 directory checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-24
#
# =======================================================================================

checkLvl3Directory <- function(input, session, lvl1DirPath, numOfCheckboxes,
                               lvl3RequiredFileTypes, participantIDDF, study_name){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  lvl1DirName <- basename(lvl1DirPath)
  
  
  # "Required Files Check" checkbox.
  if (input$checkLvl3Required & study_name != "Package only checks"){
    flaggedMsgs <- c(flaggedMsgs, checkLvl3Required(lvl1DirPath, "DIR",
                                                    lvl3RequiredFileTypes))
    updateProgressBar(session = session, 
                      id = "lvl3ProgressBar", 
                      value = 100 / numOfCheckboxes)
    # Log input
    XML_add_child("DIR3","<checkLvl3Required/>")
    x <- identical(checkLvl3Required(lvl1DirPath, "DIR",
                                     lvl3RequiredFileTypes), character(0))
    if (x == TRUE){
      XML_add_attr("checkLvl3Required", "PASS")
    }else{
      XML_add_attr("checkLvl3Required", "FAIL")
      XML_add_txt("checkLvl3Required", "Error: Missing a required file in the level 3 directory.")
    }
  }
  
  # Iterate through each lvl 2 directory.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    lvl3DirPath <- paste0(lvl2DirPath, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    
    filelistFileName <- getFileName(lvl3DirPath, "FILELIST.csv")
    missingFileName <- getFileName(lvl3DirPath, "MISSING.csv")
    dictFileName <- getFileName(lvl3DirPath, "DICT.csv")
    listOfFileNames <- c(filelistFileName, missingFileName, dictFileName)

    # "File Names Check" checkbox.
    if (input$checkLvl3FileNames & study_name != "Package only checks"){
      flaggedMsgs <- c(flaggedMsgs, checkFileNames(listOfFileNames, lvl3DirName))
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 2)
      # Log input
      XML_add_child("DIR3","<checkLvl3FileNames/>")
      x <- identical(checkFileNames(listOfFileNames, lvl3DirName), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl3FileNames", "PASS")
      }else{
        XML_add_attr("checkLvl3FileNames", "FAIL")
        XML_add_txt("checkLvl3FileNames", "Error: A Level 3 Directory does not follow proper naming convention.")
      }
    }
    
    # "File Names Comparison" checkbox.
    if (input$compareLvl3FileNames & study_name != "Package only checks"){
      if (length(checkLvl1DirName(lvl1DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", lvl3DirName),
                      "- The level 1 directory name needs to be valid to run this check.
                      Please refer to directory name check in level 1 tab.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else if (length(checkFileNames(listOfFileNames, lvl3DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Names Comparison: Directory", lvl3DirName),
                      "- All level 3 file names need to be valid to run this check.
                      Please refer to file names check.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else{
        flaggedMsgs <- c(flaggedMsgs, compareFileNames(listOfFileNames, lvl1DirName, 
                                                       lvl3DirName, 3, dirName))
        # Log input
        XML_add_child("DIR3","<compareLvl3FileNames/>")
        x <- identical( compareFileNames(listOfFileNames, lvl1DirName, 
                                         lvl3DirName, 3, dirName), character(0))
        if (x == TRUE){
          XML_add_attr("compareLvl3FileNames", "PASS")
        }else{
          XML_add_attr("compareLvl3FileNames", "FAIL")
          XML_add_txt("compareLvl3FileNames", "ERROR: File Name comparisons failed.")
        }
      }
      
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 3)
    }
    
    # "File Name Subject IDs Verification" checkbox.
    if (input$verifyLvl3FileNameSubjectIDs & study_name != "Package only checks"){
      if (length(checkLvl1DirName(lvl1DirName)) != 0){
        line <- paste(tags$span(class = "bold-category", 
                                "File Name Subject IDs Verification: Directory", 
                                lvl3DirName),
                      "- The level 1 directory name needs to be valid to run this check.
                      Please refer to directory name check in level 1 tab.")
        flaggedMsgs <- c(flaggedMsgs, line)
        if (length(flaggedMsgs) > 0){
          flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                         "<br/>"))
        }
      }
      else{
        flaggedMsgs <- c(flaggedMsgs, verifyFileNameSubjectIDs(participantIDDF, lvl1DirName, 
                                                               lvl3DirName, lvl1DirPath, 
                                                               lvl2DirPath, lvl3DirPath))
        # Log input
        XML_add_child("DIR3","<verifyLvl3FileNameSubjectIDs/>")
        x <- identical(verifyFileNameSubjectIDs(participantIDDF, lvl1DirName, 
                                                lvl3DirName, lvl1DirPath, 
                                                lvl2DirPath, lvl3DirPath), character(0))
        if (x == TRUE){
          XML_add_attr("verifyLvl3FileNameSubjectIDs", "PASS")
        }else{
          XML_add_attr("verifyLvl3FileNameSubjectIDs", "FAIL")
          XML_add_txt("verifyLvl3FileNameSubjectIDs", "ERROR: File contains file name subject ID issues.")
        }
      }
      
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 100 / numOfCheckboxes * 4)
    }
    
    # EXTRA CHECK: Acknowledge if DICT file exists in any level 3 directory.
    if (length(dictFileName) == 1){
      line <- paste(tags$span(class = "bold-category", 
                              "Please note that the level 3 directory",
                              tags$span(class = "colour-category", lvl3DirName),
                              "contains a DICT file. It is strongly recommended that this 
                              file be checked manually as there are no computerized checks
                              in place."))
      flaggedMsgs <- c(flaggedMsgs, line)
      if (length(flaggedMsgs) > 0){
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                       "<br/>"))
      }
    }
    
    # EXTRA CHECK: "Compressed Files" check box.
    if (input$checkLvl3CompressedFiles){
      flaggedMsgs <- c(flaggedMsgs, checkForCompressedFiles(lvl1DirPath))
      updateProgressBar(session = session, 
                        id = "lvl1ProgressBar", 
                        value = 100 / numOfCheckboxes * 2)
      # Log input
      XML_add_child("DIR3","<checkLvl3CompressedFiles/>")
      x <- identical(checkForCompressedFiles(lvl1DirPath), character(0))
      if (x == TRUE){
        XML_add_attr("checkLvl3CompressedFiles", "PASS")
      }else{
        XML_add_attr("checkLvl3CompressedFiles", "WARNING")
        XML_add_txt("checkLvl3CompressedFiles", "Warning: A file in the level 1 directory is compressed
                    (needs to be checked manually).")
      }
    }
  }
  
  
  return (flaggedMsgs)
}

# [END]