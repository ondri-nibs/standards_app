# checkLvl2Required.R
#
# Purpose: Compare the required and the literal contents of each level 2 directory.
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-06-19
#
# ==========================================================================================

checkLvl2Required <- function(lvl1DirPath, fileType, lvl2RequiredFileTypes = NULL){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory.
  for (dirName in lvl2Dirs) {
    dirMsgs <- character()
    
    # Compare the required and the literal contents of this level 2 directory.
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    if (fileType == "DIR" || fileType == "README"){
      # Check for required file types.
      line <- checkRequired(lvl2DirPath, dirName, lvl2RequiredFileTypes)
      dirMsgs <- c(dirMsgs, line)
    }
    
    
    # Check to ensure that there is only 1 of each file type. If there is only 1,
    # ensure that they contain no ragged rows.
    readmeFileName <- getFileName(lvl2DirPath, "README.csv")
    dictFileName <- getFileName(lvl2DirPath, "DICT.csv")
    dataFileName <- getFileName(lvl2DirPath, "DATA.csv")
    missingFileName <- getFileName(lvl2DirPath, "MISSING.csv")
    
    # README file.
    if (fileType == "README"){
      if (length(readmeFileName) > 1){
        line <- paste(tags$span(class = "bold-category", 
                                "Required Files: Directory", dirName),
                      "- There exists more than 1 README file for this level 2 directory.")
        dirMsgs <- c(dirMsgs, line)
      }
      else if (length(readmeFileName) == 1){
        readmeFilePath <- paste0(lvl2DirPath, "/", readmeFileName)
        dirMsgs <- c(dirMsgs, checkForRaggedRows(readmeFilePath, dirName, fileType))
      }
    }
    
    
    # DICT file.
    if (fileType == "DICT"){
      if (length(dictFileName) > 1){
        line <- paste(tags$span(class = "bold-category", 
                                "Required Files: Directory", dirName),
                      "- There exists more than 1 DICT file for this level 2 directory.")
        dirMsgs <- c(dirMsgs, line)
      }
      else if (length(dictFileName) == 1){
        # 1) Check that corresponding DATA file exists too.
        if (length(dataFileName) != 1){
          line <- paste(tags$span(class = "bold-category", 
                                  "Required Files: Directory", dirName),
                        "- A corresponding DATA file does not exist with", dictFileName, ".")
          dirMsgs <- c(dirMsgs, line)
        }
        
        # 2) Check for ragged rows.
        dictFilePath <- paste0(lvl2DirPath, "/", dictFileName)
        dirMsgs <- c(dirMsgs, checkForRaggedRows(dictFilePath, dirName, fileType))
      }
    }
    
    
    # DATA file.
    if (fileType == "DATA" || fileType == "DICT"){
      if (length(dataFileName) > 1){
        line <- paste(tags$span(class = "bold-category", 
                                "Required Files: Directory", dirName),
                      "- There exists more than 1 DATA file for this level 2 directory.")
        dirMsgs <- c(dirMsgs, line)
      }
      else if (length(dataFileName) == 1){
        dataFilePath <- paste0(lvl2DirPath, "/", dataFileName)
        dirMsgs <- c(dirMsgs, checkForRaggedRows(dataFilePath, dirName, "DATA"))
      }
    }
    
    
    # MISSING file.
    if (fileType == "MISSING" || fileType == "DATA"){
      if (length(missingFileName) > 1){
        line <- paste(tags$span(class = "bold-category", 
                                "Required Files: Directory", dirName),
                      "- There exists more than 1 MISSING file for this level 2 directory.")
        dirMsgs <- c(dirMsgs, line)
      }
      else if (length(missingFileName) == 1){
        missingFilePath <- paste0(lvl2DirPath, "/", missingFileName)
        dirMsgs <- c(dirMsgs, checkForRaggedRows(missingFilePath, dirName, "MISSING"))
      }
    }
    
    
    # Add a line break to separate text for other directories.
    if (length(dirMsgs) > 0){
      dirMsgs[length(dirMsgs)] <- HTML(paste(dirMsgs[length(dirMsgs)], "<br/>"))
    }
    flaggedMsgs <- c(flaggedMsgs, dirMsgs)
    
  }
  
  
  return (flaggedMsgs)
}

# [END]