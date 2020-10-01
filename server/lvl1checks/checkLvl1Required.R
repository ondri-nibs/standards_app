# checkLvl1Required.R
#
# Purpose: Compare the required and the literal contents of the level 1 directory.
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-07-15
#
# ==========================================================================================

checkLvl1Required <- function(lvl1DirPath, fileType, lvl1RequiredFileTypes = NULL){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
  
  if (fileType == "DIR"){
    # Check for required file types.
    line <- checkRequired(lvl1DirPath, dirName, lvl1RequiredFileTypes)
    flaggedMsgs <- c(flaggedMsgs, line)
    
    #Check to see if level 2 directory is required (only required for non-tabular)
    type <- checkIfTab(lvl1DirPath)
    
    # At least one level 2 directory is required if non-tabular.
    if (type == "non-tabular"){
      lvl2DirNames <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
      if (length(lvl2DirNames) == 0){
        line <- paste(tags$span(class = "bold-category", 
                                paste("Required Files: Directory", dirName)),
                      "- There exists no level 2 directory. At least one level 2 directory 
                    is required.")
        flaggedMsgs <- c(flaggedMsgs, line)
      }
    }
    }
    
  
  # Check to ensure that there is only 1 of each file type. If there is only 1,
  # ensure that their encoding is in ASCII or UTF-8 (Otherwise, the data frames
  # cannot be read in correctly).
  readmeFileName <- getFileName(lvl1DirPath, "README.csv")
  dictFileName <- getFileName(lvl1DirPath, "DICT.csv")
  dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
  missingFileName <- getFileName(lvl1DirPath, "MISSING.csv")
  
  # README file.
  if (fileType == "README"){
    # Check for mandatory README file.
    line <- checkRequired(lvl1DirPath, dirName, lvl1RequiredFileTypes)
    flaggedMsgs <- c(flaggedMsgs, line)
    
    if (length(readmeFileName) > 1){
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files: Directory", dirName),
                    "- There exists more than 1 README file for this level 1 directory.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    else if (length(readmeFileName) == 1){
      readmeFilePath <- paste0(lvl1DirPath, "/", readmeFileName)
      flaggedMsgs <- c(flaggedMsgs, checkEncoding(readmeFilePath, dirName, fileType))
    }
  }
  
  
  # DICT file.
  if (fileType == "DICT"){
    if (length(dictFileName) > 1){
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files: Directory", dirName),
                    "- There exists more than 1 DICT file for this level 1 directory.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    else if (length(dictFileName) == 1){
      # 1) Check that corresponding DATA file exists too.
      if (length(dataFileName) != 1){
        line <- paste(tags$span(class = "bold-category", 
                                "Required Files: Directory", dirName),
                      "- A corresponding DATA file does not exist with", dictFileName, ".")
        flaggedMsgs <- c(flaggedMsgs, line)
      }
      
      # 2) Check encoding.
      dictFilePath <- paste0(lvl1DirPath, "/", dictFileName)
      flaggedMsgs <- c(flaggedMsgs, checkEncoding(dictFilePath, dirName, fileType))
    }
  }
  
  
  # DATA file.
  if (fileType == "DATA" || fileType == "DICT"){
    if (length(dataFileName) > 1){
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files: Directory", dirName),
                    "- There exists more than 1 DATA file for this level 1 directory.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    else if (length(dataFileName) == 1){
      dataFilePath <- paste0(lvl1DirPath, "/", dataFileName)
      flaggedMsgs <- c(flaggedMsgs, checkEncoding(dataFilePath, dirName, fileType))
    }
  }
  
  
  # MISSING file.
  if (fileType == "MISSING" || fileType == "DATA"){
    if (length(missingFileName) > 1){
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files: Directory", dirName),
                    "- There exists more than 1 MISSING file for this level 1 directory.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    else if (length(missingFileName) == 1){
      missingFilePath <- paste0(lvl1DirPath, "/", missingFileName)
      flaggedMsgs <- c(flaggedMsgs, checkEncoding(missingFilePath, dirName, fileType))
    }
  }

  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]