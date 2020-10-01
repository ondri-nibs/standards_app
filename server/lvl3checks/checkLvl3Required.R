# checkLvl3Required.R
#
# Purpose: Compare the required and the literal contents of each level 3 directory.
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-07-02
#
# ==========================================================================================

checkLvl3Required <- function(lvl1DirPath, fileType, lvl3RequiredFileTypes = NULL){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory.
  for (dirName in lvl2Dirs) {
    dirMsgs <- character()
    
    # Compare the required and the literal contents of this level 3 directory, provided
    # the level 3 DATAFILES folder exists.
    lvl3DirPath <- paste0(lvl1DirPath, "/", dirName, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    
    if (!dir.exists(lvl3DirPath)){
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files: Directory", dirName),
                    "- A DATAFILES folder does not exist for this level 2 directory.")
      dirMsgs <- c(dirMsgs, line)
    }
    else{
      if (fileType == "DIR" || fileType == "FILELIST" || fileType == "MISSING"){
        # Check for required file types.
        line <- checkRequired(lvl3DirPath, lvl3DirName, lvl3RequiredFileTypes)
        dirMsgs <- c(dirMsgs, line)
      }
      
      
      # Check to ensure that there is only 1 of each file type. If there is only 1,
      # ensure that their encoding is in ASCII or UTF-8 (Otherwise, the data frames
      # cannot be read in correctly).
      filelistFileName <- getFileName(lvl3DirPath, "FILELIST.csv")
      missingFileName <- getFileName(lvl3DirPath, "MISSING.csv")
      dictFileName <- getFileName(lvl3DirPath, "DICT.csv")
      
      # FILELIST file.
      if (fileType == "FILELIST"){
        if (length(filelistFileName) > 1){
          line <- paste(tags$span(class = "bold-category", 
                                  "Required Files: Directory", lvl3DirName),
                        "- There exists more than 1 FILELIST file for this level 3 directory.")
          dirMsgs <- c(dirMsgs, line)
        }
        else if (length(filelistFileName) == 1){
          filelistFilePath <- paste0(lvl3DirPath, "/", filelistFileName)
          dirMsgs <- c(dirMsgs, checkEncoding(filelistFilePath, lvl3DirName, fileType))
        }
      }
      
      
      # MISSING file.
      if (fileType == "MISSING"){
        if (length(missingFileName) > 1){
          line <- paste(tags$span(class = "bold-category", 
                                  "Required Files: Directory", lvl3DirName),
                        "- There exists more than 1 MISSING file for this level 3 directory.")
          dirMsgs <- c(dirMsgs, line)
        }
        else if (length(missingFileName) == 1){
          missingFilePath <- paste0(lvl3DirPath, "/", missingFileName)
          dirMsgs <- c(dirMsgs, checkEncoding(missingFilePath, lvl3DirName, fileType))
        }
      }
      
      
      # DICT file. Cannot check encoding of DICT file as it is a text file.
      if (fileType == "DICT"){
        if (length(dictFileName) > 1){
          line <- paste(tags$span(class = "bold-category", 
                                  "Required Files: Directory", lvl3DirName),
                        "- There exists more than 1 DICT file for this level 3 directory.")
          dirMsgs <- c(dirMsgs, line)
        }
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