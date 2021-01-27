# checkLvl2READMEColumnNames.R
#
# Purpose: Check to ensure that all column names in the README file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# ==========================================================================================

checkLvl2READMEColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory. We already know that each directory will have
  # a README file (as a result of required files check).
  for (dirName in lvl2Dirs){
    dirMsgs <- character()
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    readmeFilePath <- paste0(lvl2DirPath, "/", getFileName(lvl2DirPath, "README.csv"))
    readmeDF <- read.csv(readmeFilePath, stringsAsFactors = FALSE, check.names = FALSE)
    readmeDF <- convertDataFrame(readmeDF)
    
    # Call helper function to check README column names.
    dirMsgs <- checkREADMEColumnNames(readmeDF, dirName)
    
    # Add a line break to separate text for other directories.
    if (length(dirMsgs) > 0){
      dirMsgs[length(dirMsgs)] <- HTML(paste(dirMsgs[length(dirMsgs)], "<br/>"))
    }
    flaggedMsgs <- c(flaggedMsgs, dirMsgs)
    
  }
  
  return (flaggedMsgs)
}

# [END]