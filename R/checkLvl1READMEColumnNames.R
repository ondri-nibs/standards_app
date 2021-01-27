# checkLvl1READMEColumnNames.R
#
# Purpose: Check to ensure that all column names in the README file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# ==========================================================================================

checkLvl1READMEColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
  
  # We already know that the level 1 directory will have a README file
  # (as a result of required files check).
  readmeFilePath <- paste0(lvl1DirPath, "/", getFileName(lvl1DirPath, "README.csv"))
  readmeDF <- read.csv(readmeFilePath, stringsAsFactors = FALSE, check.names = FALSE)
  readmeDF <- convertDataFrame(readmeDF)
    
  # Call helper function to check README column names.
  flaggedMsgs <- checkREADMEColumnNames(readmeDF, dirName)
    
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]