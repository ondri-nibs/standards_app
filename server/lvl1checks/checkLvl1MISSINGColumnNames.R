# checkLvl1MISSINGColumnNames.R
#
# Purpose: Check to ensure that all column names in the MISSING file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# ==========================================================================================

checkLvl1MISSINGColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
  
  # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 1 DIRECTORY.
  missingFileName <- getFileName(lvl1DirPath, "MISSING.csv")
  if (length(missingFileName) == 1){
    missingFilePath <- paste0(lvl1DirPath, "/", missingFileName)
    missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE, check.names = FALSE)
    missingDF <- convertDataFrame(missingDF)
    
    # Call helper function to check MISSING column names.
    flaggedMsgs <- checkMISSINGColumnNames(missingDF, dirName)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]