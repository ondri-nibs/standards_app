# checkLvl1DICTColumnNames.R
#
# Purpose: Check to ensure that all column names in the DICT file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# ==========================================================================================

checkLvl1DICTColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
  
  # IMPORTANT: RUN CHECK ONLY IF DICT FILE EXISTS FOR THIS PARTICULAR LEVEL 1 DIRECTORY.
  dictFileName <- getFileName(lvl1DirPath, "DICT.csv")
  if (length(dictFileName) == 1){
    dictFilePath <- paste0(lvl1DirPath, "/", dictFileName)
    dictDF <- read.csv(dictFilePath, stringsAsFactors = FALSE, check.names = FALSE)
    dictDF <- convertDataFrame(dictDF)
    
    # Call helper function to check DICT column names.
    flaggedMsgs <- checkDICTColumnNames(dictDF, dirName)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]