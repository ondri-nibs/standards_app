# checkLvl1DATAColumnNames.R
#
# Purpose: Check to ensure that all column names in the DATA file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# ==========================================================================================

checkLvl1DATAColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  dirName <- basename(lvl1DirPath)
    
  # IMPORTANT: RUN CHECK ONLY IF DATA FILE EXISTS FOR THIS PARTICULAR LEVEL 1 DIRECTORY.
  dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
  if (length(dataFileName) == 1){
    dataFilePath <- paste0(lvl1DirPath, "/", dataFileName)
    dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, check.names = FALSE)
    dataDF <- convertDataFrame(dataDF)
    
    # Call helper function to check DATA column names.
    flaggedMsgs <- checkDATAColumnNames(dataDF, dirName)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]