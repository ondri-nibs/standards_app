# checkLvl2DATAColumnNames.R
#
# Purpose: Check to ensure that all column names in the DATA file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# ==========================================================================================

checkLvl2DATAColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a DATA file.
  for (dirName in lvl2Dirs){
    dirMsgs <- character()
    
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF DATA FILE EXISTS FOR THIS PARTICULAR LEVEL 2 DIRECTORY.
    dataFileName <- getFileName(lvl2DirPath, "DATA.csv")
    if (length(dataFileName) == 1){
      dataFilePath <- paste0(lvl2DirPath, "/", dataFileName)
      dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, check.names = FALSE)
      dataDF <- convertDataFrame(dataDF)
      
      # Call helper function to check DATA column names.
      dirMsgs <- checkDATAColumnNames(dataDF, dirName)
      
      # Add a line break to separate text for other directories.
      if (length(dirMsgs) > 0){
        dirMsgs[length(dirMsgs)] <- HTML(paste(dirMsgs[length(dirMsgs)], "<br/>"))
      }
      flaggedMsgs <- c(flaggedMsgs, dirMsgs)
      
    }
  }
  
  return (flaggedMsgs)
}

# [END]