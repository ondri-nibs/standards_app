# checkLvl2DICTColumnNames.R
#
# Purpose: Check to ensure that all column names in the DICT file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# ==========================================================================================

checkLvl2DICTColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a DICT file.
  for (dirName in lvl2Dirs){
    dirMsgs <- character()
    
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF DICT FILE EXISTS FOR THIS PARTICULAR LEVEL 2 DIRECTORY.
    dictFileName <- getFileName(lvl2DirPath, "DICT.csv")
    if (length(dictFileName) == 1){
      dictFilePath <- paste0(lvl2DirPath, "/", dictFileName)
      dictDF <- read.csv(dictFilePath, stringsAsFactors = FALSE, check.names = FALSE)
      dictDF <- convertDataFrame(dictDF)
      
      # Call helper function to check DICT column names.
      dirMsgs <- checkDICTColumnNames(dictDF, dirName)
      
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