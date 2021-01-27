# checkLvl3MISSINGColumnNames.R
#
# Purpose: Check to ensure that all column names in the MISSING file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-08
#
# ==========================================================================================

checkLvl3MISSINGColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()
  
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  # Iterate through each lvl 2 directory and only check in level 3 directories that
  # contain a MISSING file.
  for (dirName in lvl2Dirs){
    dirMsgs <- character()
    
    lvl3DirPath <- paste0(lvl1DirPath, "/", dirName, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    
    # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 3 DIRECTORY.
    missingFileName <- getFileName(lvl3DirPath, "MISSING.csv")
    if (length(missingFileName) == 1){
      missingFilePath <- paste0(lvl3DirPath, "/", missingFileName)
      missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE, check.names = FALSE)
      missingDF <- convertDataFrame(missingDF)
      
      # Call helper function to check MISSING column names.
      dirMsgs <- checkMISSINGColumnNames(missingDF, lvl3DirName)
      
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