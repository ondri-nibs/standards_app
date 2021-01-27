# checkForFileTypes.R
#
# Purpose: Check whether any level 2 or any level 3 directory contains a specific
# file type, and if so, return the list of file names.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-08-02
#
# ==========================================================================================

checkForFileTypes <- function(lvl1DirPath, fileType, lvl){
  listOfFileNames <- character()
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)

  if (lvl == 2){
    lvl2DirPaths <- paste0(lvl1DirPath, "/", lvl2Dirs)
    listOfFileNames <- unlist(lapply(lvl2DirPaths, getFileName, fileType))
  }
  else if (lvl == 3){
    lvl3DirPaths <- paste0(lvl1DirPath, "/", lvl2Dirs, "/DATAFILES")
    listOfFileNames <- unlist(lapply(lvl3DirPaths, getFileName, fileType))
  }
  
  return (listOfFileNames)
}

# [END]