# getFileName.R
#
# Purpose: Return the file name of a tabular file depending on its type.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# =========================================================================================

getFileName <- function(dirPath, fileType){
  dirFileNames <- list.files(path = dirPath)
  dirFileTypes <- sapply(strsplit(dirFileNames, "_"), tail, 1)
  fileName <- (dirFileNames)[ dirFileTypes == fileType ]
  return (fileName)
}

# [END]