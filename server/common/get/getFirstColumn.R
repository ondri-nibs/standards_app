# getFirstColumn.R
#
# Purpose: Return the name of the 1st column and its phrase depending on the file type.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

getFirstColumn <- function(fileType){
  firstColumn <- c()
  
  if (fileType == "DATA.csv" || fileType == "MISSING.csv" || fileType == "FILELIST.csv"){
    firstColumn <- c("SUBJECT", "subject IDs")
  }
  else if (fileType == "DICT.csv"){
    firstColumn <- c("COLUMN_LABEL", "column labels")
  }
  else if (fileType == "README.csv"){
    firstColumn <- c("FILE", "file names")
  }
  
  return (firstColumn)
}

# [END]