# checkNumOfCharacters.R
#
# Purpose: Check whether a tabular csv file does not contain more than 200 characters 
# in text for any 1 cell. If not, the subject IDs/column labels/file names corresponding
# to the > 200 character violations are given through getNumOfCharacterLocations.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =========================================================================================

checkNumOfCharacters <- function(df, dirName, fileType){
  flaggedMsgs <- character()
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    if (any(nchar(df[ , name ]) > 200)){
      flaggedMsgs <- paste(tags$span(class = "bold-category", 
                                     "Number of Characters: Directory", dirName),
                           "- The file has content that contains more than 200
                           characters. Please use the dropdown to the right for 
                           the", firstColumn[2], "corresponding to the > 200
                           character violations.")
      break
    }
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]