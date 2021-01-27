# checkForWhiteSpaces.R
#
# Purpose: Check whether a tabular csv file contains no leading or trailing whitespaces.
# If not, the subject IDs/column labels/file names corresponding to the whitespaces
# are given through getWhiteSpaceLocations.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

checkForWhiteSpaces <- function(df, dirName, fileType){
  flaggedMsgs <- character()
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    if (any(grepl("^\\s$", substring(df[ , name ], 1, 1)) |
            grepl("^\\s$", substring(df[ , name ], nchar(df[ , name ]), 
                                     nchar(df[ , name ]))))
        ){
      flaggedMsgs <- paste(tags$span(class = "bold-category", 
                                     "White Spaces: Directory", dirName),
                           "- The file contains leading or trailing white spaces. 
                           Please use the dropdown to the right for the", firstColumn[2],
                           "corresponding to the white spaces.")
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