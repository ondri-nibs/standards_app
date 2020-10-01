# checkForCommas.R
#
# Purpose: Check whether a tabular csv file contains no commas. If not, 
# the subject IDs/column labels/file names corresponding to the commas
# are given through getCommaLocations.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

checkForCommas <- function(df, dirName, fileType){
  flaggedMsgs <- character()
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)

  for (name in columnNames){
    if (any(grepl(",", df[ , name ]))){
      flaggedMsgs <- paste(tags$span(class = "bold-category", "Commas: Directory", dirName),
                           "- The file contains commas. Please use the dropdown to
                            the right for the", firstColumn[2], "corresponding to the
                            commas. Commas are not permitted except to separate cells.
                            Please replace with a semi-colon (;).")
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