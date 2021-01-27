# checkForSpecialValues.R
#
# Purpose: Check whether the DATA file contains special values such as NaN (not a number),
# Inf (infinity), -Inf (negative infinity), or NULL (no value). If so, the subject IDs
# corresponding to the special values are given through getSpecialValues.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2020-01-28
#
# ========================================================================================

checkForSpecialValues <- function(dataFilePath, dirName){
  flaggedMsgs <- character()
  
  # Create data DF with columns of type numeric as of type character.
  dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, colClasses = "character")
  dataDF <- convertDataFrame(dataDF)
  columnNames <- colnames(dataDF)
  
  specialValues <- c("NAN", "INF", "-INF", "NULL")
  
  for (name in columnNames){
    if (any(is.nan(dataDF[ , name ]) | is.infinite(dataDF[ , name ]) | is.null(dataDF[ , name ])
            | (toupper(dataDF[ , name ]) %in% specialValues))){
      line <- paste(tags$span(class = "bold-category", "Special Values: Directory", dirName),
                    "- The file contains special values (NaN, Inf, -Inf, or NULL). 
                    Any special values should not be in the DATA file. Please use the
                    dropdown to the right for the subject IDs corresponding to the 
                    special values.")
      flaggedMsgs <- c(flaggedMsgs, line)
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