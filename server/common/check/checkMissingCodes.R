# checkMissingCodes.R
#
# Purpose: Check whether a tabular csv file contains valid missing codes wherever a 
# cell starts with "M_".  If not, the subject IDs corresponding to the missing code
# violations are given through getInvalidMissingCodes.R.
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# ========================================================================================

checkMissingCodes <- function(df, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # ========================================================================================
  
  flaggedMsgs <- character()
  columnNames <- colnames(df)
  
  for (name in columnNames){
    possibleMissingCodes <- grep("^M_", df[ , name ], value = TRUE)
    
    if (!all(possibleMissingCodes %in% validMissingCodes)){
      line <- paste(tags$span(class = "bold-category", "Missing Codes: Directory", dirName),
                    "- The file contains invalid missing codes. Please use the dropdown
                    to the right for the subject IDs corresponding to the invalid
                    missing codes.")
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