# getInvalidMissingCodes.R
#
# Purpose: If a tabular csv file contains invalid missing codes, output the column 
# name and the subject IDs corresponding to the invalid missing codes.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# ========================================================================================

getInvalidMissingCodes <- function(df, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # ========================================================================================
  
  locationDF <- data.frame(stringsAsFactors = FALSE)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    possibleMissingCodes <- grepl("^M_", df[ , name ])
    
    # Base version: Filter for possible missing codes.
    filteredDF <- df[ possibleMissingCodes , c("SUBJECT", name) ]
    colMissingCodes <- filteredDF[ , name ]
    filteredDF <- filteredDF[ !(colMissingCodes %in% validMissingCodes) , ]
    
    if (nrow(filteredDF) > 0){
      col <- c(paste("Missing Codes: Directory", dirName, "- Column", name), 
               filteredDF$SUBJECT)
      # Pad empty strings to the column so each column of the data frame has
      # the same length.
      locationDF <- cbind.fill(locationDF, col, fill = "")
    }
  }
  
  
  if (length(locationDF) > 0){
    # Remove the 1st column which contains all empty strings.
    locationDF <- locationDF[ , -1 ]
  }
  
  return (locationDF)
}

# [END]