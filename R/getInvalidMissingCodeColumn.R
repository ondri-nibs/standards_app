# getInvalidMissingCodeColumn.R
#
# Purpose: If a MISSING file contains invalid missing codes in its MISSING_CODE column,
# output the column name and the subject IDs corresponding to the invalid missing codes
# in the MISSING_CODE column only.
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-06
#
# ========================================================================================

getInvalidMissingCodeColumn <- function(missingDF, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # ========================================================================================
  
  locationDF <- data.frame(stringsAsFactors = FALSE)
  
  # 1) Check for regular missing codes.
  if (!(all(missingDF$MISSING_CODE %in% validMissingCodes))){
    # Get which indices of the column contain an invalid missing code.
    indices <- which(!(missingDF$MISSING_CODE %in% validMissingCodes))
    
    col <- c(paste("Missing Code Column Check #1: Directory", dirName, 
                   "- Column MISSING_CODE"), missingDF$SUBJECT[indices])
    # Pad empty strings to the column so each column of the data frame has
    # the same length.
    locationDF <- cbind.fill(locationDF, col, fill = "")
  }
  
  
  # Step 2: Check for special missing codes M_TBC and M_OTHER.
  if (any(missingDF$MISSING_CODE == "M_TBC") || any(missingDF$MISSING_CODE == "M_OTHER")){
    # Get which indices of the column contain an M_TBC or M_OTHER missing code.
    indices <- sort(c(which(missingDF$MISSING_CODE == "M_TBC"), 
                      which(missingDF$MISSING_CODE == "M_OTHER")))
    
    col <- c(paste("Missing Code Column Check #2: Directory", dirName, 
                   "- Column MISSING_CODE"), missingDF$SUBJECT[indices])
    # Pad empty strings to the column so each column of the data frame has
    # the same length.
    locationDF <- cbind.fill(locationDF, col, fill = "")
  }
  
  
  if (length(locationDF) > 0){
    # Remove the 1st column which contains all empty strings.
    locationDF <- locationDF[ , -1 ]
  }
  return (locationDF)
}

# [END]