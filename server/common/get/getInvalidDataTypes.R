# getInvalidDataTypes.R
#
# Purpose: If a DICT file contains invalid data types, output the column name and the 
# column labels corresponding to the invalid data types.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-10
#
# =========================================================================================

getInvalidDataTypes <- function(dictDF, dirName){
  locationDF <- data.frame(stringsAsFactors = FALSE)
  validDataTypes <- c("TEXT", "DATE", "CATEGORICAL", "ORDINAL", "NUMERIC", "MIXED")
  
  uppercaseColumnLabels <- toupper(dictDF$COLUMN_LABEL)
  uppercaseTypes <- toupper(dictDF$TYPE)
  
  # 1) Check if all data types are uppercase.
  if (any(!grepl("^[[:upper:]]+$", dictDF$TYPE))){
    # Get the column labels corresponding to the uppercase violation.
    indices <- which(!grepl("^[[:upper:]]+$", dictDF$TYPE))
    col <- c(paste("Data Types Check #1: Directory", dirName, "- Column TYPE"), 
             dictDF$COLUMN_LABEL[indices])
    
    # Pad empty strings to the column so each column of the data frame has
    # the same length.
    locationDF <- cbind.fill(locationDF, col, fill = "")
  }
  
  
  # 2) Test for invalid data types.
  if (any(!(uppercaseTypes %in% validDataTypes))){
    # Get the column labels corresponding to the invalid type violation.
    indices <- which(!(uppercaseTypes %in% validDataTypes))
    col <- c(paste("Data Types Check #2: Directory", dirName, "- Column TYPE"), 
             dictDF$COLUMN_LABEL[indices])
    
    # Pad empty strings to the column so each column of the data frame has
    # the same length.
    locationDF <- cbind.fill(locationDF, col, fill = "")
  }
  
  
  # 3) Search for "MIXED" data types.
  if (any(uppercaseTypes == "MIXED")){
    # Get the column labels corresponding to the MIXED type violation.
    indices <- which(uppercaseTypes == "MIXED")
    col <- c(paste("Data Types Check #3: Directory", dirName, "- Column TYPE"), 
             dictDF$COLUMN_LABEL[indices])
    
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