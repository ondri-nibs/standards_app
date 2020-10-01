# getInvalidDateFormat.R
#
# Purpose: If a tabular csv file contains invalid date format, output the column 
# name and the subject IDs corresponding to the invalid date format. A valid date is:
# 
#   YYYYMMMDD, valid years (4 digit number), valid months (JAN to DEC), 
#   and valid days (1-30 or 1-31 depending on the month, 1-28 (for February), 
#   and 1-29 (for February on leap years)).
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# ========================================================================================

getInvalidDateFormat <- function(df, dirName, study_name, format){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # =======================================================================================
  
  locationDF <- data.frame(stringsAsFactors = FALSE)
  columnNames <- colnames(df)
  
  # Get the column names corresponding to DATE.
  splitColumnNames <- sapply(strsplit(columnNames, "_"), tail, 1)
  colNums <- which(splitColumnNames == "DATE")
  
  for (num in colNums){
    colName <- columnNames[num]
    
    # Base version: Get dates that are neither missing codes nor blank cells.
    dates <- df[ , colName ]
    # error catching to stop non character date columns from crashing app
    if (class(dates) == "character"){
      filteredDF <- df[ , c("SUBJECT", colName) ]
      filteredDF <- filteredDF[ !(dates %in% validMissingCodes) & dates != "" , ]
      dates <- filteredDF[ , colName ]
    
    # Convert date strings to date objects.
    dateObjects <- as.Date(dates, format = "%Y%b%d")
    
    }
    
    # Check for date objects that turn NA and dates that are not exactly 9 characters long.
    if (any(is.na(dateObjects)) || any(nchar(dates) != 9)){
      indices <- sort(union(which(is.na(dateObjects)), which(nchar(dates) != 9)))
      col <- c(paste("Date Format: Directory", dirName, "- Column", colName), 
               filteredDF$SUBJECT[indices])
      
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