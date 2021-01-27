# checkDateFormat.R
#
# Purpose: Check whether a tabular csv file contains valid date format. A valid date is:
# 
#   YYYYMMMDD, valid years (4 digit number), valid months (JAN to DEC), 
#   and valid days (1-30 or 1-31 depending on the month, 1-28 (for February), 
#   and 1-29 (for February on leap years)).
# 
# If not, the subject IDs corresponding to the date format violations are given through 
# getInvalidDateFormat.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# ========================================================================================

checkDateFormat <- function(df, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # =======================================================================================
  
  flaggedMsgs <- character()
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
    
    # Check if the entire column contains missing codes and/or blank cells.
    if (length(dateObjects) == 0){
      line <- paste(tags$span(class = "bold-category", "Date Format: Directory", dirName),
                           "- Cannot check date format as all values in Column",
                           colName, "are missing codes and/or blank cells.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    # Check for date objects that turn NA and dates that are not exactly 9 characters long.
    else if (any(is.na(dateObjects)) || any(nchar(dates) != 9)){
      line <- paste(tags$span(class = "bold-category", "Date Format: Directory", dirName),
                    "- Column", colName, "has date(s) that either do not contain the 
                    correct date format YYYYMMMDD or do not contain a valid day, month, 
                    or year. Please use the dropdown to the right for the subject IDs 
                    corresponding to the invalid dates.")
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