# getInvalidPrecisionLevels.R
#
# Purpose: If a DATA file contains inconsistent precision levels, output the column 
# name and the subject IDs corresponding to the inconsistent precision levels.
# 
# 1) Mix of whole numbers and decimal numbers: Output which subject IDs have whole numbers.
# 2) Inconsistent number of decimal places if all numbers are decimal values:
#    Output which subject IDs do not match in decimal places compared to the 1st 
#    subject ID in the column.
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# =========================================================================================

getInvalidPrecisionLevels <- function(dataFilePath, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # ========================================================================================
  
  locationDF <- data.frame(stringsAsFactors = FALSE)
  
  # Create data DF with columns of type numeric as of type character.
  dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, colClasses = "character")
  dataDF <- convertDataFrame(dataDF)
  columnNames <- colnames(dataDF)
  
  for (name in columnNames){
    # Base version.
    values <- dataDF[ , name ]
    filteredDF <- dataDF[ , c("SUBJECT", name) ]
    filteredDF <- filteredDF[ !(values %in% validMissingCodes) & values != "" , ]
    values <- filteredDF[ , name ]
    
    # Varhandle library check.numeric() function
    if (all(check.numeric(values))){
      # Process values.
      values <- strsplit(values, "\\.")
      lengthOfSplitNumber <- lengths(values)
      
      # 1) Column contains mix of whole numbers and decimal numbers.
      if (!all(lengthOfSplitNumber == lengthOfSplitNumber[1])){
        indices <- which(lengthOfSplitNumber == 1)
        col <- c(paste("Precision Levels: Directory", dirName, "- Column", name),
                 filteredDF$SUBJECT[indices])
        
        # Pad empty strings to the column so each column of the data frame has
        # the same length.
        locationDF <- cbind.fill(locationDF, col, fill = "")
      }
      
      # 2) Column contains all decimal numbers but not consistent precision 
      # (number of decimal places).
      else if (all(lengthOfSplitNumber == 2)){
        numberOfDecimalPlaces <- nchar(sapply(values, "[", 2))
        if (!(all(numberOfDecimalPlaces == numberOfDecimalPlaces[1]))){
          indices <- which(numberOfDecimalPlaces != numberOfDecimalPlaces[1])
          col <- c(paste("Precision Levels: Directory", dirName, "- Column", name),
                   filteredDF$SUBJECT[indices])
          
          # Pad empty strings to the column so each column of the data frame has
          # the same length.
          locationDF <- cbind.fill(locationDF, col, fill = "")
        }
      }
    }
  }
  
  
  if (length(locationDF) > 0){
    # Remove the 1st column which contains all empty strings.
    locationDF <- locationDF[ , -1 ]
  }
  
  return (locationDF)
}

# [END]