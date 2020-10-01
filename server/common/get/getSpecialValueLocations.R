# getSpecialValueLocations.R
#
# Purpose: If a DATA file contains special values such as NaN (not a number), 
# Inf (infinity), -Inf (negative infinity), or NULL (no value), output the column
# name and the subject IDs corresponding to the special values.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2020-01-28
#
# ========================================================================================

getSpecialValueLocations <- function(dataFilePath, dirName){
  locationDF <- data.frame(stringsAsFactors = FALSE)
  
  # Create data DF with columns of type numeric as of type character.
  dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE, colClasses = "character")
  dataDF <- convertDataFrame(dataDF)
  columnNames <- colnames(dataDF)
  
  specialValues <- c("NAN", "INF", "-INF", "NULL")
  
  for (name in columnNames){
    if (any(is.nan(dataDF[ , name ]) | is.infinite(dataDF[ , name ]) | is.null(dataDF[ , name ])
            | (toupper(dataDF[ , name ]) %in% specialValues))){
      indices <- which(is.nan(dataDF[ , name ]) | is.infinite(dataDF[ , name ]) 
                       | is.null(dataDF[ , name ]) 
                       | (toupper(dataDF[ , name ]) %in% specialValues))
      col <- c(paste("Special Values: Directory", dirName, "- Column", name),
               dataDF$SUBJECT[indices])
      
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