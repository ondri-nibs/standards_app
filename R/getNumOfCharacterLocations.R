# getNumOfCharacterLocations.R
#
# Purpose: If a tabular csv file has content that contains more than 200 characters, 
# output the column name and the subject IDs/column labels/file names corresponding
# to the > 200 character violations.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =========================================================================================

getNumOfCharacterLocations <- function(df, dirName, fileType){
  locationDF <- data.frame(stringsAsFactors = FALSE)
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    if (any(nchar(df[ , name ]) > 200)){
      # Get which indices of the column contain more than 200 characters in text.
      indices <- which(nchar(df[ , name ]) > 200)
      col <- c(paste("Number of Characters: Directory", dirName, "- Column", name), 
               df[ indices, firstColumn[1] ])
      
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