# getCommaLocations.R
#
# Purpose: If a tabular csv file contains commas, output the column name and the subject
# IDs/column labels/file names corresponding to the commas.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =========================================================================================

getCommaLocations <- function(df, dirName, fileType){
  locationDF <- data.frame(stringsAsFactors = FALSE)
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    if (any(grepl(",", df[ , name ]))){
      # Get the subject IDs/column labels/file names corresponding to the 
      # column that contain the comma(s).
      indices <- which(grepl(",", df[ , name ]) == TRUE)
      col <- c(paste("Commas: Directory", dirName, "- Column", name), 
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