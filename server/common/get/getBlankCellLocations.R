# getBlankCellLocations.R
#
# Purpose: If a tabular csv file contains blank cells and/or NA's, output the column
# name and the subject IDs/column labels/file names corresponding to the blank cells.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =========================================================================================

getBlankCellLocations <- function(dfFilePath, dirName, fileType){
  locationDF <- data.frame(stringsAsFactors = FALSE)
  firstColumn <- getFirstColumn(fileType)
  
  possibleNAs <- c("NA", "N.A", ".", "N.A.", "N_A", "N/A")
  
  # Create data frame.
  df <- read.csv(dfFilePath, stringsAsFactors = FALSE, colClasses = "character")
  selection <- colnames(df) == ""
  colnames(df)[ selection ] <- "NO_TITLE"
  columnNames <- colnames(df)
  
  for (name in columnNames){
    # 1) Blank cells.
    if (any(nchar(df[ , name ]) == 0 & !is.na(df[ , name ]))){
      # Get the subject IDs/column labels/file names corresponding to the 
      # column that contain the empty string "".
      indices <- which(nchar(df[ , name ]) == 0 & !is.na(df[ , name ]))
      col <- c(paste("Blank Cells Check #1: Directory", dirName, "- Column", name),
               df[ indices, firstColumn[1] ])
      
      # Pad empty strings to the column so each column of the data frame has
      # the same length.
      locationDF <- cbind.fill(locationDF, col, fill = "")
    }
    
    
    # 2) NA (any instances) cells.
    if (any(is.na(df[ , name ]) | toupper(df[ , name ]) %in% possibleNAs)){
      # Get the subject IDs/column labels/file names corresponding to the 
      # column that contain any versions of NA.
      indices <- which(is.na(df[ , name ]) | toupper(df[ , name ]) %in% possibleNAs)
      col <- c(paste("Blank Cells Check #2: Directory", dirName, "- Column", name),
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