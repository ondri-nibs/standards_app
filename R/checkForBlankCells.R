# checkForBlankCells.R
#
# Purpose: Check whether a tabular csv file contains no blank cells (which includes NA).
# If not, the subject IDs/column labels/file names corresponding to the blank cells
# are given through getBlankCellLocations.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

checkForBlankCells <- function(dfFilePath, dirName, fileType){
  flaggedMsgs <- character()
  firstColumn <- getFirstColumn(fileType)
  
  possibleNAs <- c("NA", "N.A", ".", "N.A.", "N_A", "N/A")
  
  # Create duplicate df.
  df <- read.csv(dfFilePath, stringsAsFactors = FALSE, colClasses = "character")
  selection <- colnames(df) == ""
  colnames(df)[ selection ] <- "NO_TITLE"
  columnNames <- colnames(df)
  
  # 1) Blank cells.
  for (name in columnNames){
    if (any(nchar(df[ , name ]) == 0 & !is.na(df[ , name ]))){
      line <- paste(tags$span(class = "bold-category", 
                              "Blank Cells Check #1: Directory", dirName),
                    "- The file contains blank cells. Please use the dropdown to the right 
                    for the", firstColumn[2], "corresponding to the blank cells.")
      flaggedMsgs <- c(flaggedMsgs, line)
      break
    }
  }
    
  # 2) NA (any versions) cells.
  for (name in columnNames){
    if (any(is.na(df[ , name ]) | toupper(df[ , name ]) %in% possibleNAs)){
      line <- paste(tags$span(class = "bold-category", 
                              "Blank Cells Check #2: Directory", dirName),
                    "- The file contains an instance of NA. Any instances of NA should 
                    not be in the file. Please use the dropdown to the right for the",
                    firstColumn[2], "corresponding to the instances of NA.")
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