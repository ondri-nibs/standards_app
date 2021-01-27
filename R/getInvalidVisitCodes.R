# getInvalidVisitCodes.R
#
# Purpose: If a tabular csv file contains invalid visit codes, output the column 
# name and the subject IDs corresponding to the invalid visit codes.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# ========================================================================================

getInvalidVisitCodes <- function(dfFilePath, dirName, lvl1DirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  # Initialize vector of visit codes.
  validVisitCodes <- appendixDF %>% select(VISIT_CODES) %>% filter(VISIT_CODES != "")
  validVisitCodes <- validVisitCodes$VISIT_CODES
  
  # =======================================================================================
  
  locationDF <- data.frame(stringsAsFactors = FALSE)
  
  # Create duplicate df with content of VISIT column of type character.
  df <- read.csv(dfFilePath, stringsAsFactors = FALSE, colClasses = "character")
  df <- convertDataFrame(df)
  
  # Get the visit code.
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  visitCode <- splitDirName[3]
  
  if (any(df$VISIT != visitCode)){
    # Get which indices of the column do not match with the visit code in the level 1
    # directory name.
    indices <- which(df$VISIT != visitCode)
    col <- c(paste("Visit Codes: Directory", dirName, "- Column VISIT"), 
             df$SUBJECT[indices])
    
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