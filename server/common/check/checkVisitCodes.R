# checkVisitCodes.R
#
# Purpose: Check whether a tabular csv file contains valid visit codes. If not,
# the subject IDs corresponding to the visit code violations are given through 
# getInvalidVisitCodes.R.
#
# NOTE: This function does check for leading zeros in the visit codes.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# =========================================================================================

checkVisitCodes <- function(dfFilePath, lvl1DirName, dirName){

  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Initialize vector of visit codes.
  validVisitCodes <- appendixDF %>% select(VISIT_CODES) %>% filter(VISIT_CODES != "")
  validVisitCodes <- validVisitCodes$VISIT_CODES
  
  # =======================================================================================
  
  flaggedMsgs <- character()
  
  # Create duplicate df with content of VISIT column of type character.
  df <- read.csv(dfFilePath, stringsAsFactors = FALSE, colClasses = "character")
  df <- convertDataFrame(df)
  
  # Get the visit code.
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  visitCode <- splitDirName[3]
    
  # 1) Check to ensure that the visit code in the level 1 directory name is valid.
  if (!(visitCode %in% validVisitCodes)){
    line <- paste(tags$span(class = "bold-category", "Visit Codes: Directory", dirName), 
                  "- The visit code in the level 1 directory name is invalid.
                  Valid visit codes are: 01, 02, 03, 04, 05, 06, 07, or 08.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  # 2) If the visit code in 1) is valid, check that all visit codes in the VISIT 
  # column match up with the visit code in the level 1 directory name.
  else{
    if (any(df$VISIT != visitCode)){
      line <- paste(tags$span(class = "bold-category", "Visit Codes: Directory", dirName), 
                    "- There exists visit codes in column VISIT that do not match
                    with the visit code in the level 1 directory name. Please use the
                    dropdown to the right for the subject IDs corresponding to the
                    invalid visit codes.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  }
    
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]