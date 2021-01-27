# checkMissingCodeColumn.R
#
# Purpose: Check whether a MISSING file contains valid missing codes in its MISSING_CODE
# column. If not, the subject IDs corresponding to the missing code violations are given 
# through getInvalidMissingCodeColumn.R.
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-06
#
# ========================================================================================

checkMissingCodeColumn <- function(missingDF, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # ========================================================================================
  
  flaggedMsgs <- character()
  
  # 1) Check for regular missing codes.
  if (!(all(missingDF$MISSING_CODE %in% validMissingCodes))){
    line <- paste(tags$span(class = "bold-category", 
                            "Missing Code Column Check #1: Directory", dirName), 
                  "- The MISSING_CODE column contains invalid missing codes. Please use
                  the dropdown to the right for the subject IDs corresponding to the 
                  invalid missing codes.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # 2) Check for special missing codes M_TBC and M_OTHER.
  if (any(missingDF$MISSING_CODE == "M_TBC") || any(missingDF$MISSING_CODE == "M_OTHER")){
    line <- paste(tags$span(class = "bold-category", 
                            "Missing Code Column Check #2: Directory", dirName), 
                  "- The MISSING_CODE column uses missing code M_TBC and/or M_OTHER.
                  Please use the dropdown to the right for the subject IDs corresponding
                  to these missing codes. Use of M_TBC and/or M_OTHER requires approval 
                  from Neuroinformatics.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }

  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]