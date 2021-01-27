# checkPrecisionLevels.R
#
# Purpose: Check whether the DATA file contains consistent precision levels across a 
# numeric column (IGNORING MISSING CODES AND BLANK CELLS). If not, the subject IDs
# corresponding to the inconsistent precision are given through getInvalidPrecisionLevels.R.
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# =========================================================================================

checkPrecisionLevels <- function(dataFilePath, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # =======================================================================================
  
  flaggedMsgs <- character()
  
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
    
    # Varhandle library check.numeric() function.
    if (all(check.numeric(values))){
      # Process values.
      values <- strsplit(values, "\\.")
      lengthOfSplitNumber <- lengths(values)
      
      # 1) Column contains mix of whole numbers and decimal numbers.
      if (!all(lengthOfSplitNumber == lengthOfSplitNumber[1])){
        line <- paste(tags$span(class = "bold-category", 
                                "Precision Levels: Directory", dirName), 
                      "- Column", name, "contains a mix of whole numbers and decimal 
                      numbers. Please use the dropdown to the right for the subject IDs
                      corresponding to whole numbers.")
        flaggedMsgs <- c(flaggedMsgs, line)
      }
      
      # 2) Column contains all decimal values but not consistent precision 
      # (number of decimal places).
      else if (all(lengthOfSplitNumber == 2)){
        numberOfDecimalPlaces <- nchar(sapply(values, "[", 2))
        if (!all(numberOfDecimalPlaces == numberOfDecimalPlaces[1])){
          line <- paste(tags$span(class = "bold-category", 
                                  "Precision Levels: Directory", dirName), 
                        "- Column", name, "does not have consistent precision 
                        (number of decimal places). Precision for subject ID",
                        filteredDF$SUBJECT[1], "does not match with the precision 
                        for other subject IDs. Please use the dropdown to the right
                        for these subject IDs.")
          flaggedMsgs <- c(flaggedMsgs, line)
        }
      }
    }
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]