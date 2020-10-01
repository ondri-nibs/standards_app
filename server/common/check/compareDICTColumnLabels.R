# compareDICTColumnLabels.R
#
# Purpose: This is a helper function that compares the column labels in the DICT file 
# and the column names in the DATA file.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-10
#
# =======================================================================================

compareDICTColumnLabels <- function(dictDF, dataDF, dirName){
  flaggedMsgs <- character()
  
  dictColumnLabels <- dictDF$COLUMN_LABEL
  dataColumnNames <- colnames(dataDF)
  
  # This is all column labels in the DICT file that are not in the DATA file.
  missingFromData<- setdiff(dictColumnLabels, dataColumnNames)
  # This is all column labels in the DATA file that are not in the DICT file.
  missingFromDict <- setdiff(dataColumnNames, dictColumnLabels)
  
  if (length(missingFromData) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            paste("DICT Column Labels Comparison: Directory", dirName)), 
                  "- Column labels that are in the DICT file but not in the DATA file:",
                  tags$span(class = "colour-category", 
                            paste(missingFromData, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  if (length(missingFromDict) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            paste("DICT Column Labels Comparison: Directory", dirName)), 
                  "- Column labels that are in the DATA file but not in the DICT file:",
                  tags$span(class = "colour-category", 
                            paste(missingFromDict, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  
  return (flaggedMsgs)
}

# [END]