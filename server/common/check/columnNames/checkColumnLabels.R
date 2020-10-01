# checkColumnLabels.R
# 
# Purpose: Check whether a DICT file's column labels match in exact order with the 
# column names in the DATA file, and that no column labels are duplicated. If not,
# the column labels corresponding to either of these violations are given
# through getInvalidColumnLabels.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-10
#
# ========================================================================================

checkColumnLabels <- function(dictDF, dataDF, dirName){
  flaggedMsgs <- character()
  # Get the column names of the data DF for comparison.
  dataColList <- colnames(dataDF)
  # Convert column labels in dict DF to uppercase for case insensitive comparison.
  uppercaseColumnLabels <- toupper(dictDF$COLUMN_LABEL)
  
  # 1) COLUMN_LABEL column exists but the number of rows in the DICT file is not equal to
  # the number of columns in the DATA file.
  if (length(dictDF$COLUMN_LABEL) != length(dataColList)){
    line <- paste(tags$span(class = "bold-category", 
                            "Column Labels Check: Directory", dirName),
                  "- The number of column labels in the COLUMN_LABEL column in the DICT
                  file is not equal to the number of column names in the DATA file.")
    flaggedMsgs <- c(flaggedMsgs, line) 
  }
  
  
  # 2) Column names in the DATA file do not match with its corresponding column label in 
  # COLUMN_LABEL column in the DICT file at the specific index.
  else{
    if (any(dictDF$COLUMN_LABEL != dataColList)){
      colNums <- which(dictDF$COLUMN_LABEL != dataColList)
      line <- paste(tags$span(class = "bold-category", 
                              "Column Labels Check: Directory", dirName),
                    "- The following column #'s in the DATA file have names that do not 
                    match with their corresponding column label in COLUMN_LABEL column 
                    in the DICT file:", tags$span(class = "colour-category", 
                                                  paste(colNums, collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  }
  
  
  # 3) Find duplicated column labels (case insensitive) if they exist.
  duplicatedColumnLabels <- unique(uppercaseColumnLabels[ duplicated(uppercaseColumnLabels) ])
  if (length(duplicatedColumnLabels) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            "Column Labels Check: Directory", dirName), 
                  "- There are column labels in the DICT file that are duplicated. 
                  The following column labels correspond to these dates:",
                  tags$span(class = "colour-category", 
                            paste(duplicatedColumnLabels, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]