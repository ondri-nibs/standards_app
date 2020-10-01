# checkDATAColumnNames.R
#
# Purpose: Check to ensure that all column names in the DATA file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# ==========================================================================================

checkDATAColumnNames <- function(dataDF, dirName){
  flaggedMsgs <- character()
  columnNames <- colnames(dataDF)
    
  # 1) Look for valid column names.
  if (columnNames[1] != "SUBJECT"){
    line <- paste(tags$span(class = "bold-category", 
                            "DATA Column Names: Directory", dirName),
                  "- SUBJECT column is missing and/or is not the 1st column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if (columnNames[2] != "VISIT"){
    line <- paste(tags$span(class = "bold-category", 
                            "DATA Column Names: Directory", dirName),
                  "- VISIT column is missing and/or is not the 2nd column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # Get a list of all column names.
  splitColumnNames <- sapply(strsplit(columnNames, "_"), tail, 1)
  
  if (all(splitColumnNames != "SITE")){
    line <- paste(tags$span(class = "bold-category", 
                            "DATA Column Names: Directory", dirName),
                  "- SITE column is missing.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if (all(splitColumnNames != "DATE")){
    line <- paste(tags$span(class = "bold-category", 
                            "DATA Column Names: Directory", dirName),
                  "- DATE column is missing.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (any(columnNames == "NO_TITLE")){
    colNums <- which(columnNames == "NO_TITLE")
    line <- paste(tags$span(class = "bold-category", 
                            "DATA Column Names: Directory", dirName),
                  "- The following column #'s have no column name:",
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  # 3) Check for any column name duplicates.
  flaggedMsgs <- c(flaggedMsgs, checkForColumnNameDuplicates(dataDF, dirName))
  
  
  return (flaggedMsgs)
}

# [END]