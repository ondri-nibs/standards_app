# checkREADMEColumnNames.R
#
# Purpose: Check to ensure that all column names in the README file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# ==========================================================================================

checkDICTColumnNames <- function(dictDF, dirName){
  flaggedMsgs <- character()
  columnNames <- colnames(dictDF)
    
  # 1) Look for valid column names.
  if (columnNames[1] != "COLUMN_LABEL"){
    line <- paste(tags$span(class = "bold-category", 
                            "DICT Column Names: Directory", dirName),
                  "- COLUMN_LABEL column is missing and/or is not the 1st column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (columnNames[2] != "DESCRIPTION"){
    line <- paste(tags$span(class = "bold-category", 
                            "DICT Column Names: Directory", dirName),
                  "- DESCRIPTION column is missing and/or is not the 2nd column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (columnNames[3] != "TYPE"){
    line <- paste(tags$span(class = "bold-category", 
                            "DICT Column Names: Directory", dirName),
                  "- TYPE column is missing and/or is not the 3rd column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }

  
  if (columnNames[4] != "VALUES"){
    line <- paste(tags$span(class = "bold-category", 
                            "DICT Column Names: Directory", dirName),
                  "- VALUES column is missing and/or is not the 4th column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  # 2) Check for any invalid column names.
  if (any(columnNames != "COLUMN_LABEL" & columnNames != "DESCRIPTION" &
          columnNames != "TYPE" & columnNames != "VALUES")){
    colNums <- which(columnNames != "COLUMN_LABEL" & columnNames != "DESCRIPTION" &
                     columnNames != "TYPE" & columnNames != "VALUES")
    line <- paste(tags$span(class = "bold-category", 
                            "DICT Column Names: Directory", dirName),
                  "- The following column #'s have an invalid column name:",
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if (any(columnNames == "NO_TITLE")){
    colNums <- which(columnNames == "NO_TITLE")
    line <- paste(tags$span(class = "bold-category", 
                            "DICT Column Names: Directory", dirName),
                  "- The following column #'s have no column name:",
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # 3) Check column name syntax.
  flaggedMsgs <- c(flaggedMsgs, checkColumnNameSyntax(dictDF, dirName))
  
  
  # 4) Check for any column name duplicates.
  flaggedMsgs <- c(flaggedMsgs, checkForColumnNameDuplicates(dictDF, dirName))
  
  
  return (flaggedMsgs)
}

# [END]