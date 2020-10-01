# checkMISSINGColumnNames.R
#
# Purpose: Check to ensure that all column names in the MISSING file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-06
#
# ==========================================================================================

checkMISSINGColumnNames <- function(missingDF, dirName){
  flaggedMsgs <- character()
  columnNames <- colnames(missingDF)
    
  # 1) Look for valid column names.
  if (columnNames[1] != "SUBJECT"){
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- SUBJECT column is missing and/or is not the 1st column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if (columnNames[2] != "VISIT"){
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- VISIT column is missing and/or is not the 2nd column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # Get a list of all column names.
  splitColumnNames <- sapply(strsplit(columnNames, "_"), tail, 1)
  
  if (all(splitColumnNames != "SITE")){
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- SITE column is missing.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if (all(splitColumnNames != "DATE")){
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- DATE column is missing.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if ((tail(columnNames, 2)[1]) != "MISSING_CODE"){
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- MISSING_CODE column is missing and/or is not the 2nd last column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if (tail(columnNames, 1) != "DESCRIPTION"){
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- DESCRIPTION column is missing and/or is not the last column.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # 2) Check for any invalid column names.
  if (any(columnNames != "SUBJECT" & columnNames != "VISIT" & splitColumnNames != "SITE" &
          splitColumnNames != "DATE" & columnNames != "MISSING_CODE" &
          columnNames != "DESCRIPTION")){
    colNums <- which(columnNames != "SUBJECT" & columnNames != "VISIT" 
                     & splitColumnNames != "SITE" & splitColumnNames != "DATE" 
                     & columnNames != "MISSING_CODE" & columnNames != "DESCRIPTION")
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- The following column #'s have an invalid column name:",
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  if (any(columnNames == "NO_TITLE")){
    colNums <- which(columnNames == "NO_TITLE")
    line <- paste(tags$span(class = "bold-category", 
                            "MISSING Column Names: Directory", dirName),
                  "- The following column #'s have no column name:",
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # 3) Check column name syntax.
  flaggedMsgs <- c(flaggedMsgs, checkColumnNameSyntax(missingDF, dirName))
  
  
  # 4) Check for any column name duplicates.
  flaggedMsgs <- c(flaggedMsgs, checkForColumnNameDuplicates(missingDF, dirName))
  
  
  return (flaggedMsgs)
}

# [END]