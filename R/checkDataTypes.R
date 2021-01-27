# checkDataTypes.R
#
# Purpose: Check whether a DICT file lists the correct data type for specific column 
# labels. If not, the column labels corresponding to the data type violations are given
# through getInvalidDataTypes.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-10
#
# ========================================================================================

checkDataTypes <- function(dictDF, dirName){
  flaggedMsgs <- character()
  validDataTypes <- c("TEXT", "DATE", "CATEGORICAL", "ORDINAL", "NUMERIC", "MIXED")
  
  uppercaseColumnLabels <- toupper(dictDF$COLUMN_LABEL)
  uppercaseTypes <- toupper(dictDF$TYPE)
  
  # 1) Check if all data types are uppercase.
  if (any(!grepl("^[[:upper:]]+$", dictDF$TYPE))){
    line <- paste(tags$span(class = "bold-category", 
                            "Data Types Check #1: Directory", dirName),
                  "- There exists data types that should be in all capital letters. 
                  Please use the dropdown to the right for the column labels
                  corresponding to the lowercase data types.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # 2) Test for invalid data types.
  if (any(!(uppercaseTypes %in% validDataTypes))){
    line <- paste(tags$span(class = "bold-category", 
                            "Data Types Check #2: Directory", dirName),
                  "- There exists invalid data types. Please use the dropdown
                  to the right for the column labels corresponding to the
                  invalid data types. Options are: TEXT, DATE, CATEGORICAL, ORDINAL, 
                  NUMERIC, or MIXED (with permission from Neuroinformatics).")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # 3) Search for "MIXED" data types.
  if (any(uppercaseTypes == "MIXED")){
    line <- paste(tags$span(class = "bold-category", 
                            "Data Types Check #3: Directory", dirName),
                  "- There exists MIXED data types. Please use the dropdown to the
                  right for the column labels corresponding to type MIXED. The usage 
                  of type MIXED requires special permission from Neuroinformatics.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # 4) Search for data type corresponding to "SUBJECT" column label.
  if (uppercaseColumnLabels[1] == "SUBJECT" && uppercaseTypes[1] != "TEXT"){
    line <- paste(tags$span(class = "bold-category", 
                            "Data Types Check #4: Directory", dirName),
                  "- The SUBJECT column label should be of type TEXT.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  # Search for data type corresponding to "VISIT" column label.
  if (uppercaseColumnLabels[1] == "VISIT" && uppercaseTypes[1] != "CATEGORICAL"){
    line <- paste(tags$span(class = "bold-category", 
                            "Data Types Check #4: Directory", dirName),
                  "- The VISIT column label should be of type CATEGORICAL.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  splitColumnLabels <- sapply(strsplit(uppercaseColumnLabels, "_"), tail, 1)
  siteColNums <- which(splitColumnLabels == "SITE")
  dateColNums <- which(splitColumnLabels == "DATE")
  
  # Search for data type corresponding to "SITE" column label(s).
  if (uppercaseTypes[siteColNums] != "CATEGORICAL"){
    line <- paste(tags$span(class = "bold-category", 
                            "Data Types Check #4: Directory", dirName),
                  "- The SITE column label(s) should all be of type CATEGORICAL.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  # Search for data type corresponding to "DATE" column label(s).
  if (uppercaseTypes[dateColNums] != "DATE"){
    line <- paste(tags$span(class = "bold-category", 
                            "Data Types Check #4: Directory", dirName),
                  "- The DATE column label(s) should all be of type DATE.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]