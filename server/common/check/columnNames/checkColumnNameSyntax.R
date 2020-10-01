# checkColumnNameSyntax.R
#
# Purpose: Check whether a tabular csv file satisfies the standard of containing proper 
# column name syntax. This includes:
#   1) Containing alphanumeric characters and underscores only.
#   2) Starting with an alpha character only.
#   3) All letters being fully capitalized.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-03
#
# ==========================================================================================

checkColumnNameSyntax <- function(df, dirName){
  flaggedMsgs <- character()
  columnNames <- colnames(df)
  
  # Check if any column names do not contain alphanumeric and underscores only.
  if (!all(grepl("^[a-zA-Z0-9_]+$", columnNames))){
    colNums <- which(!grepl("^[a-zA-Z0-9_]+$", columnNames))
    line <- paste(tags$span(class = "bold-category", "Column Name Syntax: Directory", dirName), 
                  "- The following column #'s have names that need to contain alphanumeric 
                  characters and underscores only:", 
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # Check if any column names do not start with an alpha character.
  if (!all(grepl("^[[:alpha:]]$", substring(columnNames, 1, 1)))){
    colNums <- which(!grepl("^[[:alpha:]]$", substring(columnNames, 1, 1)))
    line <- paste(tags$span(class = "bold-category", "Column Name Syntax: Directory", dirName), 
                  "- The following column #'s have names that need to start with an 
                  alpha character:", 
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # Check if any column names are not fully capitalized.
  uppercaseColumnNames <- toupper(columnNames)
  if (any(uppercaseColumnNames != columnNames)){
    colNums <- which(uppercaseColumnNames != columnNames)
    line <- paste(tags$span(class = "bold-category", "Column Name Syntax: Directory", dirName), 
                  "- The following column #'s have names that contain lowercase letters. 
                  It is recommended that all of its letters be fully capitalized:", 
                  tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  return (flaggedMsgs)
}