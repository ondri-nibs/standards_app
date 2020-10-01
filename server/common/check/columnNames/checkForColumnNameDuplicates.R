# checkForColumnNameDuplicates.R
#
# Purpose: Check whether a tabular csv file satisfies the standard of containing no
# duplicate column names (case insensitive).
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-03
#
# ==========================================================================================

checkForColumnNameDuplicates <- function(df, dirName){
  flaggedMsgs <- character()
  namesChecked <- character()
  # Convert column names to uppercase for case insensitive comparison.
  columnNames <- toupper(colnames(df))
  
  for (name in columnNames){
    if (!(name %in% namesChecked)){
      colNums <- which(columnNames == name)
      numberOfDuplicates <- length(colNums)
      
      if (numberOfDuplicates > 1){
        line <- paste(tags$span(class = "bold-category", "Column Name Duplicates: 
                                Directory", dirName), 
                      "-", name, "(case insensitive)",
                      "is a column name that is duplicated at column #'s:", 
                      tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
        flaggedMsgs <- c(flaggedMsgs, line)
      }
      
      namesChecked <- c(namesChecked, name)
    }
  }
  
  return (flaggedMsgs)
}

# [END]