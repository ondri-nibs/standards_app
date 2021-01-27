# checkForEncapsulation.R
#
# Purpose: Check whether a tabular csv file contains proper encapsulation, such 
# that all data cells DO NOT have the following:
# 
#   1) Double quotes on 1 side only.
#   2) Double quotes within a data cell.
#   3) No single quotes unless it is an apostrophe.
#
# If not, the subject IDs/column labels/file names corresponding to the encapsulation
# violations are given through getNumOfCharacterLocations.R.
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# =======================================================================================

checkForEncapsulation <- function(df, dirName, fileType){
  flaggedMsgs <- character()
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    # 1) Check for double quotes on 1 side only.
    if (any(xor(grepl("^\"$", substring(df[ , name ], 1, 1)),
                grepl("^\"$", substring(df[ , name ], nchar(df[ , name ]), 
                                        nchar(df[ , name ])))))
        ){
      line <- paste(tags$span(class = "bold-category", 
                              "Encapsulation Check #1: Directory", dirName), 
                    "- Content in column", name, "has double quotes on 1 side only. 
                    Please use the dropdown to the right for the", firstColumn[2],
                    "corresponding to this violation.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    
    # 2) Check for double quotes within data cell.
    if (any(grepl("\"", substring(df[ , name ], 2, nchar(df[ , name ]) - 1)))){
      line <- paste(tags$span(class = "bold-category", 
                              "Encapsulation Check #2: Directory", dirName), 
                    "- Content in column", name, "should not have double quotes within 
                    the data cell. Please use the dropdown to the right for the", 
                    firstColumn[2], "corresponding to this violation.")
      flaggedMsgs <- c(flaggedMsgs, line)
      
    }
    
    
    # 3) Check for single quotes within data cell.
    if (any(grepl("'", substring(df[ , name ], 2, nchar(df[ , name ]) - 1)))){
      line <- paste(tags$span(class = "bold-category", 
                              "Encapsulation Check #3: Directory", dirName), 
                    "- Content in column", name, "has single quotes. Please use the dropdown
                    to the right for the", firstColumn[2], "corresponding to this violation.
                    Single quotes are allowed but not recommended, unless it is an
                    apostrophe.")
      flaggedMsgs <- c(flaggedMsgs, line)
      
    }
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]