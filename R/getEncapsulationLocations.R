# getEncapsulationLocations.R
#
# Purpose: If a tabular csv file contains any violations of encapsulation, output 
# the column name and the subject IDs/column labels/file names corresponding to
# these violations.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# ========================================================================================

getEncapsulationLocations <- function(df, dirName, fileType){
  locationDF <- data.frame(stringsAsFactors = FALSE)
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    # 1) Check for double quotes on 1 side only.
    if (any(xor(grepl("^\"$", substring(df[ , name ], 1, 1)),
                grepl("^\"$", substring(df[ , name ], nchar(df[ , name ]), 
                                        nchar(df[ , name ])))))){
      # Get which indices of the column contain double quotes on 1 side only.
      indices <- which(xor(grepl("^\"$", substring(df[ , name ], 1, 1)),
                           grepl("^\"$", substring(df[ , name ], nchar(df[ , name ]), 
                                                   nchar(df[ , name ])))) == TRUE)
      col <- c(paste("Encapsulation Check #1: Directory", dirName, "- Column", name), 
               df[ indices, firstColumn[1] ])
      
      # Pad empty strings to the column so each column of the data frame has
      # the same length.
      locationDF <- cbind.fill(locationDF, col, fill = "")
    }
    
    
    # 2) Check for double quotes within data cell.
    if (any(grepl("\"", substring(df[ , name ], 2, nchar(df[ , name ]) - 1)))){
      # Get which indices of the column contain double quotes within the data cell.
      indices <- which(grepl("\"", substring(df[ , name ], 2, 
                                             nchar(df[ , name ]) - 1)) == TRUE)
      col <- c(paste("Encapsulation Check #2: Directory", dirName, "- Column", name),
               df[ indices, firstColumn[1] ])
      
      # Pad empty strings to the column so each column of the data frame has
      # the same length.
      locationDF <- cbind.fill(locationDF, col, fill = "")
    }
    
    
    # 3) Check for single quotes within data cell.
    if (any(grepl("'", substring(df[ , name ], 2, nchar(df[ , name ]) - 1)))){
      # Get which indices of the column contain single quotes
      # within the data cell.
      indices <- which(grepl("'", substring(df[ , name ], 2, 
                                            nchar(df[ , name ]) - 1)) == TRUE)
      col <- c(paste("Encapsulation Check #3: Directory", dirName, "- Column", name), 
               df[ indices, firstColumn[1] ])
      
      # Pad empty strings to the column so each column of the data frame has
      # the same length.
      locationDF <- cbind.fill(locationDF, col, fill = "")
    }
  }
  
  
  if (length(locationDF) > 0){
    # Remove the 1st column which contains all empty strings.
    locationDF <- locationDF[ , -1 ]
  }
  return (locationDF)
}

# [END]