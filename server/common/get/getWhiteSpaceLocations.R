# getWhiteSpaceLocations.R
#
# Purpose: If a tabular csv file contains leading or trailing whitespaces, output the
# column name and the subject IDs/column labels/file names corresponding to the whitespaces.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# ========================================================================================

getWhiteSpaceLocations <- function(df, dirName, fileType){
  locationDF <- data.frame(stringsAsFactors = FALSE)
  firstColumn <- getFirstColumn(fileType)
  columnNames <- colnames(df)
  
  for (name in columnNames){
    if (any(grepl("^\\s$", substring(df[ , name ], 1, 1)) |
            grepl("^\\s$", substring(df[ , name ], nchar(df[ , name ]), 
                                     nchar(df[ , name ]))))
        ){
      # Get which indices of the column contain leading or trailing whitespace.
      indices <- which((grepl("^\\s$", substring(df[ , name ], 1, 1)) == TRUE) |
                         (grepl("^\\s$", substring(df[ , name ], nchar(df[ , name ]), 
                                                   nchar(df[ , name ]))) == TRUE))
      col <- c(paste("White Spaces: Directory:", dirName, "- Column", name), 
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