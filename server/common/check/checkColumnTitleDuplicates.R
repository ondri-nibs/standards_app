# checkColumnTitleDuplicates.R
#
# Purpose: Check whether a file satisfies the standard of containing no duplicate column
# titles (case insensitive). If not, it will alert the user by returning a vector of 
# warning text and outline which column titles have been duplicated and at what positions
# relative to the data frame.
#
# 
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date created: 2018-10-11
# Date modified: 2019-05-23
#
# ==========================================================================================

checkColumnTitleDuplicates <- function(df){
  warningText <- character()
  titlesChecked <- character()
  
  # Convert column titles to uppercase for case insensitive comparison.
  columnTitles <- toupper(colnames(df))
  
  for (title in columnTitles){
    if (!(title %in% titlesChecked)){
      colNums <- which(columnTitles == title)
      numberOfDuplicates <- length(colNums)
      colNums <- paste(colNums, collapse = ", ")
      
      # Print as output.
      if (numberOfDuplicates > 1){
        line <- paste(tags$span(class = "bold-category", "Column Title Duplicates"), 
                      "-", title, "(case insensitive)",
                      "is a column title that is duplicated at column #'s:", colNums)
        warningText <- c(warningText, line)
      }
      
      titlesChecked <- c(titlesChecked, title)
    }
  }
  
  # Add a line break to last element in vector if there is warning text, 
  # to separate from other warning text for other checkboxes.
  if (length(warningText) > 0){
    warningText[length(warningText)] <- HTML(paste
                                             (warningText[length(warningText)], 
                                              "<br/>"))
  }
  
  return (warningText)
}