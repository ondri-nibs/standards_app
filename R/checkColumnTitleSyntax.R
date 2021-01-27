# checkColumnTitleSyntax.R
#
# Purpose: Check whether a file satisfies the standard of containing proper 
# column title syntax.  This includes containing alphanumeric characters 
# and underscores only, starting with an alpha character only, and all 
# letters being fully capitalized (recommendation). If not, it will alert
# the user by returning a vector of warning text and outline which column 
# titles do not satisfy any of these requirements.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2018-10-11
#
# ==============================================================================

checkColumnTitleSyntax <- function(df){
  warningText <- character()
  numOfColumns <- ncol(df)
  columnTitles <- colnames(df)
  
  if (numOfColumns > 0){
    # Check whether any column titles contain alphanumeric and underscores only.
    if (any(!grepl("^[a-zA-Z0-9_]+$", columnTitles))){
      colNums <- which(!grepl("^[a-zA-Z0-9_]+$", columnTitles))
      colNums <- paste(colNums, collapse = ", ")
      
      # Give the column numbers of the column titles that are invalid.
      line <- paste(tags$span(class = "bold-category", "Column Title Syntax"), 
                    "- The following column #'s have titles that need to contain
                    alphanumeric characters and underscores only:", colNums)
      warningText <- c(warningText, line)
    }
    
    
    # Check whether any column titles start with an alpha character.
    if (any(!grepl("^[[:alpha:]]$", substring(columnTitles, 1, 1)))){
      colNums <- which(!grepl("^[[:alpha:]]$", substring(columnTitles, 1, 1)))
      colNums <- paste(colNums, collapse = ", ")
      
      # Give the column numbers of the column titles that are invalid.
      line <- paste(tags$span(class = "bold-category", "Column Title Syntax"), 
                    "- The following column #'s have titles that need to start with an
                    alpha character:", colNums)
      warningText <- c(warningText, line)
    }
    
    
    # Check whether any column title is not fully capitalized.
    if (any(!grepl("^[A-Z0-9_]+$", columnTitles))){
      colNums <- which(!grepl("^[A-Z0-9_]+$", columnTitles))
      colNums <- paste(colNums, collapse = ", ")
      
      # Give the column numbers of the column titles that are invalid.
      line <- paste(tags$span(class = "bold-category", "Column Title Syntax"), 
                    "- The following column #'s have titles that contain lowercase letters. 
                    It is recommended that all of its letters be fully capitalized:", colNums)
      warningText <- c(warningText, line)
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