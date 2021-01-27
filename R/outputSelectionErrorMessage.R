# outputSelectionErrorMessage.R
#
# Purpose: Create the server logic to output an error message to the screen
# when a folder and/or file(s) have not been selected by the user.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2018-02-12
#
# ==============================================================================

outputSelectionErrorMessage <- function(combinedWarningText){
  # Output an error message.
  errorMsg <- paste(
    tags$div(img(class = "caution", src = "www/caution.jpg"),
             tags$span(id = "error-title", 
                       "ERROR: A folder and/or file(s) have not been selected."))
             )
  
  # Add separator as 2nd line of text.
  separator <- paste(hr(class = "separator"))
  
  # Combine by placing at front of vector.
  combinedWarningText <- c(errorMsg, separator, combinedWarningText)
  
  # Combine all elements of error text vector into 1 output.
  combinedWarningText <- HTML(paste(combinedWarningText,
                                    collapse = "<br/>"))
  
  return (combinedWarningText)
}

# [END]