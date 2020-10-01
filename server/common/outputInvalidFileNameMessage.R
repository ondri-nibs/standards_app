# outputInvalidFileNameMessage.R
#
# Purpose: Create the server logic to output an error message to the screen
# when any of the file(s) selected have an incorrect file name.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-31
#
# ==============================================================================

outputInvalidFileNameMessage <- function(combinedWarningText){
  # Output an error message.
  errorMsg <- paste(
    tags$div(img(class = "caution", src = "caution.jpg"),
             tags$span(id = "error-title", 
                       "ERROR: One or more files selected have an incorrect file name."))
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