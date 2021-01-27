# outputMissingErrorMsg.R
#
# Purpose: Create the server logic to output an error message to the screen when a 
# selected level 1 directory and/or participant ID file and/or transfer ID file are missing.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# ==========================================================================================

outputMissingErrorMsg <- function(flaggedMsgs, lvl){
  # Output an error message.
  errorMsg <- paste(
    tags$div(img(class = "caution", src = "www/caution.jpg"),
             tags$span(id = "error-title", 
                       "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing."))
  )
  
  # Add separator as 2nd line of text.
  separator <- paste(hr(class = "separator"))
  
  # Combine by placing at front of vector.
  flaggedMsgs <- c(errorMsg, separator, flaggedMsgs)
  
  # Combine all elements of error text vector into 1 output.
  flaggedMsgs <- HTML(paste(flaggedMsgs, collapse = "<br/>"))
  
  return (flaggedMsgs)
}

# [END]