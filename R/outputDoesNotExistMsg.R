# outputDoesNotExistMsg.R
#
# Purpose: Create the server logic to output an error message to the screen when a
# specific file (such as DATA or DICT) does not exist for any particular directory.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-30
#
# ==========================================================================================

outputDoesNotExistMsg <- function(flaggedMsgs, typePhrase, lvl){
  # Output an error message.
  # 1) If level 1.
  if (lvl == 1){
    errorMsg <- paste(
      tags$div(img(class = "caution", src = "www/caution.jpg"),
               tags$span(id = "error-title", 
                         "ERROR: A", typePhrase, "does not exist in the level 1 directory.
                         No checks can be performed."))
    )
  }
  # if (lvl == 2 || lvl == 3)
  else{
    errorMsg <- paste(
      tags$div(img(class = "caution", src = "www/caution.jpg"),
               tags$span(id = "error-title", 
                         "ERROR: A", typePhrase, "does not exist in any level", lvl, 
                         "directory. No checks can be performed."))
    )
  }
  
  
  # Add separator as 2nd line of text.
  separator <- paste(hr(class = "separator"))
  
  # Combine by placing at front of vector.
  flaggedMsgs <- c(errorMsg, separator, flaggedMsgs)
  
  # Combine all elements of error text vector into 1 output.
  flaggedMsgs <- HTML(paste(flaggedMsgs, collapse = "<br/>"))
  
  return (flaggedMsgs)
}

# [END]