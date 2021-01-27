# combineFlaggedMsgs.R
#
# Purpose: Create the server logic to filter existing messages, and then add onto it by 
# adding extra lines including the total number of errors.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# ==========================================================================================

combineFlaggedMsgs <- function(flaggedMsgs, typePhrase, lvl){
  # Remove all elements in character vector with empty string.
  selection <- (flaggedMsgs == "")
  flaggedMsgs <- flaggedMsgs[ !selection ]
  
  # Get the total number of errors, by excluding elements with a line break and elements
  # acknowledging the existence of a SUP file (in lvl 1 and 2) or a DICT file (in lvl 3).
  selection <- (flaggedMsgs == paste(br()) |
                grepl("Please note that", flaggedMsgs))
  numOfErrors <- length(flaggedMsgs [ !selection ])
  
  # Give congratulatory message if no errors were found.
  # Add "Analysis Complete" as 1st line of text.
  # Add number of errors found as 2nd line of text.
  if (numOfErrors == 0){
    analysisCompleteMsg <- paste(
      tags$div(img(class = "checkmark", src = "www/checkmark.jpg"), 
               tags$span(id = "analysis-complete-title", 
                         "Analysis Of", typePhrase, "Complete"),
               br(),
               tags$span(id = "error-number-title", numOfErrors, 
                         "error(s) found in total."),
               br(),
               img(class = "smiley", src = "www/smiley.png"),
               tags$span(id = "congratulatory-title", 
                         paste("CONGRATULATIONS: Level", lvl, 
                               "has passed all standards!")))
               )
  }
  else{
    analysisCompleteMsg <- paste(
      tags$div(img(class = "checkmark", src = "www/checkmark.jpg"), 
               tags$span(id = "analysis-complete-title", 
                         "Analysis Of", typePhrase, "Complete"),
               br(),
               tags$span(id = "error-number-title", numOfErrors, 
                         "error(s) found in total."))
               )
  }
  
  
  # Add separator as 4th line of text.
  separator <- paste(hr(class = "separator"))
  
  # Combine by placing at front of vector.
  flaggedMsgs <- c(analysisCompleteMsg, separator, flaggedMsgs)
  
  # Combine all elements of character vector for every checkbox into 1 output.
  flaggedMsgs <- HTML(paste(flaggedMsgs, collapse = "<br/>"))
  
  return (flaggedMsgs)
}

# [END]