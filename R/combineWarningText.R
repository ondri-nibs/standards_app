# combineWarningText.R
#
# Purpose: Create the server logic to filter existing warning text, and then
# add onto it by adding extra lines including the total number of errors.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2018-09-06
#
# ==============================================================================

combineWarningText <- function(combinedWarningText){
  # Remove all elements in warning text vector with empty string.
  selection <- (combinedWarningText == "")
  combinedWarningText <- combinedWarningText[ !selection ]
  
  # Get the total number of errors, by excluding elements with
  # a line break.
  selection <- (combinedWarningText == paste(br()))
  numberOfErrors <- length(combinedWarningText [ !selection ])
  
  # Give congratulatory message if no errors were found.
  if (numberOfErrors == 0){
    # Add "Analysis Complete" as 1st line of text.
    # Add number of errors found as 2nd line of text.
    # Add disclaimer as 3rd line of text.
    analysisCompleteMsg <- paste(
      tags$div(img(class = "checkmark", src = "www/checkmark.jpg"), 
               tags$span(id = "analysis-complete-title", 
                         "Analysis Complete"),
               br(),
               tags$span(id = "error-number-title", numberOfErrors, 
                         "error(s) found in total."),
               br(),
               img(class = "smiley", src = "www/smiley.png"),
               tags$span(id = "congratulatory-title", 
                         "CONGRATULATIONS: The file has passed all standards!"))
               )
  }
  # Otherwise, add disclaimer to message.
  else{
    # Add "Analysis Complete" as 1st line of text.
    # Add number of errors found as 2nd line of text.
    # Add disclaimer as 3rd line of text.
    analysisCompleteMsg <- paste(
      tags$div(img(class = "checkmark", src = "www/checkmark.jpg"), 
               tags$span(id = "analysis-complete-title", 
                         "Analysis Complete"),
               br(),
               tags$span(id = "error-number-title", numberOfErrors, 
                         "error(s) found in total."))
               )
  }
  
  
  # Add separator as 4th line of text.
  separator <- paste(hr(class = "separator"))
  
  # Combine by placing at front of vector.
  combinedWarningText <- c(analysisCompleteMsg, 
                           separator, 
                           combinedWarningText)
  
  # Combine all elements of warning text vector for every checkbox
  # into 1 output.
  combinedWarningText <- HTML(paste(combinedWarningText, 
                                    collapse = "<br/>"))
  
  return (combinedWarningText)
}

# [END]