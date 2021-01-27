# checkForRaggedRows.R
#
# Purpose: Check whether a tabular csv file contains ragged rows or not.
#
# Original author: Derek Beaton (dbeaton@research.baycrest.org)
# Modified by: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

checkForRaggedRows <- function(dfFilePath, dirName, fileType){
  flaggedMsgs <- character()
  seps <- rep(",", length(dfFilePath))
  
  ### THIS IS NOT A THOROUGH VALIDATION TOOL. WE NEED SOMETHING TO CHECK FOR CONSISTENT QUOTING...
  ## no header so I can read the whole file.
  tryCatch.res <- tryCatch(file.in <- read.table(text = readLines(dfFilePath, warn = FALSE), sep = seps, 
                                                 colClasses = "character",
                                                 header = F, quote = "\""),
                           error = function(e) e, warning = function(w) w)
  
  ## by reading the data in this way, it will catch when quotes are inconsistently used.
  if (class(tryCatch.res)[1] != "data.frame"){
    if(methods::is(tryCatch.res) == "simpleWarning" | methods::is(tryCatch.res) == "simpleError"){
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files (Ragged Rows): Directory", dirName),
                    "- The", fileType, "file may have ragged rows; please verify.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]