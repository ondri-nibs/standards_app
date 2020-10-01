# checkEncoding.R
#
# Purpose: Check whether a tabular csv file is encoded in the broadly accepted formats 
# of ASCII or UTF-8; UTF-8 is preferred and suggested.
#
# Original author: Derek Beaton (dbeaton@research.baycrest.org)
# Modified by: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

checkEncoding <- function(dfFilePath, dirName, fileType){
  flaggedMsgs <- character()
  seps <- rep(",", length(dfFilePath))
  
  enc.guess.res <- guess_encoding(dfFilePath) # If it's not ASCII or UTF-8 it's an error.
  probabilityMatrix <- as.matrix(enc.guess.res)
  
  if (!all(enc.guess.res$encoding %in% c("ASCII","UTF-8"))){
    probabilityValues <- character()
    for (index in 1:nrow(probabilityMatrix)){
      probabilityValues <- c(probabilityValues, 
                             paste(paste(probabilityMatrix[ index , ]), collapse = ": "))
    } 
    
    line <- paste(tags$span(class = "bold-category", 
                            "Required Files (Encoding): Directory", dirName),
                  "- Probability of the", fileType, "file not exclusively ASCII or UTF-8. 
                  The encoding probabilities are:", 
                  tags$span(class = "colour-category", paste(probabilityValues, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  ### THIS IS NOT A THOROUGH VALIDATION TOOL. WE NEED SOMETHING TO CHECK FOR CONSISTENT QUOTING...
  ## no header so I can read the whole file.
  tryCatch.res <- tryCatch(file.in <- read.table(text = readLines(dfFilePath, warn = FALSE), sep = seps, 
                                                 colClasses = "character",
                                                 header = F, quote = "\""),
                           error = function(e) e, warning = function(w) w)
  
  ## by reading the data in this way, it will catch when quotes are inconsistently used.
  if (class(tryCatch.res)[1] != "data.frame"){
    if(is(tryCatch.res) == "simpleWarning" | is(tryCatch.res) == "simpleError"){
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files (Encoding): Directory", dirName),
                    "- The", fileType, "file may have ragged rows; please verify.
                    Cannot check for non UTF-8 encoding in", fileType, "file.")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  }
  else{
    utf.res <- lapply(file.in, validUTF8)
    utf.tests <- unlist(lapply(utf.res, sum)) == nrow(file.in)
    if(!all(utf.tests)){
      colNums <- which(!utf.tests)
      line <- paste(tags$span(class = "bold-category", 
                              "Required Files (Encoding): Directory", dirName),
                    "- There is non UTF-8 encoding for the", fileType, "file at column #'s:", 
                    tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
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