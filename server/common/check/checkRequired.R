# checkRequired.R
#
# Purpose: This is a helper function that compares the files in the current directory
# and the given required files of that directory.
#
# Author: Jeremy Tanuan (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# =======================================================================================

checkRequired <- function(dirPath, dirName, requiredFileTypes) {
  flaggedMsgs <- character()
  missingFromDir <- character()
  
  # Get a list of all file names (including directories).
  allFileNames <- list.files(path = dirPath)
  fileTypes <- sapply(strsplit(allFileNames, "_"), tail, 1)
  
  # This is all the file types that are required but are not in the directory
  missingFromDir <- setdiff(requiredFileTypes, fileTypes)
  
  if (length(missingFromDir) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            paste("Required Files: Directory", dirName)),
                  "- File types or folders that are required but not in the directory:",
                  tags$span(class = "colour-category", 
                            paste(missingFromDir, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  return (flaggedMsgs)
}

# [END]