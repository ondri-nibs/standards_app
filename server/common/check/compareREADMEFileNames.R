# compareREADMEFileNames.R
#
# Purpose: This is a helper function that compares the file names in the README file 
# and the files in the directory containing the README file (for level 1 and 2).
#
# Author: Jeremy Tanuan (jahn@research.baycrest.org)
#
# Date: 2019-07-09
#
# =======================================================================================

compareREADMEFileNames <- function(readmeDF, dirPath, dirName){
  flaggedMsgs <- character()
  
  readmeFileNames <- readmeDF$FILE
  exclusionFiles = c(getFileName(dirPath, "README.csv"))
  dirFileNames <- setdiff(list.files(path = dirPath), exclusionFiles)
  
  # This is all file names in the directory that are not in the README file.
  missingFromCSV <- setdiff(dirFileNames, readmeFileNames)
  # This is all file names in the README file that are not in the directory.
  missingFromDir <- setdiff(readmeFileNames, dirFileNames)
  
  if (length(missingFromCSV) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            paste("README File Names Comparison: Directory", dirName)), 
                  "- File or folder names that are in the directory but not in the
                  README file:", 
                  tags$span(class = "colour-category", paste(missingFromCSV, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  if (length(missingFromDir) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            paste("README File Names Comparison: Directory", dirName)), 
                  "- File or folder names that are in the README file but not in the
                  directory:", 
                  tags$span(class = "colour-category", paste(missingFromDir, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  
  return (flaggedMsgs)
}

# [END]