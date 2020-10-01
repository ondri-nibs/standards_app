# checkForDuplicateFileNames.R
#
# Purpose: Check folders in all levels to see if there are any duplicate files
# (this is a non-tabular check).
#
# Author: Roberto Lenitni (rlentini@research.baycrest.org)
#
# Date: 2020-08-14
#
# =======================================================================================

checkForDuplicateFileNames <- function(lvl1DirPath){
  
  flaggedMsgs <- character()
  Basename_list <- c()
  # List of all the file names found in the package
  allFileNames <- list.files(path = lvl1DirPath, recursive = TRUE)
  
  # Warning message if there is a duplicate file name in the folder
  for (file in allFileNames){
    Basename_list <- c(basename(file), Basename_list)
    occurence_table <- table(Basename_list)
  }
  
  for (i in names(occurence_table)){
    if (occurence_table[i] > 1){
      line <- paste(tags$span(class = "bold-category", "Warning: Directory - Duplicate Name", i),
                    "- Please note that the file name is used multiple times.")
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