# checkFileExt.R
#
# Purpose: Check if all the files in the level 1 directory with no extension are folders.
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-07-15
#
# ==========================================================================================

checkFileExt <- function(lvl1DirPath){
  flaggedMsgs <- character()
  
  lvl1FileNames <- list.files(path = lvl1DirPath)
  lvl2DirNames <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  nonDirFiles <- setdiff(lvl1FileNames, 
                         c(lvl2DirNames, list.files(path = lvl1DirPath,
                                                    pattern = "\\.[A-z]+$")))
  
  if (length(nonDirFiles) > 0){
    line <- paste(tags$span(class = "bold-category", "File Extensions Check"),
                  "- Files in the level 1 directory that have no extension and are 
                  not a folder:", 
                  tags$span(class = "colour-category", paste(nonDirFiles, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]