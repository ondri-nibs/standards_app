# checkLvl2DirNames.R
#
# Purpose: Check that all level 2 directories are named strictly using alphanumeric 
# characters and underscores. In addition, check that the directory names do not start
# with digits or underscores.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-25
#
# ==========================================================================================

checkLvl2DirNames <- function(listOfDirNames){
  flaggedMsgs <- character()

  # Test #1: Alphanumeric characters and underscores only.
  if (!all(grepl("^[a-zA-Z0-9_]+$", listOfDirNames))){
    indices <- which(!grepl("^[a-zA-Z0-9_]+$", listOfDirNames))
    line <- paste(tags$span(class = "bold-category", "Directory Names Check"),
                  "- The following level 2 directory names need to contain alphanumeric 
                  characters and underscores only:",
                  tags$span(class = "colour-category", 
                            paste(listOfDirNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
    
  # Test #2: Directory name does not start with digits or underscores.
  if (!all(grepl("^[[:alpha:]]$", substring(listOfDirNames, 1, 1)))){
    indices <- which(!grepl("^[[:alpha:]]$", substring(listOfDirNames, 1, 1)))
    line <- paste(tags$span(class = "bold-category", "Directory Names Check"), 
                  "- The following level 2 directory names need to start with an alpha
                  character:", tags$span(class = "colour-category", 
                                         paste(listOfDirNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  # Test #3: Message that uppercase letters only are preferred.
  # Check if any column names are not fully capitalized.
  uppercaseDirNames <- toupper(listOfDirNames)
  if (any(uppercaseDirNames != listOfDirNames)){
    indices <- which(uppercaseDirNames != listOfDirNames)
    line <- paste(tags$span(class = "bold-category", "Directory Names Check"), 
                  "- The following level 2 directory names contain lowercase letters.
                  It is recommended that all of its letters be fully capitalized:", 
                  tags$span(class = "colour-category", 
                            paste(listOfDirNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]