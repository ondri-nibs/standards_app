# compareFileNames.R
#
# Purpose: Compare a list of file names to the level 1 directory name to
# ensure that the cohort code, visit code, platform code, and date all match.
#
# This function only runs if all file names and the level 1 directory name are valid.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-23
#
# ==========================================================================================

compareFileNames <- function(listOfFileNames, lvl1DirName, dirName, 
                             lvl = NULL, lvl2DirName = NULL){
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  splitFileNames <- strsplit(listOfFileNames, "_")
  
  ondriCodes <- sapply(splitFileNames, function(x) x[1])
  cohortCodes <- sapply(splitFileNames, function(x) x[2])
  visitCodes <- sapply(splitFileNames, function(x) x[3])
  platformCodes <- sapply(splitFileNames, function(x) x[4])
  dates <- sapply(splitFileNames, function(x) x[length(x) - 1])
  
  
  flaggedMsgs <- character()
  
  # Test #1: ONDRI codes.
  if (any(ondriCodes != splitDirName[1])){
    indices <- which(ondriCodes != splitDirName[1])
    line <- paste(tags$span(class = "bold-category", 
                            "File Names Comparison: Directory", dirName),
                  "- The following file names have an ONDRI code that does not match
                  the ONDRI code in the level 1 directory name:", 
                  tags$span(class = "colour-category", 
                            paste(listOfFileNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  # Test #2: Cohort codes.
  if (any(cohortCodes != splitDirName[2])){
    indices <- which(cohortCodes != splitDirName[2])
    line <- paste(tags$span(class = "bold-category", 
                            "File Names Comparison: Directory", dirName),
                  "- The following file names have a cohort code that does not match
                  the cohort code in the level 1 directory name:", 
                  tags$span(class = "colour-category", 
                            paste(listOfFileNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
   
  # Test #3: Visit codes.
  if (any(visitCodes != splitDirName[3])){
    indices <- which(visitCodes != splitDirName[3])
    line <- paste(tags$span(class = "bold-category", 
                            "File Names Comparison: Directory", dirName),
                  "- The following file names have a visit code that does not match
                  the visit code in the level 1 directory name:", 
                  tags$span(class = "colour-category", 
                            paste(listOfFileNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  # Test #4: Platform codes.
  if (any(platformCodes != splitDirName[4])){
    indices <- which(platformCodes != splitDirName[4])
    line <- paste(tags$span(class = "bold-category", 
                            "File Names Comparison: Directory", dirName),
                  "- The following file names have a platform code that does not match
                  the platform code in the level 1 directory name:", 
                  tags$span(class = "colour-category", 
                            paste(listOfFileNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  # Test #5: Date.
  if (any(dates != splitDirName[length(splitDirName) - 1])){
    indices <- which(dates != splitDirName[length(splitDirName) - 1])
    line <- paste(tags$span(class = "bold-category", 
                            "File Names Comparison: Directory", dirName),
                  "- The following file names have a date that does not match
                  the date in the level 1 directory name:", 
                  tags$span(class = "colour-category", 
                            paste(listOfFileNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  # Test #6: Optional subplatform or dataset code.
  optionalCodes <- sapply(splitFileNames, function(x) x[5])
  # This means that subplatform or dataset code exists.
  if (!all(optionalCodes == dates)){
    if (any(optionalCodes != splitDirName[5])){
      indices <- which(optionalCodes != splitDirName[5])
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Comparison: Directory", dirName),
                    "- The following file names have an optional subplatform or dataset code
                    that does not match the subplatform or dataset code in the level 1 
                    directory name:", 
                    tags$span(class = "colour-category", 
                              paste(listOfFileNames[indices], collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  }
  
  # Test #7: Level 2 directory name in file name if level 2 or level 3.
  if (!is.null(lvl)){
    truncatedFileNames <- lapply(splitFileNames, head, -2)
    truncatedFileNames <- sapply(truncatedFileNames, paste, collapse = "_")
    
    # Truncated file names should end with level 2 directory name, as date and file type
    # have been removed.
    if (!all(endsWith(truncatedFileNames, lvl2DirName))){
      indices <- which(!endsWith(truncatedFileNames, lvl2DirName))
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Comparison: Directory", dirName),
                    "- The following file names need to contain the level 2 directory name
                    before the date:", 
                    tags$span(class = "colour-category",
                              paste(listOfFileNames[indices], collapse = ", ")))
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