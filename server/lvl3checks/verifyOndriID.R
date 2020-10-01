# verifyOndriID.R
#
# Purpose: Verify that each participant file name in the level 3 directory 
# (DATAFILES folder) has a valid ONDRI ID.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# ==========================================================================================

verifyOndriID <- function(lvl3DirPath, lvl3DirName){
  flaggedMsgs <- character()
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Valid ONDRI codes.
  validOndriCodes <- appendixDF %>% select(ONDRI_CODES) %>% filter(ONDRI_CODES != "")
  validOndriCodes <- validOndriCodes$ONDRI_CODES
  
  # Initialize vector of file types.
  validFileTypes <- appendixDF %>% select(FILE_TYPES) %>% filter(FILE_TYPES != "")
  validFileTypes <- validFileTypes$FILE_TYPES
  # Get file names of specific file types.
  exclusionFiles <- unlist(sapply(validFileTypes, getFileName, dirPath = lvl3DirPath,
                                  USE.NAMES = FALSE))
  
  lvl3FileNames <- list.files(path = lvl3DirPath)
  participantFileNames <- setdiff(lvl3FileNames, exclusionFiles)
  ondriCode <- sapply(strsplit(participantFileNames, "_"), head, 1)
  
  if (!all(ondriCode %in% validOndriCodes)){
    indices <- which(!(ondriCode %in% validOndriCodes))
    line <- paste(tags$span(class = "bold-category", 
                            paste("ONDRI ID Verification: Directory", lvl3DirName)),
                  "- The following participant file names in the DATAFILES folder do not 
                  contain a valid ONDRI ID:", 
                  tags$span(class = "colour-category",
                            paste(participantFileNames[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  return (flaggedMsgs)
}

# [END]