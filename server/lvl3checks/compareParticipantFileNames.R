# compareParticipantFileNames.R
#
# Purpose: Compare the file names in the FILELIST file and the literal contents of each 
# level 3 directory (DATAFILES folder) to ensure that they match.
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-07-02
#
# ==========================================================================================

compareParticipantFileNames <- function(filelistDF, lvl3DirPath, lvl3DirName){
  flaggedMsgs <- character()
    
  # Get the MISSING file name and DICT file name if either exist.
  exclusionFiles <- c(getFileName(lvl3DirPath, "FILELIST.csv"), 
                      getFileName(lvl3DirPath, "MISSING.csv"),
                      getFileName(lvl3DirPath, "DICT.csv"))
  
  lvl3FileNames <- list.files(path = lvl3DirPath)
  participantFileNames <- setdiff(lvl3FileNames, exclusionFiles)
  
  # This is all file names in the folder that are not in the FILELIST file.
  missingFromCSV <- setdiff(participantFileNames, filelistDF$FILENAME)
  # This is all file names in the FILELIST file that are not in the folder.
  missingFromDir <- setdiff(filelistDF$FILENAME, participantFileNames)
    
  if (length(missingFromCSV) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            paste("Participant File Names Comparison: Directory", 
                                  lvl3DirName)), 
                  "- File names that are in the DATAFILES folder but not in 
                  the FILELIST file:", 
                  tags$span(class = "colour-category",
                            paste(missingFromCSV, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  if (length(missingFromDir) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            paste("Participant File Names Comparison: Directory", 
                                  lvl3DirName)), 
                  "- File names that are in the FILELIST file but not in 
                  the DATAFILES folder:", 
                  tags$span(class = "colour-category",
                            paste(missingFromDir, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  
  return (flaggedMsgs)
}

# [END]