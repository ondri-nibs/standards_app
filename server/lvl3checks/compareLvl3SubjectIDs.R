# compareLvl3SubjectIDs.R
#
# Purpose: Compare the subject ID in the SUBJECT column and the subject ID in the file 
# name in the FILENAME column (of the FILELIST file) to ensure that they match.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# ==========================================================================================

compareLvl3SubjectIDs <- function(filelistDF, lvl3DirName){
  flaggedMsgs <- character()
  
  splitIDs <- lapply(strsplit(filelistDF$FILENAME, "_"), head, 3)
  subjectIDs <- sapply(splitIDs, paste, collapse = "_")
  
  if (!all(subjectIDs == filelistDF$SUBJECT)){
    indices <- which(subjectIDs != filelistDF$SUBJECT)
    line <- paste(tags$span(class = "bold-category", 
                            paste("Subject IDs Comparison: Directory", lvl3DirName)),
                  "- The following subject IDs in the SUBJECT column do not 
                  match with their respective subject ID in the FILENAME column
                  (of the FILELIST file):",
                  tags$span(class = "colour-category",
                            paste(filelistDF$SUBJECT[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  
  return (flaggedMsgs)
}

# [END]