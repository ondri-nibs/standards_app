# checkForTransferIDs.R
#
# Purpose: Check whether a DATA/FILELIST/MISSING file contain transfer IDs. If so, 
# location of these transfer IDs are given through getTransferIDLocations.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2020-01-28
#
# ========================================================================================

checkForTransferIDs <- function(df, transferIDDF, dirName, fileType){
  flaggedMsgs <- character()
  
  if (any(df$SUBJECT %in% transferIDDF$TRANSFER_ID)){
    line <- paste(tags$span(class = "bold-category", "Transfer IDs: Directory", dirName), 
                  "- There are subject IDs in the", fileType, "file that are transfer IDs. 
                  Please use the dropdown to the right for these subject IDs. Please use
                  the transfer ID file to get their correct ID and double check their 
                  data including site code.")
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]