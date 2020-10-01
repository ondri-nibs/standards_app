# getTransferIDLocations.R
#
# Purpose: If a DATA/FILELIST/MISSING file contains transfer IDs, output the subject IDs 
# corresponding to the transfer IDs.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2020-01-28
#
# ========================================================================================

getTransferIDLocations <- function(df, transferIDDF, dirName){
  locationDF <- data.frame(stringsAsFactors = FALSE)

  if (any(df$SUBJECT %in% transferIDDF$TRANSFER_ID)){
    indices <- which(df$SUBJECT %in% transferIDDF$TRANSFER_ID)
    col <- c(paste("Transfer IDs: Directory", dirName, "- Column SUBJECT"),
             df$SUBJECT[indices])
    
    # Pad empty strings to the column so each column of the data frame has
    # the same length.
    locationDF <- cbind.fill(locationDF, col, fill = "")
  }
  
  
  if (length(locationDF) > 0){
    # Remove the 1st column which contains all empty strings.
    locationDF <- locationDF[ , -1 ]
  }
  
  return (locationDF)
}

# [END]