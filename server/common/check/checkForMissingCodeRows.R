# checkForMissingCodeRows.R
#
# Purpose: Detect if entire row(s) contain missing codes, except for the SITE and DATE 
# values. If this is the case, outline which participants should be moved to the MISSING
# file instead of being kept in the DATA file.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# ========================================================================================

checkForMissingCodeRows <- function(dataDF, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES
  
  # ========================================================================================
  
  flaggedMsgs <- character()
  participantList <- character()
  columnNames <- colnames(dataDF)
    
  # Get all SITE and DATE column names of the data DF.
  splitColumnNames <- sapply(strsplit(columnNames, "_"), tail, 1)
  siteColumnNames <- columnNames[ which(splitColumnNames == "SITE") ]
  dateColumnNames <- columnNames[ which(splitColumnNames == "DATE") ]
  dataDF <- dataDF[ , !(columnNames %in% c(siteColumnNames, dateColumnNames)) ]
    
  
  # Step 2: Transpose the DF to check missing codes by column.
  transposedDataDF <- as.data.frame(t(dataDF))
  colnames(transposedDataDF) <- as.character(unlist(transposedDataDF[ 1, ]))
  transposedDataDF = transposedDataDF[ -1, ]
    
  
  # Step 3: Check for columns that contain all missing codes.
  participantIDs <- colnames(transposedDataDF)
  # Omit participant IDs that are the empty string.
  participantIDs <- participantIDs[ participantIDs != "" ]
  for (id in participantIDs){
    if (all(transposedDataDF[ , id ] %in% validMissingCodes)){
      participantList <- c(participantList, id)
    }
  }
    
  
  # Step 4: Add participant list to flagged msgs for output.
  if (length(participantList) > 0){
    line <- paste(tags$span(class = "bold-category", 
                            "Missing Code Rows: Directory", dirName),
                  "- There are entire rows (omitting SITE and DATE) that contain missing 
                  codes. The following participants should be moved to a MISSING file
                  instead of being kept in the DATA file:", 
                  tags$span(class = "colour-category", 
                            paste(participantList, collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]