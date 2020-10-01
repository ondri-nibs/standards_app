# compareColumnNames.R
#
# Purpose: Check whether a MISSING file's SUBJECT, VISIT, SITE, and DATE column names 
# match in order exactly with the SUBJECT, VISIT, SITE, and DATE column names present
# in the DATA file or the FILELIST file.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-06
#
# ========================================================================================

compareColumnNames <- function(missingDF, otherDF, dirName, fileType){
  # Get all SITE and DATE column names of the MISSING DF.
  splitColumnNames <- sapply(strsplit(colnames(missingDF), "_"), tail, 1)
  siteColumnNames <- colnames(missingDF)[ which(splitColumnNames == "SITE") ]
  dateColumnNames <- colnames(missingDF)[ which(splitColumnNames == "DATE") ]
  missingColList <- c("SUBJECT", "VISIT", siteColumnNames, dateColumnNames)
  
  # Get all SITE and DATE column names of the DATA or FILELIST DF.
  splitColumnNames <- sapply(strsplit(colnames(otherDF), "_"), tail, 1)
  siteColumnNames <- colnames(otherDF)[ which(splitColumnNames == "SITE") ]
  dateColumnNames <- colnames(otherDF)[ which(splitColumnNames == "DATE") ]
  otherColList <- c("SUBJECT", "VISIT", siteColumnNames, dateColumnNames)
  
  
  flaggedMsgs <- character()
  
  missingFromMISSING <- setdiff(otherColList, missingColList)
  missingFromOther <- setdiff(missingColList, otherColList)
  
  if (length(missingFromMISSING) != 0 || length(missingFromOther) != 0){
    # 1) All column names in otherColList exist in missingColList.
    if (length(missingFromMISSING) != 0){
      line <- paste(tags$span(class = "bold-category", 
                              "Column Names Comparison: Directory", dirName),
                    "- Column names that are in the", fileType, "file but not in the 
                    MISSING file:", 
                    tags$span(class = "colour-category",
                              paste(missingFromMISSING, collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # 2) Column names in missingColList that are not (and should not be) in otherColList.
    if (length(missingFromOther) != 0){
      line <- paste(tags$span(class = "bold-category", 
                              "Column Names Comparison: Directory", dirName),
                    "- Column names that are in the MISSING file but not in the", 
                    fileType, "file:", 
                    tags$span(class = "colour-category",
                              paste(missingFromOther, collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  }
  else{
    # At this point, we know that the column lists match, but we need to check that their
    # ordering are identical.
    missingColList <- colnames(missingDF)[(colnames(missingDF) %in% missingColList)]
    otherColList <- colnames(otherDF)[(colnames(otherDF) %in% otherColList)]
    
    if (any(missingColList != otherColList)){
      colPositions <- which(missingColList != otherColList)
      line <- paste(tags$span(class = "bold-category", 
                              "Column Names Comparison: Directory", dirName),
                    "- Only including SUBJECT, VISIT, SITE, and DATE column names, 
                    the ordering of the column names in the MISSING file and the",
                    fileType, "file are inconsistent. They are inconsistent at the
                    following positions: ", 
                    tags$span(class = "colour-category", paste(colPositions, collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  }
  
  return (flaggedMsgs)
}

# [END]