# getInvalidSiteCodes.R
#
# Purpose: If a tabular csv file contains invalid site codes, output the column 
# name and the subject IDs corresponding to the invalid site codes.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# ========================================================================================

getInvalidSiteCodes <- function(df, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Initialize vector of site codes and missing codes.
  validSiteCodes <- appendixDF %>% select(SITE_CODES) %>% filter(SITE_CODES != "")
  validMissingCodes <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  # Add missing codes to the list of valid site codes.
  validSiteCodes <- c(validSiteCodes$SITE_CODES, validMissingCodes$MISSING_CODES)
  
  # Platform specific site codes.
  
  # =======================================================================================
  
  locationDF <- data.frame(stringsAsFactors = FALSE)
  columnNames <- colnames(df)
  # Get the column names corresponding to SITE.
  splitColumnNames <- sapply(strsplit(columnNames, "_"), tail, 1)
  colNums <- which(splitColumnNames == "SITE")
    
  for (num in colNums){
    colName <- columnNames[num]
    siteCodes <- df[ , colName ]
    
    # Get platform code of site column name.
    platformCode <- unlist(strsplit(colName, "_"))[1]
    
    # Scenario # 1: Check for both regular and platform specific site codes.
    if (platformCode %in% colnames(platformSpecificDF)){
      if (!all(siteCodes %in% c(validSiteCodes, platformSpecificDF[ , platformCode ]))){
        # Get which indices of the column contain an invalid site code.
        indices <- which(!(siteCodes %in% 
                             c(validSiteCodes, platformSpecificDF[ , platformCode ])))
        
        col <- c(paste("Site Codes Check #1: Directory", dirName, "- Column", colName), 
                 df$SUBJECT[indices])
        # Pad empty strings to the column so each column of the data frame has 
        # the same length.
        locationDF <- cbind.fill(locationDF, col, fill = "")
      }
        
      # Flag if OTH is used as a site code.
      if (platformCode == "NPSY"){
        if (any(siteCodes == "OTH")){
          # Get which indices of the column contain an OTH site code.
          indices <- which(siteCodes == "OTH")
          
          col <- c(paste("Site Codes Check #2: Directory", dirName, "- Column", colName), 
                   df$SUBJECT[indices])
          # Pad empty strings to the column so each column of the data frame has
          # the same length.
          locationDF <- cbind.fill(locationDF, col, fill = "")
        }
      }
      
    }
    # Scenario # 2: Check for regular site codes only.
    else{
      if (!all(siteCodes %in% validSiteCodes)){
        # Get which indices of the column contain an invalid site code.
        indices <- which(!(siteCodes %in% validSiteCodes))
        
        col <- c(paste("Site Codes Check #1: Directory", dirName, "- Column", colName), 
                 df$SUBJECT[indices])
        # Pad empty strings to the column so each column of the data frame has
        # the same length.
        locationDF <- cbind.fill(locationDF, col, fill = "")
      }
    }
    
    
    # Check independent of either scenario: Flag if M_TBC or M_OTHER are used as site codes.
    if (any(siteCodes == "M_TBC") || any(siteCodes == "M_OTHER")){
      # Get which indices of the column contain an M_TBC or M_OTHER site code.
      indices <- sort(c(which(siteCodes == "M_TBC"), which(siteCodes == "M_OTHER")))
      
      col <- c(paste("Site Codes Check #3: Directory", dirName, "- Column", colName), 
               df$SUBJECT[indices])
      # Pad empty strings to the column so each column of the data frame has
      # the same length.
      locationDF <- cbind.fill(locationDF, col, fill = "")
    }
  }
  
  
  if (length(locationDF) > 0){
    # Remove the 1st column which contains all empty strings.
    locationDF <- locationDF[ , -1 ]
  }
  return (locationDF)
}

# [END]