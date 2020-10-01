# checkSiteCodes.R
#
# Purpose: Check whether a tabular csv file contains valid site codes. If not,
# the subject IDs corresponding to the site code violations are given through 
# getInvalidSiteCodes.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# =======================================================================================

checkSiteCodes <- function(df, dirName){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Initialize vector of site codes and missing codes.
  validSiteCodes <- appendixDF %>% select(SITE_CODES) %>% filter(SITE_CODES != "")
  validMissingCodes <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  # Add missing codes to the list of valid site codes.
  validSiteCodes <- c(validSiteCodes$SITE_CODES, validMissingCodes$MISSING_CODES)
  # Platform specific site codes.
  platformSpecificDF <- read.csv("appendix/platformspecificSiteCodes.csv",
                                 stringsAsFactors = FALSE, colClasses = "character")
  
  # =======================================================================================
  
  flaggedMsgs <- character()
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
        line <- paste(tags$span(class = "bold-category", 
                                "Site Codes Check #1: Directory", dirName),
                      "- Column", colName, "has invalid site code(s). Please use the
                      dropdown to the right for the subject IDs corresponding to the 
                      invalid site codes.")
        flaggedMsgs <- c(flaggedMsgs, line)
      }
        
      # Flag if OTH is used as a site code.
      if (platformCode == "NPSY"){
        if (any(siteCodes == "OTH")){
          line <- paste(tags$span(class = "bold-category", 
                                  "Site Codes Check #2: Directory", dirName),
                        "- Column", colName, "uses site code OTH. Please use the 
                        dropdown to the right for the subject IDs corresponding to 
                        site code OTH. Use of OTH requires approval from Neuroinformatics.")
          flaggedMsgs <- c(flaggedMsgs, line)
        }
      }
    }
    # Scenario # 2: Check for regular site codes only.
    else{
      if (!all(siteCodes %in% validSiteCodes)){
        line <- paste(tags$span(class = "bold-category", 
                                "Site Codes Check #1: Directory", dirName),
                      "- Column", colName, "has invalid site code(s). Please use the
                      dropdown to the right for the subject IDs corresponding to the 
                      invalid site codes.")
        flaggedMsgs <- c(flaggedMsgs, line)
      }
    }
    
    
    # Check independent of either scenario: Flag if M_TBC or M_OTHER are used as site codes.
    if (any(siteCodes == "M_TBC") || any(siteCodes == "M_OTHER")){
      line <- paste(tags$span(class = "bold-category", 
                              "Site Codes Check #3: Directory", dirName),
                    "- Column", colName, "uses missing code M_TBC and/or M_OTHER.
                    Please use the dropdown to the right for the subject IDs corresponding 
                    to these missing codes. Use of M_TBC and/or M_OTHER requires approval 
                    from Neuroinformatics.")
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