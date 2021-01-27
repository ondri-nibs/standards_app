# checkColumnNameFormat.R
#
# Purpose: Check whether the DATA/FILELIST/MISSING file satisfies the standard of having a 
# SITE and DATE column in which its platform code and subplatform code matches the platform
# code and subplatform code in the level 1 directory name, as follows:
#
# [REQUIRED PLATFORM CODE]_[OPTIONAL SUBPLATFORM CODE]_SITE_[OPTIONAL SUPPLEMENTAL CODE]
# [REQUIRED PLATFORM CODE]_[OPTIONAL SUBPLATFORM CODE]_DATE_[OPTIONAL SUPPLEMENTAL CODE]
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-08-12
#
# ==========================================================================================

checkColumnNameFormat <- function(df, lvl1DirName, dirName){
  flaggedMsgs <- character()
  
  columnNames <- colnames(df)
  columnTypes <- sapply(strsplit(columnNames, "_"), tail, 1)
  colNums <- which(columnTypes == "SITE" | columnTypes == "DATE")
  
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  platformCode <- splitDirName[4]
  subplatformCode <- splitDirName[5]
  
  for (num in colNums){
    variable <- columnTypes[num]
    colName <- columnNames[num]
    splitColName <- unlist(strsplit(colName, "_"))
    
    # Column name is missing required platform code.
    if (length(splitColName) < 2){
      # Give the column number of the column name that is invalid.
      line <- paste(tags$span(class = "bold-category", 
                              "Column Name Format: Directory", dirName), 
                    "- Column", colName, "does not follow the correct format:",
                    paste0("[REQUIRED PLATFORM CODE]_[OPTIONAL SUBPLATFORM CODE]_", 
                           variable, "_[OPTIONAL SUPPLEMENTAL CODE]"))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    else{
      if (splitColName[1] != platformCode){
        # Give the column number of the column name that is invalid.
        line <- paste(tags$span(class = "bold-category", 
                                "Column Name Format: Directory", dirName), 
                      "- Column", colName, "has a platform code that does not match the 
                      platform code in the level 1 directory name.")
        flaggedMsgs <- c(flaggedMsgs, line)
      }
      
      # Check only if subplatform code exists.
      if (subplatformCode != "MINIMUM" && subplatformCode != "FULL" &&
          is.na(as.Date(subplatformCode, format = "%Y%b%d"))){
        if (splitColName[2] != subplatformCode){
          # Give the column number of the column name that is invalid.
          line <- paste(tags$span(class = "bold-category", 
                                  "Column Name Format: Directory", dirName), 
                        "- Column", colName, "has a subplatform code that does not match the 
                        subplatform code in the level 1 directory name.")
          flaggedMsgs <- c(flaggedMsgs, line)
        }
      }
    }
    
  }
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]