# checkFILELISTColumnNames.R
#
# Purpose: Check to ensure that all column names in the FILELIST file are valid and at the 
# right position.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# ==========================================================================================

checkFILELISTColumnNames <- function(lvl1DirPath){
  flaggedMsgs <- character()

  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  # Iterate through each lvl 2 directory.
  for (dirName in lvl2Dirs){
    dirMsgs <- character()
    
    lvl3DirPath <- paste0(lvl1DirPath, "/", dirName, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    filelistFilePath <- paste0(lvl3DirPath, "/", getFileName(lvl3DirPath, "FILELIST.csv"))
    
    filelistDF <- read.csv(filelistFilePath, stringsAsFactors = FALSE, check.names = FALSE)
    filelistDF <- convertDataFrame(filelistDF)
    columnNames <- colnames(filelistDF)
    
    # 1) Look for valid column names.
    if (columnNames[1] != "SUBJECT"){
      line <- paste(tags$span(class = "bold-category", 
                              "FILELIST Column Names: Directory", lvl3DirName),
                    "- SUBJECT column is missing and/or is not the 1st column.")
      dirMsgs <- c(dirMsgs, line)
    }
    
    if (columnNames[2] != "VISIT"){
      line <- paste(tags$span(class = "bold-category", 
                              "FILELIST Column Names: Directory", lvl3DirName),
                    "- VISIT column is missing and/or is not the 2nd column.")
      dirMsgs <- c(dirMsgs, line)
    }
    
    
    splitColumnNames <- sapply(strsplit(columnNames, "_"), tail, 1)
    
    if (all(splitColumnNames != "SITE")){
      line <- paste(tags$span(class = "bold-category", 
                              "FILELIST Column Names: Directory", lvl3DirName),
                    "- SITE column is missing.")
      dirMsgs <- c(dirMsgs, line)
    }
    
    if (all(splitColumnNames != "DATE")){
      line <- paste(tags$span(class = "bold-category", 
                              "FILELIST Column Names: Directory", lvl3DirName),
                    "- DATE column is missing.")
      dirMsgs <- c(dirMsgs, line)
    }
    
    if (tail(columnNames, 1) != "FILENAME"){
      line <- paste(tags$span(class = "bold-category", 
                              "FILELIST Column Names: Directory", lvl3DirName),
                    "- FILENAME column is missing and/or is not the last column.")
      dirMsgs <- c(dirMsgs, line)
    }
    
    
    # if remindd data then we can accept DEVICE_LOCATION as an option
    # 2) Check for any invalid column names.
    if (any(columnNames != "SUBJECT" & columnNames != "VISIT" & splitColumnNames != "SITE" &
            splitColumnNames != "DATE" & columnNames != "FILENAME" & columnNames != "DEVICE_LOCATION")){
      colNums <- which(columnNames != "SUBJECT" & columnNames != "VISIT" 
                       & splitColumnNames != "SITE" & splitColumnNames != "DATE" 
                       & columnNames != "FILENAME")
      line <- paste(tags$span(class = "bold-category", 
                              "FILELIST Column Names: Directory", lvl3DirName),
                    "- The following column #'s have an invalid column name:",
                    tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
      dirMsgs <- c(dirMsgs, line)
    }
    
    if (any(columnNames == "NO_TITLE")){
      colNums <- which(columnNames == "NO_TITLE")
      line <- paste(tags$span(class = "bold-category", 
                              "FILELIST Column Names: Directory", lvl3DirName),
                    "- The following column #'s have no column name:",
                    tags$span(class = "colour-category", paste(colNums, collapse = ", ")))
      dirMsgs <- c(dirMsgs, line)
    }
    
    
    # 3) Check column name syntax.
    dirMsgs <- c(dirMsgs, checkColumnNameSyntax(filelistDF, lvl3DirName))
    
    
    # 4) Check for any column name duplicates.
    dirMsgs <- c(dirMsgs, checkForColumnNameDuplicates(filelistDF, lvl3DirName))
    
    
    # Add a line break to separate text for other directories.
    if (length(dirMsgs) > 0){
      dirMsgs[length(dirMsgs)] <- HTML(paste(dirMsgs[length(dirMsgs)], "<br/>"))
    }
    flaggedMsgs <- c(flaggedMsgs, dirMsgs)
    
  }
  
  return (flaggedMsgs)
}

# [END]