# checkForCompressedFiles.R
#
# Purpose: Checks to see if there are compressed files in the current folder being checked.
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-13-08
#
# ========================================================================================

checkForCompressedFiles <- function(lvl1DirPath){
  volumes <- c(Home = fs::path_home(), getVolumes()())
  
  # creating a list of the file extensions found in folder
  flaggedMsgs <- character()
  extType <- c()
  
  FileNames <- list.files(path = lvl1DirPath)
  # list of common compressed files
  compressedFiles <- c("zip", "tar", "gz", "tar.gz")
  
  for (item in FileNames){
    ext <- file_ext(item)
    extType <- c(ext, extType)
  }
  
  # Warning message if there is a compressed file in the folder
  for (ext in extType){
    
    if (any( ext %in% compressedFiles)){
      flaggedMsgs <- paste(tags$span(class = "bold-category", "Warning: Directory - Compressed Files", basename(lvl1DirPath)),
                           "- Please note that the directory contains a compressed file. 
                                It is strongly recommended that this file be checked manually
                                as there are no computerized checks in place.")
      break
    }
  
  }
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]