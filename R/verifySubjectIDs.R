# verifySubjectIDs.R
#
# Purpose: Verify the subject IDs in the DATA/FILELIST/MISSING file with the 
# following checks:
# 
#   1) Check that each subject ID in the DATA/FILELIST/MISSING file is in the participant
#   ID file.
#   2) For whichever subject IDs exist in the DATA/FILELIST/MISSING file, check that
#   their cohort code is correct.
#
#
# MISSING file:
#   3) Check for duplicate subject IDs in the MISSING file.
#
# DATA/FILELIST file:
#   3) Check that each ID in the participant ID file with cohort code specified by the
#   file name exists in the DATA/FILELIST file (or the MISSING file in lvl 1, 2, and/or 3 
#   if they exist).
#   4) Check for duplicate subject IDs in the DATA/FILELIST file
#   (or the MISSING file in lvl 1, 2, and/or 3 if they exist).
#
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# ==========================================================================================

verifySubjectIDs <- function(df, participantIDDF, lvl1DirName, dirName, fileType,
                             lvl1DirPath = NULL, lvl2DirPath = NULL, lvl3DirPath = NULL){
  flaggedMsgs <- character()
  
  # Get the cohort code.
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  cohortCode <- splitDirName[2]
      
  # 1) Check that each subject ID in the DATA/FILELIST/MISSING file is in the participant
  # ID file.
  if (!all(df$SUBJECT %in% participantIDDF$SUBJECT)){
    indices <- which(!(df$SUBJECT %in% participantIDDF$SUBJECT))
    line <- paste(tags$span(class = "bold-category", 
                            paste("Subject IDs Verification: Directory", dirName)),
                  "- The following subject IDs in the", fileType, "file are not in the 
                  participant ID file:",
                  tags$span(class = "colour-category",
                            paste(df$SUBJECT[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  
  # 2) For whichever subject IDs exist in the DATA/FILELIST/MISSING file, check that
  # their cohort code is correct.
  filteredDF <- df[df$SUBJECT %in% participantIDDF$SUBJECT, ]
  
  # This guarantees that the filtered id data frame only has participants
  # with valid cohort code as specified in the level 1 directory name.
  filteredParticipantIDDF <- participantIDDF[participantIDDF$COHORT == cohortCode, ]
  
  if (!all(filteredDF$SUBJECT %in% filteredParticipantIDDF$SUBJECT)){
    indices <- which(!(filteredDF$SUBJECT %in% filteredParticipantIDDF$SUBJECT))
    line <- paste(tags$span(class = "bold-category", 
                            paste("Subject IDs Verification: Directory", dirName)),
                  "- The following subject IDs in the", fileType, "file do exist 
                  in the participant ID file but some do not match the cohort code 
                  specified in the level 1 directory name:", 
                  tags$span(class = "colour-category",
                            paste(filteredDF$SUBJECT[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  if (fileType == "MISSING"){
    # 3) Check for duplicate IDs in the MISSING file.
    if (any(duplicated(df$SUBJECT))){
      indices <- which(duplicated(df$SUBJECT))
      line <- paste(tags$span(class = "bold-category", 
                              paste("Subject IDs Verification: Directory", dirName)),
                    "- The following subject IDs in the MISSING file are duplicated:", 
                    tags$span(class = "colour-category",
                              paste(unique(df$SUBJECT[indices]), collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
      flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                     "<br/>"))
    }
  }
  # fileType == "DATA" || fileType == "FILELIST"
  else{
    # Aggregate the subject IDs of all 3 MISSING files, if they exist.
    unionOfSubjectIDs <- df$SUBJECT
    missingSubjectIDs <- character()
    
    lvl1MissingFileName <- character()
    lvl2MissingFileName <- character()
    lvl3MissingFileName <- character()
    phrase <- ""
    
    if (!is.null(lvl1DirPath)){
      lvl1MissingFileName <- getFileName(lvl1DirPath, "MISSING.csv")
      
      # Only run if MISSING file in level 1 exists.
      if (length(lvl1MissingFileName) == 1){
        lvl1MissingFilePath <- paste0(lvl1DirPath, "/", lvl1MissingFileName)
        lvl1MissingDF <- read.csv(lvl1MissingFilePath, stringsAsFactors = FALSE)
        lvl1MissingDF <- convertDataFrame(lvl1MissingDF)
        unionOfSubjectIDs <- union(unionOfSubjectIDs, lvl1MissingDF$SUBJECT)
        missingSubjectIDs <- c(missingSubjectIDs, lvl1MissingDF$SUBJECT)
        phrase <- "level 1:"
      }
    }
    if (!is.null(lvl2DirPath)){
      lvl2MissingFileName <- getFileName(lvl2DirPath, "MISSING.csv")
      
      # Only run if MISSING file in level 2 exists.
      if (length(lvl2MissingFileName) == 1){
        lvl2MissingFilePath <- paste0(lvl2DirPath, "/", lvl2MissingFileName)
        lvl2MissingDF <- read.csv(lvl2MissingFilePath, stringsAsFactors = FALSE)
        lvl2MissingDF <- convertDataFrame(lvl2MissingDF)
        unionOfSubjectIDs <- union(unionOfSubjectIDs, lvl2MissingDF$SUBJECT)
        missingSubjectIDs <- c(missingSubjectIDs, lvl2MissingDF$SUBJECT)
        
        if (phrase == "level 1:"){
          phrase <- "levels 1 and 2:"
        }
        else{
          phrase <- "level 2:"
        }
      }
    }
    if (!is.null(lvl3DirPath)){
      lvl3MissingFileName <- getFileName(lvl3DirPath, "MISSING.csv")
      
      # Only run if MISSING file in level 3 exists.
      if (length(lvl3MissingFileName) == 1){
        lvl3MissingFilePath <- paste0(lvl3DirPath, "/", lvl3MissingFileName)
        lvl3MissingDF <- read.csv(lvl3MissingFilePath, stringsAsFactors = FALSE)
        lvl3MissingDF <- convertDataFrame(lvl3MissingDF)
        unionOfSubjectIDs <- union(unionOfSubjectIDs, lvl3MissingDF$SUBJECT)
        missingSubjectIDs <- c(missingSubjectIDs, lvl3MissingDF$SUBJECT)
        
        if (phrase == "levels 1 and 2:"){
          phrase <- "levels 1, 2, and 3:"
        }
        else if (phrase == "level 1:"){
          phrase <- "levels 1 and 3:"
        }
        else if (phrase == "level 2:"){
          phrase <- "levels 2 and 3:"
        }
        else{
         phrase <- "level 3:" 
        }
      }
    }
    
    
    if (length(lvl1MissingFileName) == 1 || length(lvl2MissingFileName) == 1 ||
        length(lvl3MissingFileName) == 1){
      # 3) Check that each ID in the participant ID file with cohort code specified by the
      # file name exists in the DATA/FILELIST file or the MISSING file in lvl 1, 2, and/or
      # 3 if they exist. Using filteredParticipantIDDF from 2).
      if (!all(filteredParticipantIDDF$SUBJECT %in% unionOfSubjectIDs)){
        indices <- which(!(filteredParticipantIDDF$SUBJECT %in% unionOfSubjectIDs))
        
        line <- paste(tags$span(class = "bold-category", 
                                paste("Subject IDs Verification: Directory", dirName)),
                      "- The following subject IDs in the participant ID file are part of 
                      the same cohort and visit code as defined in the level 1 directory name,
                      but are missing in both the", fileType, "file, and the MISSING file
                      in", phrase,
                      tags$span(class = "colour-category",
                                paste(filteredParticipantIDDF$SUBJECT[indices], collapse = ", ")))
        flaggedMsgs <- c(flaggedMsgs, line)
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                       "<br/>"))
      }
      
      
      # 4) Check for duplicate IDs in the DATA/FILELIST file.
      if (any(duplicated(df$SUBJECT))){
        indices <- which(duplicated(df$SUBJECT))
        line <- paste(tags$span(class = "bold-category", 
                                paste("Subject IDs Verification: Directory", dirName)),
                      "- The following subject IDs in the", fileType, "file are duplicated:", 
                      tags$span(class = "colour-category",
                                paste(unique(df$SUBJECT[indices]), collapse = ", ")))
        flaggedMsgs <- c(flaggedMsgs, line)
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                       "<br/>"))
      }
      # 4) Check for duplicate IDs in any of the MISSING files in lvl 1, 2, and/or 3
      # if they exist.
      intersectOfSubjectIDs <- intersect(df$SUBJECT, missingSubjectIDs)
      if (length(intersectOfSubjectIDs) > 0){
        line <- paste(tags$span(class = "bold-category", 
                                paste("Subject IDs Verification: Directory", dirName)),
                      "- The following subject IDs in the", fileType, "file are duplicated
                      in any of the MISSING files in", phrase,
                      tags$span(class = "colour-category",
                                paste(intersectOfSubjectIDs, collapse = ", ")))
        flaggedMsgs <- c(flaggedMsgs, line)
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                       "<br/>"))
      }
    }
    # MISSING file does not exist in any level.
    else{
      # 3) Check that each ID in the participant ID file with cohort code specified by the 
      # file name exists in the DATA/FILELIST file.
      # Using filteredParticipantIDDF from 2).
      if (!all(filteredParticipantIDDF$SUBJECT %in% df$SUBJECT)){
        indices <- which(!(filteredParticipantIDDF$SUBJECT %in% df$SUBJECT))
        
        if (fileType == "FILELIST"){
          line <- paste(tags$span(class = "bold-category", 
                                  paste("Subject IDs Verification: Directory", dirName)),
                        "- The following subject IDs in the participant ID file are part of 
                        the same cohort and visit code as defined in the level 1 directory 
                        name, but are missing in the", fileType, "file:",
                        tags$span(class = "colour-category",
                                  paste(filteredParticipantIDDF$SUBJECT[indices], 
                                        collapse = ", ")),
                        ". If participant files are not available for any of these subject
                        IDs, please create a MISSING file in level 3 and add them into the 
                        MISSING file.")
        }
        else{
          line <- paste(tags$span(class = "bold-category", 
                                  paste("Subject IDs Verification: Directory", dirName)),
                        "- The following subject IDs in the participant ID file are part of
                        the same cohort and visit code as defined in the level 1 directory 
                        name, but are missing in the", fileType, "file:",
                        tags$span(class = "colour-category",
                                  paste(filteredParticipantIDDF$SUBJECT[indices], 
                                        collapse = ", ")),
                        ". If data is not available for any of these subject IDs, please 
                        create a MISSING file in level 1 or 2 and add them into the 
                        MISSING file.")
        }
        flaggedMsgs <- c(flaggedMsgs, line)
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                       "<br/>"))
      }
      
      
      # 4) Check for duplicate IDs in the DATA/FILELIST file.
      if (any(duplicated(df$SUBJECT))){
        indices <- which(duplicated(df$SUBJECT))
        line <- paste(tags$span(class = "bold-category", 
                                paste("Subject IDs Verification: Directory", dirName)),
                      "- The following subject IDs in the", fileType, "file are duplicated:", 
                      tags$span(class = "colour-category",
                                paste(unique(df$SUBJECT[indices]), collapse = ", ")))
        flaggedMsgs <- c(flaggedMsgs, line)
        flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                       "<br/>"))
      }
    }
  }
  

  return (flaggedMsgs)
}

# [END]