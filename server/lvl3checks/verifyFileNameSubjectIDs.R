# verifyFileNameSubjectIDs.R
#
# Purpose: Verify the subject IDs in the participant file names with the following checks:
# 
#   1) Check that the subject ID in each participant file name is in the participant ID file.
#   2) For whichever subject IDs exist, check that their cohort code is correct.
#   3) Check that each ID in the participant ID file with cohort code specified by the file
#   name exists as a subject ID in a participant file name (or the MISSING file in lvl 1, 2, 
#   and/or 3 if they exist).
#   4) Check for duplicate subject IDs in the participant file names
#   (or the MISSING file in lvl 1, 2, and/or 3 if they exist).
#
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-08-13
#
# ==========================================================================================

verifyFileNameSubjectIDs <- function(participantIDDF, lvl1DirName, dirName,
                                     lvl1DirPath, lvl2DirPath, lvl3DirPath){
  flaggedMsgs <- character()
  
  # Get the cohort code.
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  cohortCode <- splitDirName[2]
  
  # Load the codes input data
  appendixDF <- loadData()
  
  
  # Initialize vector of file types.
  validFileTypes <- appendixDF %>% select(FILE_TYPES) %>% filter(FILE_TYPES != "")
  validFileTypes <- validFileTypes$FILE_TYPES
  # Get file names of specific file types.
  exclusionFiles <- unlist(sapply(validFileTypes, getFileName, dirPath = lvl3DirPath,
                                  USE.NAMES = FALSE))
  
  lvl3FileNames <- list.files(path = lvl3DirPath)
  participantFileNames <- setdiff(lvl3FileNames, exclusionFiles)
  splitIDs <- lapply(strsplit(participantFileNames, "_"), head, 3)
  subjectIDs <- sapply(splitIDs, paste, collapse = "_")
  
  # 1) Check that the subject ID in each participant file name is in the participant ID file.
  if (!all(subjectIDs %in% participantIDDF$SUBJECT)){
    indices <- which(!(subjectIDs %in% participantIDDF$SUBJECT))
    line <- paste(tags$span(class = "bold-category", 
                            paste("File Name Subject IDs Verification: Directory",
                                  dirName)),
                  "The following subject IDs in the participant file names are not
                  in the participant ID file:",
                  tags$span(class = "colour-category",
                            paste(subjectIDs[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }
  
  
  # 2) For whichever subject IDs exist, check that their cohort code is correct.
  filteredSubjectIDs <- subjectIDs[subjectIDs %in% participantIDDF$SUBJECT]
  
  # This guarantees that the filtered id data frame only has participants
  # with valid cohort code as specified in the level 1 directory name.
  filteredParticipantIDDF <- participantIDDF[participantIDDF$COHORT == cohortCode, ]
  
  if (!all(filteredSubjectIDs %in% filteredParticipantIDDF$SUBJECT)){
    indices <- which(!(filteredSubjectIDs %in% filteredParticipantIDDF$SUBJECT))
    line <- paste(tags$span(class = "bold-category", 
                            paste("File Name Subject IDs Verification: Directory",
                                  dirName)),
                  "- The following subject IDs in the participant file names do exist in 
                  the participant ID file but some do not match the cohort code specified
                  in the level 1 directory name:", 
                  tags$span(class = "colour-category",
                            paste(filteredSubjectIDs[indices], collapse = ", ")))
    flaggedMsgs <- c(flaggedMsgs, line)
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                   "<br/>"))
  }

  
  # Aggregate the subject IDs of all 3 MISSING files, if they exist.
  unionOfSubjectIDs <- subjectIDs
  missingSubjectIDs <- character()
  phrase <- ""

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
    
  if (length(lvl1MissingFileName) == 1 || length(lvl2MissingFileName) == 1 ||
      length(lvl3MissingFileName) == 1){
    # 3) Check that each ID in the participant ID file with cohort code specified by the 
    # file name exists as a subject ID in a participant file name (or the MISSING file in
    # lvl 1, 2, and/or 3 if they exist).
    if (!all(filteredParticipantIDDF$SUBJECT %in% unionOfSubjectIDs)){
      indices <- which(!(filteredParticipantIDDF$SUBJECT %in% unionOfSubjectIDs))
      
      line <- paste(tags$span(class = "bold-category", 
                              paste("File Name Subject IDs Verification: Directory", 
                                    dirName)),
                    "- The following subject IDs in the participant ID file are part of the
                    same cohort and visit code as defined in the level 1 directory name,
                    but are missing in both a participant file name and the MISSING file
                    in", phrase,
                    tags$span(class = "colour-category",
                              paste(filteredParticipantIDDF$SUBJECT[indices], 
                                    collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
      flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                     "<br/>"))
    }
      
      
    # 4) Check for duplicate IDs in the participant file names.
    if (any(duplicated(subjectIDs))){
      indices <- which(duplicated(subjectIDs))
      line <- paste(tags$span(class = "bold-category", 
                              paste("File Name Subject IDs Verification: Directory", 
                                    dirName)),
                    "- The following subject IDs in the participant file names
                    are duplicated:", 
                    tags$span(class = "colour-category",
                              paste(unique(subjectIDs[indices]), collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
      flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                     "<br/>"))
    }
    # 4) Check for duplicate IDs in any of the MISSING files in lvl 1, 2, and/or 3
    # if they exist.
    intersectOfSubjectIDs <- intersect(subjectIDs, missingSubjectIDs)
    if (length(intersectOfSubjectIDs) > 0){
      line <- paste(tags$span(class = "bold-category", 
                              paste("File Name Subject IDs Verification: Directory",
                                    dirName)),
                    "- The following subject IDs in the participant file names 
                    are duplicated in any of the MISSING files in", phrase,
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
    # file name exists as a subject ID in a participant file name.
    # Using filteredParticipantIDDF from 2).
    if (!all(filteredParticipantIDDF$SUBJECT %in% subjectIDs)){
      indices <- which(!(filteredParticipantIDDF$SUBJECT %in% subjectIDs))
      line <- paste(tags$span(class = "bold-category", 
                              paste("File Name Subject IDs Verification: Directory",
                                    dirName)),
                    "- The following subject IDs in the participant ID file are part of the 
                    same cohort and visit code as defined in the level 1 directory name,
                    but are missing in a participant file name:",
                    tags$span(class = "colour-category",
                              paste(filteredParticipantIDDF$SUBJECT[indices], 
                                    collapse = ", ")),
                    ". If participant files are not available for any of these subject 
                    IDs, please create a MISSING file in level 3 and add them into the
                    MISSING file.")
      flaggedMsgs <- c(flaggedMsgs, line)
      flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)],
                                                     "<br/>"))
    }
  }
  
  
  return (flaggedMsgs)
}

# [END]