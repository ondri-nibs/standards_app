# checkFileNames.R
#
# Purpose: Check whether a list of file names follows the naming convention of: 
# 
# [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL SUBPLATFORM CODE]_
# [OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR CURATION]_[FILE TYPE]
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-24
#
# ==========================================================================================

checkFileNames <- function(listOfFileNames, dirName){
  splitFileNames <- strsplit(listOfFileNames, "_")
  flaggedMsgs <- character()
  
  if (any(lengths(splitFileNames) < 6)){
    indices <- which(lengths(splitFileNames) < 6)
    line <- paste(tags$span(class = "bold-category", 
                            "File Names Check: Directory", dirName), 
                  "- The following file names do not follow the correct format:",
                  tags$span(class = "colour-category", 
                            paste(listOfFileNames[indices], collapse = ", ")), ".",
                  "Valid file naming format is:
                  [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                  SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                  CURATION]_[FILE TYPE]")
    flaggedMsgs <- c(flaggedMsgs, line)
  }else{
      
      # Load the codes input data
      appendixDF <- loadData()
      
      
      # Valid ONDRI codes.
      validOndriCodes <- (appendixDF %>% select(ONDRI_CODES) %>% filter(ONDRI_CODES != ""))
      validOndriCodes <- validOndriCodes$ONDRI_CODES
      # Valid Cohort codes.
      validCohortCodes <- (appendixDF %>% select(COHORT_CODES) %>% filter(COHORT_CODES != ""))
      validCohortCodes <- validCohortCodes$COHORT_CODES
      # Valid visit codes.
      validVisitCodes <- (appendixDF %>% select(VISIT_CODES) %>% filter(VISIT_CODES != ""))
      validVisitCodes <- validVisitCodes$VISIT_CODES
      # Valid platform codes.
      validPlatformCodes <- (appendixDF %>% select(PLATFORM_CODES) %>% 
                               filter(PLATFORM_CODES != ""))
      validPlatformCodes <- validPlatformCodes$PLATFORM_CODES
      # Valid file types.
      validFileTypes <- (appendixDF %>% select(FILE_TYPES) %>% filter(FILE_TYPES != ""))
      validFileTypes <- validFileTypes$FILE_TYPES
    }
    
      
    # ======================================================================================
    
    flaggedMsgs <- character()
    
    ondriCodes <- sapply(splitFileNames, function(x) x[1])
    cohortCodes <- sapply(splitFileNames, function(x) x[2])
    visitCodes <- sapply(splitFileNames, function(x) x[3])
    platformCodes <- sapply(splitFileNames, function(x) x[4])
    dates <- sapply(splitFileNames, function(x) x[length(x) - 1])
    fileTypes <- sapply(splitFileNames, function(x) x[length(x)])
    
    # Test #1: ONDRI code.
    if (!all(ondriCodes %in% validOndriCodes)){
      indices <- which(!(ondriCodes %in% validOndriCodes))
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Check: Directory", dirName),
                    "- The following file names have incorrect ONDRI code:",
                    tags$span(class = "colour-category", 
                              paste(listOfFileNames[indices], collapse = ", ")), ".",
                    "Valid file naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_[FILE TYPE]")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #2: Cohort code.
    if (!all(cohortCodes %in% validCohortCodes)){
      indices <- which(!(cohortCodes %in% validCohortCodes))
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Check: Directory", dirName),
                    "- The following file names have incorrect cohort code:",
                    tags$span(class = "colour-category", 
                              paste(listOfFileNames[indices], collapse = ", ")), ".",
                    "Valid file naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_[FILE TYPE]")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #3: Visit code.
    if (!all(visitCodes %in% validVisitCodes)){
      indices <- which(!(visitCodes %in% validVisitCodes))
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Check: Directory", dirName),
                    "- The following file names have incorrect visit code:",
                    tags$span(class = "colour-category", 
                              paste(listOfFileNames[indices], collapse = ", ")), ".",
                    "Valid file naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_[FILE TYPE]")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #4: Platform code.
    if (!all(platformCodes %in% validPlatformCodes)){
      indices <- which(!(platformCodes %in% validPlatformCodes))
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Check: Directory", dirName),
                    "- The following file names have incorrect platform code:",
                    tags$span(class = "colour-category", 
                              paste(listOfFileNames[indices], collapse = ", ")), ".",
                    "Valid file naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_[FILE TYPE]")
      flaggedMsgs <- c(flaggedMsgs, line)
    }

    # Test #5: Date of release.
    dateObjects <- as.Date(dates, format = "%Y%b%d")
    if (any(is.na(dateObjects))){
      indices <- which(is.na(dateObjects))
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Check: Directory", dirName),
                    "- The following file names have incorrect date format:",
                    tags$span(class = "colour-category", 
                              paste(listOfFileNames[indices], collapse = ", ")), ".",
                    "Valid file naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_[FILE TYPE]")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #6: File type.
    if (!all(fileTypes %in% validFileTypes)){
      indices <- which(!(fileTypes %in% validFileTypes))
      line <- paste(tags$span(class = "bold-category", 
                              "File Names Check: Directory", dirName),
                    "- The following file names have incorrect file type:",
                    tags$span(class = "colour-category", 
                              paste(listOfFileNames[indices], collapse = ", ")), ".",
                    "Valid file naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_[FILE TYPE]")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
  
  
  
  if (length(flaggedMsgs) > 0){
    flaggedMsgs[length(flaggedMsgs)] <- HTML(paste(flaggedMsgs[length(flaggedMsgs)], 
                                                   "<br/>"))
  }
  return (flaggedMsgs)
}

# [END]