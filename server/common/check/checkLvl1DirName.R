# checkLvl1DirName.R
#
# Purpose: Check that the level 1 directory follows the naming convention of:
#
# [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL SUBPLATFORM CODE]_
# [OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR CURATION]_DATAPKG
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-07-15
#
# ==========================================================================================

checkLvl1DirName <- function(lvl1DirName){
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  flaggedMsgs <- character()
  
  if (length(splitDirName) < 6){
    line <- paste(tags$span(class = "bold-category", "Lvl 1 Directory Name Check"), 
                  "- The level 1 directory name",
                  tags$span(class = "colour-category", lvl1DirName), 
                  "does not follow the correct format. Valid directory naming format is:
                  [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                  SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                  CURATION]_DATAPKG")
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
      
  
    # ======================================================================================
    
    # Test #1: ONDRI code.
    if (!(splitDirName[1] %in% validOndriCodes)){
      line <- paste(tags$span(class = "bold-category", "Lvl 1 Directory Name Check"),
                    "- The level 1 directory name",
                    tags$span(class = "colour-category", lvl1DirName),
                    "has incorrect ONDRI code. Valid directory naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_DATAPKG")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #2: Cohort code.
    if (!(splitDirName[2] %in% validCohortCodes)){
      line <- paste(tags$span(class = "bold-category", "Lvl 1 Directory Name Check"),
                    "- The level 1 directory name",
                    tags$span(class = "colour-category", lvl1DirName),
                    "has incorrect cohort code. Valid directory naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_DATAPKG")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #3: Visit code.
    if (!(splitDirName[3] %in% validVisitCodes)){
      line <- paste(tags$span(class = "bold-category", "Lvl 1 Directory Name Check"),
                    "- The level 1 directory name",
                    tags$span(class = "colour-category", lvl1DirName),
                    "has incorrect visit code. Valid directory naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_DATAPKG")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #4: Platform code.
    if (!(splitDirName[4] %in% validPlatformCodes)){
      line <- paste(tags$span(class = "bold-category", "Lvl 1 Directory Name Check"),
                    "- The level 1 directory name",
                    tags$span(class = "colour-category", lvl1DirName),
                    "has incorrect platform code. Valid directory naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_DATAPKG")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
  
    # Test #5: Date of release.
    dateObject <- as.Date(splitDirName[length(splitDirName) - 1], format = "%Y%b%d")
    if (is.na(dateObject)){
      line <- paste(tags$span(class = "bold-category", "Lvl 1 Directory Name Check"),
                    "- The level 1 directory name",
                    tags$span(class = "colour-category", lvl1DirName),
                    "has incorrect date format. Valid directory naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_DATAPKG")
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Test #6: DATAPKG ending.
    if (splitDirName[length(splitDirName)] != "DATAPKG"){
      line <- paste(tags$span(class = "bold-category", "Lvl 1 Directory Name Check"),
                    "- The level 1 directory name",
                    tags$span(class = "colour-category", lvl1DirName),
                    "needs to end with DATAPKG. Valid directory naming format is:
                    [ONDRI CODE]_[COHORT CODE]_[VISIT CODE]_[PLATFORM CODE]_[OPTIONAL 
                    SUBPLATFORM CODE]_[OPTIONAL DATA SET CODE]_[DATE OF RELEASE FOR
                    CURATION]_DATAPKG")
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