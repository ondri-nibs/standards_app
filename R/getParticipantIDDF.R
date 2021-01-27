# getParticipantIDDF.R
#
# Purpose: Return an ID data frame corresponding to the participants with the visit 
# code specified by the level 1 directory name.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# =========================================================================================

getParticipantIDDF <- function(participantIDFilePath, lvl1DirName, study_name){
  
  # Testing this feature 
  # Call onto the appendix valid participant ID file for the different studies from the appendix
  if (study_name == 'OND01'){
    # This will be changed to a in app file once it is embedded in the app
    participantIDDF <- read.csv(participantIDFilePath, stringsAsFactors = FALSE)
  }else{
    # Create the id data frame by reading .csv file.
    participantIDDF <- read.csv(participantIDFilePath, stringsAsFactors = FALSE)
  }
  
  # Capitalize column names for ID data frame.
  colnames(participantIDDF) <- toupper(colnames(participantIDDF))
  columnNames <- colnames(participantIDDF)
  
  # Get visit code from level 1 directory name.
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  visitCode <- splitDirName[3]
  
  
  if (visitCode == "01" || visitCode == "02"){
    if ("INCLUDE_IN_SCREENING_BASELINE" %in% columnNames){
      participantIDDF <- participantIDDF[ participantIDDF$INCLUDE_IN_SCREENING_BASELINE == "YES" , ]
    }
  }
  else if (visitCode == "03"){
    if ("INCLUDE_IN_6_MONTH" %in% columnNames){
      participantIDDF <- participantIDDF[ participantIDDF$INCLUDE_IN_6_MONTH == "YES" , ]
    }
  }
  else if (visitCode == "04"){
    if ("INCLUDE_IN_YEAR_1" %in% columnNames){
      participantIDDF <- participantIDDF[ participantIDDF$INCLUDE_IN_YEAR_1 == "YES" , ]
    }
  }
  else if (visitCode == "05"){
    if ("INCLUDE_IN_18_MONTH" %in% columnNames){
      participantIDDF <- participantIDDF[ participantIDDF$INCLUDE_IN_18_MONTH == "YES" , ]
    }
  }
  else if (visitCode == "06"){
    if ("INCLUDE_IN_YEAR_2" %in% columnNames){
      participantIDDF <- participantIDDF[ participantIDDF$INCLUDE_IN_YEAR_2 == "YES" , ]
    }
  }
  else if (visitCode == "07"){
    if ("INCLUDE_IN_30_MONTH" %in% columnNames){
      participantIDDF <- participantIDDF[ participantIDDF$INCLUDE_IN_30_MONTH == "YES" , ]
    }
  }
  else if (visitCode == "08"){
    if ("INCLUDE_IN_YEAR_3" %in% columnNames){
      participantIDDF <- participantIDDF[ participantIDDF$INCLUDE_IN_YEAR_3 == "YES" , ]
    }
  }
  
  return (participantIDDF)
}

# [END]