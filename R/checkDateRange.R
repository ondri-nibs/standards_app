# checkDateRange.R
#
# Purpose: Check whether a tabular csv file contains valid date ranges. A valid date
# range is a date between the date of consent and X # of weeks after consent depending
# on the visit code, in addition to verifying that it is a valid date (not a future date).
# If not, the subject IDs corresponding to the date range violations are given through 
# getInvalidDateRange.R.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# =========================================================================================

checkDateRange <- function(dfFilePath, participantIDDF, lvl1DirName, dirName, study_name){
  
  # Load the codes input data
  appendixDF <- loadData()
  
  # Initialize vector of missing codes.
  appendixDF <- appendixDF %>% select(MISSING_CODES) %>% filter(MISSING_CODES != "")
  validMissingCodes <- appendixDF$MISSING_CODES

  # =======================================================================================
  
  flaggedMsgs <- character()
  
  # Create duplicate df with content of VISIT column of type character.
  df <- read.csv(dfFilePath, stringsAsFactors = FALSE, colClasses = "character")
  df <- convertDataFrame(df)
  columnNames <- colnames(df)
  
  # Get the column names corresponding to DATE.
  splitColumnNames <- sapply(strsplit(columnNames, "_"), tail, 1)
  colNums <- which(splitColumnNames == "DATE")
  # Get visit code from level 1 directory name.
  splitDirName <- unlist(strsplit(lvl1DirName, "_"))
  visitCode <- splitDirName[3]
    
  for (num in colNums){
    colName <- columnNames[num]
    
    # Base version: Get only valid dates.
    dates <- df[ , colName ]
    filteredDF <- df[ , c("SUBJECT", "VISIT", colName) ]
    filteredDF <- filteredDF[ !is.na(as.Date(dates, format = "%Y%b%d")) , ]
    dates <- filteredDF[ , colName ]
    
    # Convert date strings to date objects.
    dateObjects <- as.Date(dates, format = "%Y%b%d")
    
    # Date in file is a future date compared to today's date.
    if (any((Sys.Date() - dateObjects) < 0)){
      indices <- which((Sys.Date() - dateObjects) < 0)
      line <- paste(tags$span(class = "bold-category", "Date Range: Directory", dirName),
                    "- Column", colName, "contains dates in advance of today's date.
                    The following subject IDs correspond to these dates:",
                    tags$span(class = "colour-category", 
                              paste(filteredDF$SUBJECT[indices], collapse = ", ")))
      flaggedMsgs <- c(flaggedMsgs, line)
    }
    
    # Subject IDs.
    subjectIDs <- filteredDF$SUBJECT
    
    indicesToCheck <- which((Sys.Date() - dateObjects) < 0)
    for (index in 1:nrow(filteredDF)){
      if (length(dateObjects) > 0 && !(index %in% indicesToCheck)
          && !is.na(dateObjects[index])){
        indexInID <- (which(participantIDDF$SUBJECT == filteredDF$SUBJECT[index]))[1]
        consentDate <- participantIDDF$CONSENT_DATE[indexInID]
        # Convert consent date strings to date objects.
        consentDateObject <- as.Date(consentDate, format = "%Y%b%d")
        numOfDays <- dateObjects[index] - consentDateObject
        
        # Get subject ID for that particular participant.
        subjectID <- subjectIDs[index]

        # Date is before consent date.
        if (numOfDays < 0){
          elapsedNumOfWeeks <- round((abs(numOfDays) / 7), digits = 1)
          line <- paste(tags$span(class = "bold-category", 
                                  "Date Range: Directory", dirName), 
                        "- Column", colName, ": Date for subject ID", subjectID, 
                        "is out of window assessment. The date is", elapsedNumOfWeeks,
                        "weeks before consent date.")
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        # ELSE clause depends on the visit code.
        else{

          # VISIT CODE #1 and #2: Date is 8 weeks after consent date.
          if ((visitCode == "01" || visitCode == "02") && numOfDays > 56){
            elapsedNumOfWeeks <- round((numOfDays / 7), digits = 1)
            line <- paste(tags$span(class = "bold-category", 
                                    "Date Range: Directory", dirName), 
                          "- Column", colName, ": Date for subject ID", subjectID, 
                          "is out of window assessment. The date is", elapsedNumOfWeeks,
                          "weeks after consent date.")
            flaggedMsgs <- c(flaggedMsgs, line)
        
          #VISIT CODE for Beam and ReMiNDD: Date is 1 week after consent
          }else if (study_name != "OND01" & numOfDays > 7){
            elapsedNumOfWeeks <- round((numOfDays / 7), digits = 1)
            line <- paste(tags$span(class = "bold-category", 
                                    "Date Range: Directory", dirName), 
                          "- Column", colName, ": Date for subject ID", subjectID, 
                          "is out of window assessment. The date is", elapsedNumOfWeeks,
                          "weeks after consent date.")
            flaggedMsgs <- c(flaggedMsgs, line)
          }
          
          # VISIT CODE #3: Date is > 2 weeks before or after consent
          # date + 6 months.
          else if (visitCode == "03"){
            numOfDays <- dateObjects[index] - (consentDateObject %m+% months(6))
            elapsedNumOfWeeks <- round((abs(numOfDays) / 7), digits = 1)
            
            if (numOfDays > 14){
              line <- paste(tags$span(class = "bold-category",
                                      "Date Range: Directory", dirName),
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks after consent date + 6 months.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
            else if (numOfDays < -14){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName),
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks before consent date + 6 months.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
          }
          # VISIT CODE #4: Date is > 6 weeks after consent date + 1 year, or
          # > 4 weeks before consent date + 1 year.
          else if (visitCode == "04"){
            numOfDays <- dateObjects[index] - (consentDateObject %m+% years(1))
            elapsedNumOfWeeks <- round((abs(numOfDays) / 7), digits = 1)
            
            if (numOfDays > 42){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks after consent date + 1 year.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
            else if (numOfDays < -28){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks before consent date + 1 year.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
          }
          # VISIT CODE #5: Date is > 2 weeks before or after consent
          # date + 18 months.
          else if (visitCode == "05"){
            numOfDays <- dateObjects[index] - (consentDateObject %m+% months(18))
            elapsedNumOfWeeks <- round((abs(numOfDays) / 7), digits = 1)
            
            if (numOfDays > 14){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks after consent date + 18 months.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
            else if (numOfDays < -14){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks before consent date + 18 months.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
          }
          # VISIT CODE #6: Date is > 6 weeks after consent date + 2 years, or
          # > 4 weeks before consent date + 2 years.
          else if (visitCode == "06"){
            numOfDays <- dateObjects[index] - (consentDateObject %m+% years(2))
            elapsedNumOfWeeks <- round((abs(numOfDays) / 7), digits = 1)
            
            if (numOfDays > 42){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks after consent date + 2 years.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
            else if (numOfDays < -28){
              line <- paste(tags$span(class = "bold-category",
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks before consent date + 2 years.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
          }
          # VISIT CODE #7: Date is > 2 weeks before or after consent
          # date + 30 months.
          else if (visitCode == "07"){
            numOfDays <- dateObjects[index] - (consentDateObject %m+% months(30))
            elapsedNumOfWeeks <- round((abs(numOfDays) / 7), digits = 1)
            
            if (numOfDays > 14){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks after consent date + 30 months.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
            else if (numOfDays < -14){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks before consent date + 30 months.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
          }
          # VISIT CODE #8: Date is > 6 weeks after consent date + 3 years, or
          # > 4 weeks before consent date + 3 years.
          else if (visitCode == "08"){
            numOfDays <- dateObjects[index] - (consentDateObject %m+% years(3))
            elapsedNumOfWeeks <- round((abs(numOfDays) / 7), digits = 1)
            
            if (numOfDays > 42){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks after consent date + 3 years.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
            else if (numOfDays < -28){
              line <- paste(tags$span(class = "bold-category", 
                                      "Date Range: Directory", dirName), 
                            "- Column", colName, ": Date for subject ID", subjectID, 
                            "is out of window assessment. The date is", elapsedNumOfWeeks,
                            "weeks before consent date + 3 years.")
              flaggedMsgs <- c(flaggedMsgs, line)
            }
          }
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