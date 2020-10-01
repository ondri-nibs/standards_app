# getInvalidLvl1MISSING.R
#
# Purpose: Create the server logic to print a list of subject IDs for each standard
# and column name for MISSING file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

getInvalidLvl1MISSING <- function(input, output, session, lvl1DirPath, participantIDDF,
                                  transferIDDF){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  dirName <- basename(lvl1DirPath)
  
  # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 1 DIRECTORY.
  missingFileName <- getFileName(lvl1DirPath, "MISSING.csv")
  if (length(missingFileName) == 1){
    missingFilePath <- paste0(lvl1DirPath, "/", missingFileName)
    missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE)
    missingDF <- convertDataFrame(missingDF)
    
    # For lvl1 MISSING ===================================================================
    
    if (checkIfTab(lvl1DirPath) == "non-tabular"){
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl1MISSINGTransferIDs){
        combinedDF <- cbind.fill(combinedDF, 
                                 getTransferIDLocations(missingDF, transferIDDF, dirName), 
                                 fill = "")
      }
      
      # "MISSING_CODE Column Check" checkbox.
      if (input$checkLvl1MISSINGMissingColumn){
        combinedDF <- cbind.fill(combinedDF,
                                 getInvalidMissingCodeColumn(missingDF, dirName),
                                 fill = "")
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl1MISSINGVisitCodes){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidVisitCodes(missingFilePath, dirName,
                                                      basename(lvl1DirPath)), 
                                 fill = "")
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl1MISSINGSiteCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(missingDF, dirName),
                                 fill = "")
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl1MISSINGDateFormat){
        combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(missingDF, dirName), 
                                 fill = "")
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl1MISSINGMissingCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(missingDF, dirName), 
                                 fill = "")
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl1MISSINGBlankCells){
        combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(missingFilePath, dirName,
                                                                   "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl1MISSINGCommas){
        combinedDF <- cbind.fill(combinedDF, getCommaLocations(missingDF, dirName,
                                                               "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Number of Characters Check" checkbox.
      if (input$checkLvl1MISSINGNumOfCharacters){
        combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(missingDF, dirName,
                                                                        "MISSING.csv"), 
                                 fill = "")
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl1MISSINGWhiteSpaces){
        combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(missingDF, dirName,
                                                                    "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl1MISSINGEncapsulation){
        combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(missingDF, dirName, 
                                                                       "MISSING.csv"), 
                                 fill = "")
      }
    
    
    # Make 1st row the column names of the huge/combined data frame.
    colnames(combinedDF) <- as.character(unlist(combinedDF[ 1 , ]))
    combinedDF <- combinedDF[ -1 , ]
    
    # Update dropdown box.
    updateSelectInput(session, "lvl1AlgoSelection", label = "", 
                      choices = colnames(combinedDF)[-1])
    
    
    # Update list of subject IDs depending on selection from dropdown menu.
    observe({
      listOfSubjectIDs <- combinedDF[[ input$lvl1AlgoSelection ]]
      
      # Remove any NAs from the vector.
      listOfSubjectIDs <- listOfSubjectIDs[ !(listOfSubjectIDs == "") ]
      
      # Convert to HTML for printing to screen.
      listOfSubjectIDs <- HTML(paste(listOfSubjectIDs, collapse = "<br/>"))
      
      # Print the list of subject IDs.
      output$lvl1OutputID <- renderPrint(listOfSubjectIDs)
    })
  }else if (checkIfTab(lvl1DirPath) == "tabular"){
    # "Transfer IDs Check" checkbox.
    if (input$checkTabularMISSINGTransferIDs){
      combinedDF <- cbind.fill(combinedDF, 
                               getTransferIDLocations(missingDF, transferIDDF, dirName), 
                               fill = "")
    }
    
    # "MISSING_CODE Column Check" checkbox.
    if (input$checkTabularMISSINGMissingColumn){
      combinedDF <- cbind.fill(combinedDF,
                               getInvalidMissingCodeColumn(missingDF, dirName),
                               fill = "")
    }
    
    # "Visit Codes Check" checkbox.
    if (input$checkTabularMISSINGVisitCodes){
      combinedDF <- cbind.fill(combinedDF, 
                               getInvalidVisitCodes(missingFilePath, dirName,
                                                    basename(lvl1DirPath)), 
                               fill = "")
    }
    
    # "Site Codes Check" checkbox.
    if (input$checkTabularMISSINGSiteCodes){
      combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(missingDF, dirName),
                               fill = "")
    }
    
    # "Date Format Check" checkbox.
    if (input$checkTabularMISSINGDateFormat){
      combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(missingDF, dirName), 
                               fill = "")
    }
    
    # "Missing Codes Check" checkbox.
    if (input$checkTabularMISSINGMissingCodes){
      combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(missingDF, dirName), 
                               fill = "")
    }
    
    # "Blank Cells Check" checkbox.
    if (input$checkTabularMISSINGBlankCells){
      combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(missingFilePath, dirName,
                                                                 "MISSING.csv"), 
                               fill = "")
    }
    
    # "Commas Check" checkbox.
    if (input$checkTabularMISSINGCommas){
      combinedDF <- cbind.fill(combinedDF, getCommaLocations(missingDF, dirName,
                                                             "MISSING.csv"), 
                               fill = "")
    }
    
    # "Number of Characters Check" checkbox.
    if (input$checkTabularMISSINGNumOfCharacters){
      combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(missingDF, dirName,
                                                                      "MISSING.csv"), 
                               fill = "")
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkTabularMISSINGWhiteSpaces){
      combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(missingDF, dirName,
                                                                  "MISSING.csv"), 
                               fill = "")
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkTabularMISSINGEncapsulation){
      combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(missingDF, dirName, 
                                                                     "MISSING.csv"), 
                               fill = "")
    }
  }
  }
  }

# [END]