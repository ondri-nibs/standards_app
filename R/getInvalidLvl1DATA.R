# getInvalidLvl1DATA.R
#
# Purpose: Create the server logic to print a list of subject IDs for each standard
# and column name for DATA file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# =======================================================================================

getInvalidLvl1DATA <- function(input, output, session, lvl1DirPath, participantIDDF,
                               transferIDDF){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  dirName <- basename(lvl1DirPath)
    
  # IMPORTANT: RUN CHECK ONLY IF DATA FILE EXISTS FOR THIS PARTICULAR LEVEL 1 DIRECTORY.
  dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
  if (length(dataFileName) == 1){
    dataFilePath <- paste0(lvl1DirPath, "/", dataFileName)
    dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
    dataDF <- convertDataFrame(dataDF)
    
    # For lvl 1 DATA ======================================================================
    
    if (checkIfTab(lvl1DirPath) == "non-tabular"){
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl1DATATransferIDs){
        combinedDF <- cbind.fill(combinedDF, 
                                 getTransferIDLocations(dataDF, transferIDDF, dirName), 
                                 fill = "")
      }
        
      # "Precision Levels Check" checkbox.
      if (input$checkLvl1DATAPrecisionLevels){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidPrecisionLevels(dataFilePath, dirName), 
                                 fill = "")
      }
      
      # "Special Values Check" checkbox.
      if (input$checkLvl1DATASpecialValues){
        combinedDF <- cbind.fill(combinedDF, 
                                 getSpecialValueLocations(dataFilePath, dirName), 
                                 fill = "")
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl1DATAVisitCodes){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidVisitCodes(dataFilePath, dirName,
                                                      basename(lvl1DirPath)), 
                                 fill = "")
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl1DATASiteCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(dataDF, dirName),
                                 fill = "")
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl1DATADateFormat){
        combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(dataDF, dirName), 
                                 fill = "")
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl1DATAMissingCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(dataDF, dirName), 
                                 fill = "")
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl1DATABlankCells){
        combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(dataFilePath, dirName,
                                                                   "DATA.csv"), 
                                 fill = "")
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl1DATACommas){
        combinedDF <- cbind.fill(combinedDF, getCommaLocations(dataDF, dirName,
                                                               "DATA.csv"), 
                                 fill = "")
      }
      
      # "Number of Characters Check" checkbox.
      if (input$checkLvl1DATANumOfCharacters){
        combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(dataDF, dirName,
                                                                        "DATA.csv"), 
                                 fill = "")
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl1DATAWhiteSpaces){
        combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(dataDF, dirName,
                                                                    "DATA.csv"), 
                                 fill = "")
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl1DATAEncapsulation){
        combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(dataDF, dirName, 
                                                                       "DATA.csv"), 
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
    
    
  # For tabulat DATA ====================================================================
  }else if(checkIfTab(lvl1DirPath) == 'tabular'){
    
      # "Transfer IDs Check" checkbox.
      if (input$checkTabularDATATransferIDs){
        combinedDF <- cbind.fill(combinedDF, 
                                 getTransferIDLocations(dataDF, transferIDDF, dirName), 
                                 fill = "")
      }
      
      # "Precision Levels Check" checkbox.
      if (input$checkTabularDATAPrecisionLevels){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidPrecisionLevels(dataFilePath, dirName), 
                                 fill = "")
      }
      
      # "Special Values Check" checkbox.
      if (input$checkTabularDATASpecialValues){
        combinedDF <- cbind.fill(combinedDF, 
                                 getSpecialValueLocations(dataFilePath, dirName), 
                                 fill = "")
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkTabularDATAVisitCodes){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidVisitCodes(dataFilePath, dirName,
                                                      basename(lvl1DirPath)), 
                                 fill = "")
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkTabularDATASiteCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(dataDF, dirName),
                                 fill = "")
      }
      
      # "Date Format Check" checkbox.
      if (input$checkTabularDATADateFormat){
        combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(dataDF, dirName), 
                                 fill = "")
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkTabularDATAMissingCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(dataDF, dirName), 
                                 fill = "")
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkTabularDATABlankCells){
        combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(dataFilePath, dirName,
                                                                   "DATA.csv"), 
                                 fill = "")
      }
      
      # "Commas Check" checkbox.
      if (input$checkTabularDATACommas){
        combinedDF <- cbind.fill(combinedDF, getCommaLocations(dataDF, dirName,
                                                               "DATA.csv"), 
                                 fill = "")
      }
      
      # "Number of Characters Check" checkbox.
      if (input$checkTabularDATANumOfCharacters){
        combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(dataDF, dirName,
                                                                        "DATA.csv"), 
                                 fill = "")
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkTabularDATAWhiteSpaces){
        combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(dataDF, dirName,
                                                                    "DATA.csv"), 
                                 fill = "")
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkTabularDATAEncapsulation){
        combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(dataDF, dirName, 
                                                                       "DATA.csv"), 
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
      output$tabularOutputID <- renderPrint(listOfSubjectIDs)
    })
  }
  }
}


# [END]