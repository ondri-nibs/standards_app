# getInvalidLvl2MISSING.R
#
# Purpose: Create the server logic to print a list of subject IDs for each standard
# and column name for MISSING file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

getInvalidLvl2MISSING <- function(input, output, session, lvl1DirPath, participantIDDF,
                                  transferIDDF){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a MISSING file.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 2 DIRECTORY.
    missingFileName <- getFileName(lvl2DirPath, "MISSING.csv")
    if (length(missingFileName) == 1){
      missingFilePath <- paste0(lvl2DirPath, "/", missingFileName)
      missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE)
      missingDF <- convertDataFrame(missingDF)
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl2MISSINGTransferIDs){
        combinedDF <- cbind.fill(combinedDF, 
                                 getTransferIDLocations(missingDF, transferIDDF, dirName), 
                                 fill = "")
      }
      
      # "MISSING_CODE Column Check" checkbox.
      if (input$checkLvl2MISSINGMissingColumn){
        combinedDF <- cbind.fill(combinedDF,
                                 getInvalidMissingCodeColumn(missingDF, dirName),
                                 fill = "")
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl2MISSINGVisitCodes){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidVisitCodes(missingFilePath, dirName,
                                                      basename(lvl1DirPath)), 
                                 fill = "")
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl2MISSINGSiteCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(missingDF, dirName),
                                 fill = "")
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl2MISSINGDateFormat){
        combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(missingDF, dirName), 
                                 fill = "")
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl2MISSINGMissingCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(missingDF, dirName), 
                                 fill = "")
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl2MISSINGBlankCells){
        combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(missingFilePath, dirName,
                                                                   "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl2MISSINGCommas){
        combinedDF <- cbind.fill(combinedDF, getCommaLocations(missingDF, dirName,
                                                               "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Number of Characters Check" checkbox.
      if (input$checkLvl2MISSINGNumOfCharacters){
        combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(missingDF, dirName,
                                                                        "MISSING.csv"), 
                                 fill = "")
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl2MISSINGWhiteSpaces){
        combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(missingDF, dirName,
                                                                    "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl2MISSINGEncapsulation){
        combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(missingDF, dirName, 
                                                                       "MISSING.csv"), 
                                 fill = "")
      }
    }
  }
  
  # Make 1st row the column names of the huge/combined data frame.
  colnames(combinedDF) <- as.character(unlist(combinedDF[ 1 , ]))
  combinedDF <- combinedDF[ -1 , ]
  
  # Update dropdown box.
  updateSelectInput(session, "lvl2AlgoSelection", label = "", 
                    choices = colnames(combinedDF)[-1])
  
  
  # Update list of subject IDs depending on selection from dropdown menu.
  observe({
    listOfSubjectIDs <- combinedDF[[ input$lvl2AlgoSelection ]]
    
    # Remove any NAs from the vector.
    listOfSubjectIDs <- listOfSubjectIDs[ !(listOfSubjectIDs == "") ]
    
    # Convert to HTML for printing to screen.
    listOfSubjectIDs <- HTML(paste(listOfSubjectIDs, collapse = "<br/>"))
    
    # Print the list of subject IDs.
    output$lvl2OutputID <- renderPrint(listOfSubjectIDs)
  })
}

# [END]