# getInvalidLvl3MISSING.R
#
# Purpose: Create the server logic to print a list of subject IDs for each standard
# and column name for MISSING file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

getInvalidLvl3MISSING <- function(input, output, session, lvl1DirPath, participantIDDF,
                                  transferIDDF){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 3 directories that
  # contain a MISSING file.
  for (dirName in lvl2Dirs) {
    lvl3DirPath <- paste0(lvl1DirPath, "/", dirName, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    
    # IMPORTANT: RUN CHECK ONLY IF MISSING FILE EXISTS FOR THIS PARTICULAR LEVEL 3 DIRECTORY.
    missingFileName <- getFileName(lvl3DirPath, "MISSING.csv")
    if (length(missingFileName) == 1){
      missingFilePath <- paste0(lvl3DirPath, "/", missingFileName)
      missingDF <- read.csv(missingFilePath, stringsAsFactors = FALSE)
      missingDF <- convertDataFrame(missingDF)
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl3MISSINGTransferIDs){
        combinedDF <- cbind.fill(combinedDF, 
                                 getTransferIDLocations(missingDF, transferIDDF, 
                                                        lvl3DirName),
                                 fill = "")
      }
      
      # "MISSING_CODE Column Check" checkbox.
      if (input$checkLvl3MISSINGMissingColumn){
        combinedDF <- cbind.fill(combinedDF,
                                 getInvalidMissingCodeColumn(missingDF, lvl3DirName),
                                 fill = "")
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl3MISSINGVisitCodes){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidVisitCodes(missingFilePath, lvl3DirName,
                                                      basename(lvl1DirPath)), 
                                 fill = "")
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl3MISSINGSiteCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(missingDF, lvl3DirName),
                                 fill = "")
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl3MISSINGDateFormat){
        combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(missingDF, lvl3DirName), 
                                 fill = "")
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl3MISSINGMissingCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(missingDF, lvl3DirName), 
                                 fill = "")
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl3MISSINGBlankCells){
        combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(missingFilePath, 
                                                                   lvl3DirName,
                                                                   "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl3MISSINGCommas){
        combinedDF <- cbind.fill(combinedDF, getCommaLocations(missingDF, lvl3DirName,
                                                               "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Number of Characters Check" checkbox.
      if (input$checkLvl3MISSINGNumOfCharacters){
        combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(missingDF, lvl3DirName,
                                                                        "MISSING.csv"), 
                                 fill = "")
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl3MISSINGWhiteSpaces){
        combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(missingDF, lvl3DirName,
                                                                    "MISSING.csv"), 
                                 fill = "")
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl3MISSINGEncapsulation){
        combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(missingDF, lvl3DirName, 
                                                                       "MISSING.csv"), 
                                 fill = "")
      }
    }
  }
  
  # Make 1st row the column names of the huge/combined data frame.
  colnames(combinedDF) <- as.character(unlist(combinedDF[ 1 , ]))
  combinedDF <- combinedDF[ -1 , ]
  
  # Update dropdown box.
  updateSelectInput(session, "lvl3AlgoSelection", label = "", 
                    choices = colnames(combinedDF)[-1])
  
  
  # Update list of subject IDs depending on selection from dropdown menu.
  observe({
    listOfSubjectIDs <- combinedDF[[ input$lvl3AlgoSelection ]]
    
    # Remove any NAs from the vector.
    listOfSubjectIDs <- listOfSubjectIDs[ !(listOfSubjectIDs == "") ]
    
    # Convert to HTML for printing to screen.
    listOfSubjectIDs <- HTML(paste(listOfSubjectIDs, collapse = "<br/>"))
    
    # Print the list of subject IDs.
    output$lvl3OutputID <- renderPrint(listOfSubjectIDs)
  })
}

# [END]