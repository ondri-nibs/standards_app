# getInvalidFILELIST.R
#
# Purpose: Create the server logic to print a list of subject IDs for each standard
# and column name for FILELIST file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-04
#
# =======================================================================================

getInvalidFILELIST <- function(input, output, session, lvl1DirPath, participantIDDF, transferIDDF){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory.
  for (dirName in lvl2Dirs) {
    lvl3DirPath <- paste0(lvl1DirPath, "/", dirName, "/DATAFILES")
    lvl3DirName <- paste0(dirName, "/DATAFILES")
    
    filelistFilePath <- paste0(lvl3DirPath, "/", getFileName(lvl3DirPath, "FILELIST.csv"))
    filelistDF <- read.csv(filelistFilePath, stringsAsFactors = FALSE)
    filelistDF <- convertDataFrame(filelistDF)
    
    # "Visit Codes Check" checkbox.
    if (input$checkFILELISTVisitCodes){
      combinedDF <- cbind.fill(combinedDF, 
                               getInvalidVisitCodes(filelistFilePath, lvl3DirName,
                                                    basename(lvl1DirPath)), 
                               fill = "")
    }
    
    # "Site Codes Check" checkbox.
    if (input$checkFILELISTSiteCodes){
      combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(filelistDF, lvl3DirName),
                               fill = "")
    }
    
    # "Date Format Check" checkbox.
    if (input$checkFILELISTDateFormat){
      combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(filelistDF, lvl3DirName), 
                               fill = "")
    }
    
    # "Missing Codes Check" checkbox.
    if (input$checkFILELISTMissingCodes){
      combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(filelistDF, lvl3DirName), 
                               fill = "")
    }
    
    # "Blank Cells Check" checkbox.
    if (input$checkFILELISTBlankCells){
      combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(filelistFilePath, 
                                                                 lvl3DirName,
                                                                 "FILELIST.csv"), 
                               fill = "")
    }
    
    # "Commas Check" checkbox.
    if (input$checkFILELISTCommas){
      combinedDF <- cbind.fill(combinedDF, getCommaLocations(filelistDF, lvl3DirName,
                                                             "FILELIST.csv"), 
                               fill = "")
    }
    
    # "Number of Characters Check" checkbox.
    if (input$checkFILELISTNumOfCharacters){
      combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(filelistDF, 
                                                                      lvl3DirName,
                                                                      "FILELIST.csv"), 
                               fill = "")
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkFILELISTWhiteSpaces){
      combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(filelistDF, lvl3DirName,
                                                                  "FILELIST.csv"), 
                               fill = "")
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkFILELISTEncapsulation){
      combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(filelistDF, 
                                                                     lvl3DirName, 
                                                                     "FILELIST.csv"), 
                               fill = "")
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