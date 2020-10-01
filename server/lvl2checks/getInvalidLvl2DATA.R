# getInvalidLvl2DATA.R
#
# Purpose: Create the server logic to print a list of subject IDs for each standard
# and column name for DATA file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# =======================================================================================

getInvalidLvl2DATA <- function(input, output, session, lvl1DirPath, participantIDDF,
                               transferIDDF){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a DATA file.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF DATA FILE EXISTS FOR THIS PARTICULAR LEVEL 2 DIRECTORY.
    dataFileName <- getFileName(lvl2DirPath, "DATA.csv")
    if (length(dataFileName) == 1){
      dataFilePath <- paste0(lvl2DirPath, "/", dataFileName)
      dataDF <- read.csv(dataFilePath, stringsAsFactors = FALSE)
      dataDF <- convertDataFrame(dataDF)
      
      # "Transfer IDs Check" checkbox.
      if (input$checkLvl2DATATransferIDs){
        combinedDF <- cbind.fill(combinedDF, 
                                 getTransferIDLocations(dataDF, transferIDDF, dirName), 
                                 fill = "")
      }
      
      # "Precision Levels Check" checkbox.
      if (input$checkLvl2DATAPrecisionLevels){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidPrecisionLevels(dataFilePath, dirName), 
                                 fill = "")
      }
      
      # "Special Values Check" checkbox.
      if (input$checkLvl2DATASpecialValues){
        combinedDF <- cbind.fill(combinedDF, 
                                 getSpecialValueLocations(dataFilePath, dirName), 
                                 fill = "")
      }
      
      # "Visit Codes Check" checkbox.
      if (input$checkLvl2DATAVisitCodes){
        combinedDF <- cbind.fill(combinedDF, 
                                 getInvalidVisitCodes(dataFilePath, dirName,
                                                      basename(lvl1DirPath)), 
                                 fill = "")
      }
      
      # "Site Codes Check" checkbox.
      if (input$checkLvl2DATASiteCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidSiteCodes(dataDF, dirName),
                                 fill = "")
      }
      
      # "Date Format Check" checkbox.
      if (input$checkLvl2DATADateFormat){
        combinedDF <- cbind.fill(combinedDF, getInvalidDateFormat(dataDF, dirName), 
                                 fill = "")
      }
      
      # "Missing Codes Check" checkbox.
      if (input$checkLvl2DATAMissingCodes){
        combinedDF <- cbind.fill(combinedDF, getInvalidMissingCodes(dataDF, dirName), 
                                 fill = "")
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl2DATABlankCells){
        combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(dataFilePath, dirName,
                                                                   "DATA.csv"), 
                                 fill = "")
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl2DATACommas){
        combinedDF <- cbind.fill(combinedDF, getCommaLocations(dataDF, dirName,
                                                               "DATA.csv"), 
                                 fill = "")
      }
      
      # "Number of Characters Check" checkbox.
      if (input$checkLvl2DATANumOfCharacters){
        combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(dataDF, dirName,
                                                                        "DATA.csv"), 
                                 fill = "")
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl2DATAWhiteSpaces){
        combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(dataDF, dirName,
                                                                    "DATA.csv"), 
                                 fill = "")
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl2DATAEncapsulation){
        combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(dataDF, dirName, 
                                                                       "DATA.csv"), 
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