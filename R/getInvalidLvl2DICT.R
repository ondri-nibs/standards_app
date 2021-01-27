# getInvalidLvl2DICT.R
#
# Purpose: Create the server logic to print a list of column labels for each standard
# and column name for DICT file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-11
#
# =======================================================================================

getInvalidLvl2DICT <- function(input, output, session, lvl1DirPath){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a DICT file.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    # IMPORTANT: RUN CHECK ONLY IF DICT FILE and DATA FILE EXIST FOR THIS PARTICULAR 
    # LEVEL 2 DIRECTORY.
    dictFileName <- getFileName(lvl2DirPath, "DICT.csv")
    if (length(dictFileName) == 1){
      dictFilePath <- paste0(lvl2DirPath, "/", dictFileName)
      dictDF <- read.csv(dictFilePath, stringsAsFactors = FALSE)
      dictDF <- convertDataFrame(dictDF)
      
      # "Data Types Check" checkbox.
      if (input$checkLvl2DICTDataTypes){
        combinedDF <- cbind.fill(combinedDF, getInvalidDataTypes(dictDF, dirName), 
                                 fill = "")
      }
      
      # "Blank Cells Check" checkbox.
      if (input$checkLvl2DICTBlankCells){
        combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(dictFilePath, dirName,
                                                                   "DICT.csv"), 
                                 fill = "")
      }
      
      # "Commas Check" checkbox.
      if (input$checkLvl2DICTCommas){
        combinedDF <- cbind.fill(combinedDF, getCommaLocations(dictDF, dirName,
                                                               "DICT.csv"), 
                                 fill = "")
      }
      
      # "Number of Characters Check" checkbox.
      if (input$checkLvl2DICTNumOfCharacters){
        combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(dictDF, dirName,
                                                                        "DICT.csv"), 
                                 fill = "")
      }
      
      # "White Spaces Check" checkbox.
      if (input$checkLvl2DICTWhiteSpaces){
        combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(dictDF, dirName,
                                                                    "DICT.csv"), 
                                 fill = "")
      }
      
      # "Encapsulation Check" checkbox.
      if (input$checkLvl2DICTEncapsulation){
        combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(dictDF, dirName, 
                                                                       "DICT.csv"), 
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