# getInvalidLvl2README.R
#
# Purpose: Create the server logic to print a list of file names for each standard
# and column name for README file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

getInvalidLvl2README <- function(input, output, session, lvl1DirPath){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  lvl2Dirs <- list.dirs(path = lvl1DirPath, full.names = FALSE, recursive = FALSE)
  
  # Iterate through each lvl 2 directory and only check in level 2 directories that
  # contain a README file.
  for (dirName in lvl2Dirs) {
    lvl2DirPath <- paste0(lvl1DirPath, "/", dirName)
    
    readmeFileName <- getFileName(lvl2DirPath, "README.csv")
    readmeFilePath <- paste0(lvl2DirPath, "/", readmeFileName)
    readmeDF <- read.csv(readmeFilePath, stringsAsFactors = FALSE)
    readmeDF <- convertDataFrame(readmeDF)
    
    # "Blank Cells Check" checkbox.
    if (input$checkLvl2READMEBlankCells){
      combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(readmeFilePath, dirName,
                                                                 "README.csv"), 
                               fill = "")
    }
    
    # "Commas Check" checkbox.
    if (input$checkLvl2READMECommas){
      combinedDF <- cbind.fill(combinedDF, getCommaLocations(readmeDF, dirName,
                                                             "README.csv"), 
                               fill = "")
    }
    
    # "Number of Characters Check" checkbox.
    if (input$checkLvl2READMENumOfCharacters){
      combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(readmeDF, dirName,
                                                                      "README.csv"), 
                               fill = "")
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkLvl2READMEWhiteSpaces){
      combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(readmeDF, dirName,
                                                                  "README.csv"), 
                               fill = "")
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkLvl2READMEEncapsulation){
      combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(readmeDF, dirName, 
                                                                     "README.csv"), 
                               fill = "")
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