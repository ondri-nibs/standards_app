# getInvalidLvl1README.R
#
# Purpose: Create the server logic to print a list of file names for each standard
# and column name for README file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-05
#
# =======================================================================================

getInvalidLvl1README <- function(input, output, session, lvl1DirPath){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  dirName <- basename(lvl1DirPath)
  
  readmeFileName <- getFileName(lvl1DirPath, "README.csv")
  readmeFilePath <- paste0(lvl1DirPath, "/", readmeFileName)
  readmeDF <- read.csv(readmeFilePath, stringsAsFactors = FALSE)
  readmeDF <- convertDataFrame(readmeDF)
  
  # For level 1 directory =======================================================================================  
  
  if (checkIfTab(lvl1DirPath) == "non-tabular"){
    # "Blank Cells Check" checkbox.
    if (input$checkLvl1READMEBlankCells){
      combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(readmeFilePath, dirName,
                                                                 "README.csv"), 
                               fill = "")
    }
    
    # "Commas Check" checkbox.
    if (input$checkLvl1READMECommas){
      combinedDF <- cbind.fill(combinedDF, getCommaLocations(readmeDF, dirName,
                                                             "README.csv"), 
                               fill = "")
    }
    
    # "Number of Characters Check" checkbox.
    if (input$checkLvl1READMENumOfCharacters){
      combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(readmeDF, dirName,
                                                                      "README.csv"), 
                               fill = "")
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkLvl1READMEWhiteSpaces){
      combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(readmeDF, dirName,
                                                                  "README.csv"), 
                               fill = "")
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkLvl1READMEEncapsulation){
      combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(readmeDF, dirName, 
                                                                     "README.csv"), 
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
    
    # For Tabular directory =======================================================================================
} else if (checkIfTab(lvl1DirPath) == "tabular"){
    if (input$checkTabularREADMEBlankCells){
      combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(readmeFilePath, dirName,
                                                                 "README.csv"), 
                               fill = "")
    }
    
    # "Commas Check" checkbox.
    if (input$checkTabularREADMECommas){
      combinedDF <- cbind.fill(combinedDF, getCommaLocations(readmeDF, dirName,
                                                             "README.csv"), 
                               fill = "")
    }
    
    # "Number of Characters Check" checkbox.
    if (input$checkTabularREADMENumOfCharacters){
      combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(readmeDF, dirName,
                                                                      "README.csv"), 
                               fill = "")
    }
    
    # "White Spaces Check" checkbox.
    if (input$checkTabularREADMEWhiteSpaces){
      combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(readmeDF, dirName,
                                                                  "README.csv"), 
                               fill = "")
    }
    
    # "Encapsulation Check" checkbox.
    if (input$checkTabularREADMEEncapsulation){
      combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(readmeDF, dirName, 
                                                                     "README.csv"), 
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
    
}
}

# [END]