# getInvalidLvl1DICT.R
#
# Purpose: Create the server logic to print a list of column labels for each standard
# and column name for DICT file tabular checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-15
#
# =======================================================================================

getInvalidLvl1DICT <- function(input, output, session, lvl1DirPath){
  combinedDF <- data.frame(stringsAsFactors = FALSE)
  dirName <- basename(lvl1DirPath)
  
  # IMPORTANT: RUN CHECK ONLY IF DICT FILE and DATA FILE EXIST FOR THIS PARTICULAR 
  # LEVEL 1 DIRECTORY.
  dictFileName <- getFileName(lvl1DirPath, "DICT.csv")

  if (length(dictFileName) == 1){
    dictFilePath <- paste0(lvl1DirPath, "/", dictFileName)
    dictDF <- read.csv(dictFilePath, stringsAsFactors = FALSE)
    dictDF <- convertDataFrame(dictDF)
    
    # For lvl1 dictionary ===============================================================
      
    if (checkIfTab(lvl1DirPath) == "non-tabular"){
      
        # "Data Types Check" checkbox.
        if (input$checkLvl1DICTDataTypes){
          combinedDF <- cbind.fill(combinedDF, getInvalidDataTypes(dictDF, dirName), 
                                   fill = "")
        }
        
        # "Blank Cells Check" checkbox.
        if (input$checkLvl1DICTBlankCells){
          combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(dictFilePath, dirName,
                                                                     "DICT.csv"), 
                                   fill = "")
        }
        
        # "Commas Check" checkbox.
        if (input$checkLvl1DICTCommas){
          combinedDF <- cbind.fill(combinedDF, getCommaLocations(dictDF, dirName,
                                                                 "DICT.csv"), 
                                   fill = "")
        }
        
        # "Number of Characters Check" checkbox.
        if (input$checkLvl1DICTNumOfCharacters){
          combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(dictDF, dirName,
                                                                          "DICT.csv"), 
                                   fill = "")
        }
        
        # "White Spaces Check" checkbox.
        if (input$checkLvl1DICTWhiteSpaces){
          combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(dictDF, dirName,
                                                                      "DICT.csv"), 
                                   fill = "")
        }
        
        # "Encapsulation Check" checkbox.
        if (input$checkLvl1DICTEncapsulation){
          combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(dictDF, dirName, 
                                                                         "DICT.csv"), 
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
    
    # For Tabular dictionary ===============================================================
      
    } else if (checkIfTab(lvl1DirPath) == "tabular"){
      
        if (input$checkTabularDICTDataTypes){
          combinedDF <- cbind.fill(combinedDF, getInvalidDataTypes(dictDF, dirName), 
                                   fill = "")
        }
        
        # "Blank Cells Check" checkbox.
        if (input$checkTabularDICTBlankCells){
          combinedDF <- cbind.fill(combinedDF, getBlankCellLocations(dictFilePath, dirName,
                                                                     "DICT.csv"), 
                                   fill = "")
        }
        
        # "Commas Check" checkbox.
        if (input$checkTabularDICTCommas){
          combinedDF <- cbind.fill(combinedDF, getCommaLocations(dictDF, dirName,
                                                                 "DICT.csv"), 
                                   fill = "")
        }
        
        # "Number of Characters Check" checkbox.
        if (input$checkTabularDICTNumOfCharacters){
          combinedDF <- cbind.fill(combinedDF, getNumOfCharacterLocations(dictDF, dirName,
                                                                          "DICT.csv"), 
                                   fill = "")
        }
        
        # "White Spaces Check" checkbox.
        if (input$checkTabularDICTWhiteSpaces){
          combinedDF <- cbind.fill(combinedDF, getWhiteSpaceLocations(dictDF, dirName,
                                                                      "DICT.csv"), 
                                   fill = "")
        }
        
        # "Encapsulation Check" checkbox.
        if (input$checkTabularDICTEncapsulation){
          combinedDF <- cbind.fill(combinedDF, getEncapsulationLocations(dictDF, dirName, 
                                                                         "DICT.csv"), 
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