# tabularServer.R
#
# Purpose: Create the server logic for the tabular tab for tabular structure checks.
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-06-19
# ==========================================================================================

tabularServer <- function(input, output, session){
  TabularRequiredFileTypes <- c("README.csv", "METHODS.pdf", "DICT.csv", "DATA.csv")
  lvl <- 1
  
  # Disable checkboxes that are mandatory checks.
  disable("checkTabularRequired1")
  disable("checkTabularRequired2")
  disable("checkTabularRequired3")
  disable("checkTabularRequired4")
  disable("checkTabularREADMEColumnNames")
  disable("checkTabularDICTColumnNames")
  disable("checkTabularDATAColumnNames")
  disable("checkTabularMISSINGColumnNames")
  disable("checkTabularDirName1")
  disable("checkTabularDirName2")
  
  # Button to bring user back to the select form page
  observeEvent(input$Back, {
    {js$reset()}
  })
  
  # Save the log file
  observe({
    # Computer volumes depend on OS: Windows, Mac, or Linux.
    volumes <- c(Home = fs::path_home(), getVolumes()())
    

    shinyFileSave(input, "save", roots=volumes)
    fileinfo <- parseSavePath(volumes, input$save)
    if (nrow(fileinfo) > 0) {
      path <- as.character(fileinfo$datapath)
      create_log_file(path)
    }
    
  })
  
  observeEvent(
    # Run code only if "Check Directory" button is clicked.
    input$tabularCheckDirButton, {
      
      # Log file Input
      XML_add_child("checks", "<DIR/>")
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Clear output text section to make blank.
      output$tabularOutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$tabularOutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl1AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "tabularProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      
      
      # Log Input
      XML_add_child("DIR", "<outputSelection_DIR/>")
      
      if (length(lvl1DirPath) == 0){
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_attr("outputSelection_DIR", "FAIL")
        XML_add_txt("outputSelection_DIR", "ERROR: A level 1 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")

      }
      else if (!dir.exists(lvl1DirPath)){
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_child("DIR", "<outputMissing_DIR/>")
        XML_add_attr("outputMissing_DIR", "FAIL")
        XML_add_txt("outputMissing_DIR", "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        #Log Input
        XML_add_attr("outputSelection_DIR", "PASS")
      }
      else{
        # Log Input
        XML_add_attr("outputSelection_DIR", "PASS")
        XML_add_child("DIR", "<outputMissing_DIR/>")
        XML_add_attr("outputMissing_DIR", "PASS")
        # Declare variable for progress computation.
        numOfCheckboxes <- 5
        
        # Lvl 1 Directory Checks.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1Directory(input, session, lvl1DirPath, 
                                                         numOfCheckboxes, 
                                                         TabularRequiredFileTypes, study_name))
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "Directory", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "tabularProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$tabularOutputText <- renderPrint(flaggedMsgs)
      
}

)
  
  
  observeEvent(
    # Run code only if "Check README" button is clicked.
    input$tabularCheckREADMEButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Clear output text section to make blank.
      output$tabularOutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$tabularOutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl1AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "tabularProgressBar", 
                        value = 0)

      # Get lvl 1 directory path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      
      
      # Log input
      XML_add_child("checks", "<README/>")
      XML_add_child("README", "<outputSelection_README/>")
      
      if (length(lvl1DirPath) == 0){
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
        # Log input
        XML_add_attr("outputSelection_README", "FAIL")
        XML_add_txt("outputSelection_README", "ERROR: A level 1 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
      
      }
      else if (!dir.exists(lvl1DirPath)){
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_child("README", "<outputMissing_README/>")
        XML_add_attr("outputMissing_README", "FAIL")
        XML_add_txt("outputMissing_README", "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        #Log Input
        XML_add_child("outputSelection_README", "<PASS_TAB3/>")
      }
      else{
        # Log Input
        XML_add_attr("outputSelection_README", "PASS")
        XML_add_child("README", "<outputMissing_README/>")
        XML_add_attr("outputMissing_README", "PASS")
        # Declare variable for progress computation.
        XML_add_child("README", "<checkLvl1Required_README/>")
        numOfCheckboxes <- 10
        
        # MANDATORY CHECKS:
        # 1) Check that the README file, the METHODS file, and at least one level 2 folder
        # exist in the level 1 directory, and that there is only 1 of each possible type
        # that is a file.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1Required(lvl1DirPath, "README",
                                                        c("README.csv")))
        updateProgressBar(session = session, 
                          id = "tabularProgressBar", 
                          value = 100 / numOfCheckboxes)
        
        
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl1Required_README", "PASS")
          XML_add_child("README", "<checkLvl1READMEColumnNames/>")
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1READMEColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "tabularProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }else{
          # Log input
          XML_add_attr("checkLvl1Required_README", "FAIL")
          XML_add_txt("checkLvl1Required_README", "ERROR: The required check did not pass.")
        }
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
        if (length(flaggedMsgs) != 0){
          # Log input
          XML_add_attr("checkLvl1READMEColumnNames", "FAIL")
          XML_add_txt("checkLvl1READMEColumnNames", "ERROR: The column names check did not pass.")
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          #Log Input
          XML_add_attr("checkLvl1READMEColumnNames", "PASS")
          # README Tabular Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1README(input, session, lvl1DirPath, 
                                                        numOfCheckboxes,
                                                        TabularRequiredFileTypes, study_name))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl1README(input, output, session, lvl1DirPath)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "README File", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "tabularProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$tabularOutputText <- renderPrint(flaggedMsgs)
    
  })
  
  
  observeEvent(
    # Run code only if "Check DICT" button is clicked.
    input$tabularCheckDICTButton, {
      
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Clear output text section to make blank.
      output$tabularOutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$tabularOutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl1AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "tabularProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      dictFileName <- getFileName(lvl1DirPath, "DICT.csv")
      
      # Log input
      XML_add_child("checks", "<DICT/>")
      XML_add_child("DICT", "<outputSelection_DICT/>" )
      XML_add_child("DICT", "<outputMissing_DICT/>")
      XML_add_child("DICT", "<outputDoesNotExist_DICT/>")
      if (length(lvl1DirPath) == 0){
        XML_add_attr("outputSelection_DICT", "FAIL")
        XML_add_txt("outputSelection_DICT", "ERROR: A level 1 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
      }
      else if (!dir.exists(lvl1DirPath)){
        #Log input 
        XML_add_attr("outputSelection_DICT", 'PASS')
        XML_add_attr("outputMissing_DICT", "FAIL")
        XML_add_txt("outputMissing_DICT", "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
      }
      else if (length(dictFileName) == 0){
        #Log input 
        XML_add_attr("outputSelection_DICT", 'PASS')
        XML_add_attr("outputMissing_DICT", "PASS")
        XML_add_attr("outputDoesNotExist_DICT", "FAIL")
        XML_add_txt("outputDoesNotExist_DICT" ,"ERROR: DICT file does not exist in the level 1 directory.
                         No checks can be performed.")
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "DICT file", lvl)
      }
      else{
        # Log input
        XML_add_attr("outputSelection_DICT", 'PASS')
        XML_add_attr("outputMissing_DICT", "PASS")
        XML_add_attr("outputDoesNotExist_DICT", "PASS")
        # Declare variable for progress computation.
        numOfCheckboxes <- 11
        
        # Log input
        XML_add_child("DICT", "<checkLvl1Required_DICT/>")
        XML_add_child("DICT", "<checkLvl1DICTColumnNames/>")
        # MANDATORY CHECKS:
        # 1) Check that the README file, the METHODS file, and at least one level 2 folder
        # exist in the level 1 directory, and that there is only 1 of each possible type
        # that is a file.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1Required(lvl1DirPath, "DICT"))
        updateProgressBar(session = session, 
                          id = "tabularProgressBar", 
                          value = 100 / numOfCheckboxes)
        
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl1Required_DICT", "PASS")
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1DICTColumnNames(lvl1DirPath))
          flaggedMsgs <- c(flaggedMsgs, checkLvl1DATAColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "tabularProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
        if (length(flaggedMsgs) != 0){
          # Log input
          XML_add_attr("checkLvl1Required_DICT", "FAIL")
          XML_add_txt("checkLvl1Required_DICT","ERROR: The required check did not pass.")
          XML_add_attr("checkLvl1DICTColumnNames", "FAIL")
          XML_add_txt("checkLvl1DICTColumnNames", "ERROR: The column names check did not pass.")
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          # Log Input
          XML_add_attr("checkLvl1Required", "PASS")
          XML_add_attr("checkLvl1DICTColumnNames", "PASS")
          
          # DICT Tabular Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1DICT(input, session, lvl1DirPath, 
                                                      numOfCheckboxes, study_name))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl1DICT(input, output, session, lvl1DirPath)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "DICT File", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "tabularProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$tabularOutputText <- renderPrint(flaggedMsgs)
    }
  )
  
  
  observeEvent(
    # Run code only if "Check DATA" button is clicked.
    input$tabularCheckDATAButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Clear output text section to make blank.
      output$tabularOutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$tabularOutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl1AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "tabularProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path, participant ID file path, and transfer ID file path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
      transferIDFilePath <- parseFilePaths(volumes, input$transferIDFile)
      dataFileName <- getFileName(lvl1DirPath, "DATA.csv")
      
      # Log input
      XML_add_child("checks", "<DATA/>")
      XML_add_child("DATA", "<outputSelection_DATA/>" )
      XML_add_child("DATA", "<outputMissing_DATA/>")
      XML_add_child("DATA", "<outputDoesNotExist_DATA/>")
      
      # Making sure the participantFile is set as FALSE if "Package only checks"
      if (study_name == "Package only checks"){
        participantFile = FALSE
      }
      
      if (participantFile == TRUE){
        if (length(lvl1DirPath) == 0 || length(participantIDFilePath$datapath) == 0){
         
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_DATA", "FAIL")
          XML_add_txt("outputSelection_DATA", "ERROR: Directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
          
        }
        else if (!dir.exists(lvl1DirPath) || !file.exists(participantIDFilePath$datapath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_DATA", "PASS")
          XML_add_attr("outputMissing_DATA", "FAIL")
          XML_add_txt("outputMissing_DATA", "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")

          }
          
      else if (length(dataFileName) == 0){
        # Log input
        XML_add_attr("outputSelection_DATA", "PASS")
        XML_add_attr("outputMissing_DATA", "PASS")
        XML_add_attr("outputDoesNotExist_DATA", "FAIL")
        XML_add_txt("outputDoesNotExist_DATA" ,"ERROR: DATA file does not exist in the level 1 directory.
                         No checks can be performed.")
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "DATA file", lvl)
      }
        else{
          # Log input
          XML_add_attr("outputSelection_DATA", "PASS")
          XML_add_attr("outputMissing_DATA", "PASS")
          XML_add_attr("outputDoesNotExist_DATA", "PASS")
          XML_add_child("DATA", "<checkLvl1Required_DATA/>")
          
          
          # Declare variable for progress computation. 
          numOfCheckboxes <- 23
          
          # MANDATORY CHECKS:
          # 1) Check that the README file, the METHODS file, and at least one level 2 folder
          # exist in the level 1 directory, and that there is only 1 of each possible type
          # that is a file.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1Required(lvl1DirPath, "DATA"))
          updateProgressBar(session = session, 
                            id = "tabularProgressBar", 
                            value = 100 / numOfCheckboxes)
          
          # Run only if 1) passes.
          if (length(flaggedMsgs) == 0){
            # Log input
            XML_add_attr("checkLvl1Required_DATA", "PASS")
            XML_add_child("DATA", "<checkLvl1DATAColumnNames/>")
            XML_add_child("DATA", "<checkLvl1MISSINGColumnNames_DATA/>")
            
            # 2) Check that all the necessary column names exist, that they are correct,
            # and that there aren't any duplicates.
            flaggedMsgs <- c(flaggedMsgs, checkLvl1DATAColumnNames(lvl1DirPath))
            flaggedMsgs <- c(flaggedMsgs, checkLvl1MISSINGColumnNames(lvl1DirPath))
            updateProgressBar(session = session,
                              id = "tabularProgressBar", 
                              value = 100 / numOfCheckboxes * 2)
          }
          # Log input
          XML_add_child("DATA", "<checkLvl1DirName_DATA/>")
          # 3) Check that the level 1 directory name is correct to get visit code
          # for subject ID, visit code, and date range checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(basename(lvl1DirPath)))
          updateProgressBar(session = session, 
                            id = "tabularProgressBar", 
                            value = 100 / numOfCheckboxes * 3)
          
          
          # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 3 CHECKS PASS.
          if (length(flaggedMsgs) != 0){
            # Log input
            XML_add_attr("checkLvl1Required_DATA", "FAIL")
            XML_add_txt("checkLvl1Required_DATA", "Error: Missing a Required file in the level 1 directory.")
            XML_add_attr("checkLvl1DirName_DATA", "FAIL")
            XML_add_txt("checkLvl1DirName_DATA", "Error: A Level 1 Directory does not follow proper naming convention.")
            XML_add_attr("checkLvl1DATAColumnNames", "FAIL")
            XML_add_txt("checkLvl1DATAColumnNames", "Error: File contains incorrect column names.")
            XML_add_attr("checkLvl1MISSINGColumnNames_DATA", "FAIL")
            XML_add_txt("checkLvl1MISSINGColumnNames_DATA", "Error: MISSING file contains incorrect column names.")
            
            
            line <- paste(tags$span(class = "bold-category", 
                                    "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
            flaggedMsgs <- c(flaggedMsgs, line)
          }
          else{
            # Log input
            XML_add_attr("checkLvl1DirName_DATA", "PASS")
            XML_add_attr("checkLvl1DATAColumnNames", "PASS")
            XML_add_attr("checkLvl1MISSINGColumnNames_DATA", "PASS")
            
            # Create the participant ID data frame based on the visit code in the level 1 
            # directory name. Column names have already been capitalized.
            participantIDDF <- getParticipantIDDF(participantIDFilePath$datapath, 
                                                  basename(lvl1DirPath), study_name)
            

            if (study_name == "OND01"){
              # Create the transfer ID data frame.
              transferIDDF <- read.csv(transferIDFilePath$datapath, stringsAsFactors = FALSE)
            }else{
              transferIDDF <- NULL
            }
            
            # MISSING Tabular Checks.
            flaggedMsgs <- c(flaggedMsgs, checkLvl1DATA(input, session, lvl1DirPath, 
                                                        numOfCheckboxes, participantIDDF,
                                                        transferIDDF, study_name, participantFile))
            
            # Get the subject IDs corresponding to the flagged messages.
            getInvalidLvl1DATA(input, output, session, lvl1DirPath, participantIDDF,
                               transferIDDF)
          }
          
          flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "DATA File", lvl)
          
          # Give static progress bar a final value of 100
          # at the end of computation.
          updateProgressBar(session = session, 
                            id = "tabularProgressBar",
                            value = 100)
        }
        
        # Print onto screen.
        output$tabularOutputText <- renderPrint(flaggedMsgs)
        
      }else{
      # If the study name is Package only checks we can skip the participantIDDF checks.
        if (length(lvl1DirPath) == 0){
          
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_DATA", "FAIL")
          XML_add_txt("outputSelection_DATA", "ERROR: Directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
          
        }
        else if (!dir.exists(lvl1DirPath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_DATA", "PASS")
          XML_add_attr("outputMissing_DATA", "FAIL")
          XML_add_txt("outputMissing_DATA", "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
          
        }
        
        else if (length(dataFileName) == 0){
          # Log input
          XML_add_attr("outputSelection_DATA", "PASS")
          XML_add_attr("outputMissing_DATA", "PASS")
          XML_add_attr("outputDoesNotExist_DATA", "FAIL")
          XML_add_txt("outputDoesNotExist_DATA" ,"ERROR: DATA file does not exist in the level 1 directory.
                         No checks can be performed.")
          flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "DATA file", lvl)
        }
      else{
        # Log input
        XML_add_attr("outputSelection_DATA", "PASS")
        XML_add_attr("outputMissing_DATA", "PASS")
        XML_add_attr("outputDoesNotExist_DATA", "PASS")
        XML_add_child("DATA", "<checkLvl1Required_DATA/>")
        
        
        # Declare variable for progress computation. 
        numOfCheckboxes <- 23
        
        # MANDATORY CHECKS:
        # 1) Check that the README file, the METHODS file, and at least one level 2 folder
        # exist in the level 1 directory, and that there is only 1 of each possible type
        # that is a file.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1Required(lvl1DirPath, "DATA"))
        updateProgressBar(session = session, 
                          id = "tabularProgressBar", 
                          value = 100 / numOfCheckboxes)
        
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0){
          # Log input
          XML_add_attr("checkLvl1Required_DATA", "PASS")
          XML_add_child("DATA", "<checkLvl1DATAColumnNames/>")
          XML_add_child("DATA", "<checkLvl1MISSINGColumnNames_DATA/>")
          
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1DATAColumnNames(lvl1DirPath))
          flaggedMsgs <- c(flaggedMsgs, checkLvl1MISSINGColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "tabularProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        # Log input
        XML_add_child("DATA", "<checkLvl1DirName_DATA/>")
        # 3) Check that the level 1 directory name is correct to get visit code
        # for subject ID, visit code, and date range checks.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(basename(lvl1DirPath)))
        updateProgressBar(session = session, 
                          id = "tabularProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 3 CHECKS PASS.
        if (length(flaggedMsgs) != 0){
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          # Create the participant ID data frame based on the visit code in the level 1 
          # directory name. Column names have already been capitalized.
          participantIDDF <- NULL
          transferIDDF <- NULL
          
          # DATA Tabular Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1DATA(input, session, lvl1DirPath, 
                                                      numOfCheckboxes, participantIDDF,
                                                      transferIDDF, study_name, participantFile))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl1DATA(input, output, session, lvl1DirPath, participantIDDF,
                             transferIDDF)
        }
      }
        
      flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "DATA File", lvl)
      
      # Give static progress bar a final value of 100
      # at the end of computation.
      updateProgressBar(session = session, 
                        id = "tabularProgressBar",
                        value = 100)
      
      }
      
      # Print onto screen.
      output$tabularOutputText <- renderPrint(flaggedMsgs)
    }
    
  )
  
  
  observeEvent(
    # Run code only if "Check MISSING" button is clicked.
    input$tabularCheckMISSINGButton, {
      
      # Log input
      XML_add_child("checks", "<MISSING/>")
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Clear output text section to make blank.
      output$tabularOutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$tabularOutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl1AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "tabularProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path, participant ID file path, and transfer ID file path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
      transferIDFilePath <- parseFilePaths(volumes, input$transferIDFile)
      missingFileName <- getFileName(lvl1DirPath, "MISSING.csv")
      
      # Log input
      XML_add_child("MISSING", "<outputSelection_MISSING/>")
      XML_add_child("MISSING", "<outputMissing_MISSING/>")
      
      # Making sure the participantFile is set as FALSE if "Package only checks"
      if (study_name == "Package only checks"){
        participantFile = FALSE
      }
      
      if (participantFile == TRUE){
        if (length(lvl1DirPath) == 0 || length(participantIDFilePath$datapath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING", "FAIL")
          XML_add_txt("outputSelection_MISSING", "ERROR: A level 1 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath) || !file.exists(participantIDFilePath$datapath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING", "PASS")
          XML_add_attr("outputMissing_MISSING", "FAIL")
          XML_add_txt("outputMissing_MISSING", "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        }
      }else{
        if (length(lvl1DirPath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING", "FAIL")
          XML_add_txt("outputSelection_MISSING", "ERROR: A level 1 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING", "PASS")
          XML_add_attr("outputMissing_MISSING", "FAIL")
          XML_add_txt("outputMissing_MISSING", "ERROR: The level 1 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        }
      }

      # Log input
      XML_add_child("MISSING", "<outputDoesNotExist_MISSING/>")
      if (length(missingFileName) == 0){
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "MISSING file", lvl)
        XML_add_attr("outputDoesNotExist_MISSING", "FAIL")
        XML_add_txt("outputDoesNotExist_MISSING" ,"ERROR: MISSING file does not exist in the level 1 directory.
                         No checks can be performed.")
      }
      else{
        # Log Input
        XML_add_attr("outputSelection_MISSING", "PASS")
        XML_add_attr("outputMissing_MISSING", "PASS")
        XML_add_attr("outputDoesNotExist_MISSING", "PASS")
        
        # Declare variable for progress computation.
        numOfCheckboxes <- 21
        
        # Log input
        XML_add_child("MISSING", "<checkLvl1MISSINGColumnNames/>")
        XML_add_child("MISSING", "<checkLvl1Required_MISSING/>")
        XML_add_child("MISSING", "<checkLvl1DirName_MISSING/>")
        
        # MANDATORY CHECKS:
        # 1) Check that the README file, the METHODS file, and at least one level 2 folder
        # exist in the level 1 directory, and that there is only 1 of each possible type
        # that is a file.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1Required(lvl1DirPath, "MISSING"))
        updateProgressBar(session = session, 
                          id = "tabularProgressBar", 
                          value = 100 / numOfCheckboxes)

        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl1Required_MISSING", "PASS")
          
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1MISSINGColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "tabularProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        
        if (study_name != "Package only checks"){
        # 3) Check that the level 1 directory name is correct to get visit code
        # for subject ID, visit code, and date range checks.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(basename(lvl1DirPath)))
        updateProgressBar(session = session, 
                          id = "tabularProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        
        }
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 3 CHECKS PASS.
        if (length(flaggedMsgs) != 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl1Required_MISSING", "FAIL")
          XML_add_attr("checkLvl1MISSINGColumnNames", "FAIL")
          XML_add_attr("checkLvl1DirName_MISSING", "FAIL")
          XML_add_txt("checkLvl1Required_MISSING", "Error: Missing a Required file in the level 1 directory.")
          XML_add_txt("checkLvl1MISSINGColumnNames", "Error: File contains incorrect column names.")
          XML_add_txt("checkLvl1DirName_MISSING", "Error: A Level 1 Directory does not follow proper naming convention.")
          
          
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          # Log input
          XML_add_attr("checkLvl1MISSINGColumnNames", "PASS")
          XML_add_attr("checkLvl1DirName_MISSING", "PASS")
          
          # Making sure the participantFile is set as FALSE if "Package only checks"
          if (study_name == "Package only checks"){
            participantFile = FALSE
          }
          
          # Create the participant ID data frame based on the visit code in the level 1
          # directory name. Column names have already been capitalized.
          if (participantFile == TRUE){
            participantIDDF <- getParticipantIDDF(participantIDFilePath$datapath, 
                                                  basename(lvl1DirPath), study_name)
            if (study_name == "OND01"){
              # Create the transfer ID data frame.
              transferIDDF <- read.csv(transferIDFilePath$datapath, stringsAsFactors = FALSE)
            }else{
              transferIDDF <- NULL
            }
          }else{
            participantIDDF <- NULL
            transferIDDF <- NULL
          }
          
          
          
          # MISSING Tabular Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1MISSING(input, session, lvl1DirPath, 
                                                         numOfCheckboxes, participantIDDF,
                                                         transferIDDF, study_name, participantFile))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl1MISSING(input, output, session, lvl1DirPath, participantIDDF,
                                transferIDDF)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "MISSING File", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "tabularProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$tabularOutputText <- renderPrint(flaggedMsgs)
    }
  )
}

# [END]