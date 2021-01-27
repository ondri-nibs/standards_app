# lvl2Server.R
#
# Purpose: Create the server logic for the level 2 tab for lvl 2 structure checks.
#
# Author: Jeremy Tanuan (jtanuan@research.baycrest.org)
#
# Date: 2019-06-19
#
# =======================================================================================

lvl2Server <- function(input, output, session){
  
  lvl2RequiredFileTypes <- c("README.csv", "DATAFILES")
  lvl <- 2
  
  # Disable checkboxes that are mandatory checks.
  disable("checkLvl2Required1")
  disable("checkLvl2Required2")
  disable("checkLvl2Required3")
  disable("checkLvl2Required4")
  disable("checkLvl2READMEColumnNames")
  disable("checkLvl2DICTColumnNames")
  disable("checkLvl2DATAColumnNames")
  disable("checkLvl2MISSINGColumnNames")
  disable("checkLvl1DirName3")
  disable("checkLvl1DirName4")
  
  observeEvent(
    # Run code only if "Check Directory" button is clicked.
    input$lvl2CheckDirButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
    
      # Log file Input
      XML_add_child("checks", "<DIR2/>")
      XML_general_attr("DIR2", "lvl", "2")
      
      # Clear output text section to make blank.
      output$lvl2OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl2OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl2AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 0)
      
      # Log Input
      XML_add_child("DIR2", "<outputSelection_DIR2/>")
      
      # Get lvl 1 directory path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)

      if (length(lvl1DirPath) == 0){
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_attr("outputSelection_DIR2", "FAIL")
        XML_add_txt("outputSelection_DIR2", "ERROR: A level 2 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
      }
      else if (!dir.exists(lvl1DirPath)){
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_child("DIR2", "<outputMissing_DIR2/>")
        XML_add_attr("outputMissing_DIR2", "FAIL")
        XML_add_txt("outputMissing_DIR2", "ERROR: The level 2 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        #Log Input
        XML_add_attr("outputSelection_DIR2", "PASS")
      }
      else{
        # Log Input
        XML_add_attr("outputSelection_DIR2", "PASS")
        XML_add_child("DIR2", "<outputMissing_DIR2/>")
        XML_add_attr("outputMissing_DIR2", "PASS")
        # Declare variable for progress computation.
        numOfCheckboxes <- 4
        
        # Lvl 2 Directory Checks.
        flaggedMsgs <- c(flaggedMsgs, checkLvl2Directory(input, session, lvl1DirPath, 
                                                         numOfCheckboxes, 
                                                      lvl2RequiredFileTypes, study_name))
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "Directory", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$lvl2OutputText <- renderPrint(flaggedMsgs)
    }
  )
  
  
  observeEvent(
    # Run code only if "Check README" button is clicked.
    input$lvl2CheckREADMEButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Clear output text section to make blank.
      output$lvl2OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl2OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl2AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      
      # Log input
      XML_add_child("checks", "<README2/>")
      XML_general_attr("README2", "lvl", "2")
      XML_add_child("README2", "<outputSelection_README2/>")
      
      if (length(lvl1DirPath) == 0){
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
      }
      else if (!dir.exists(lvl1DirPath)){
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_child("README2", "<outputMissing_README2/>")
        XML_add_attr("outputMissing_README2", "FAIL")
        XML_add_txt("outputMissing_README2", "ERROR: The level 2 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        #Log Input
        XML_add_attr("outputSelection_README2", "PASS")
      }
      else{
        # Log Input
        XML_add_attr("outputSelection_README2", "PASS")
        XML_add_child("README2", "<outputMissing_README2/>")
        XML_add_attr("outputMissing_README2", "PASS")
        XML_add_child("README2", "<checkLvl2Required_README2/>")
        
        # Declare variable for progress computation.
        numOfCheckboxes <- 10
        
        # MANDATORY CHECKS:
        # 1) Check that the README file and the DATAFILES folder exists in each level 2
        # directory, and that there is only 1 of each possible type.
        flaggedMsgs <- c(flaggedMsgs, checkLvl2Required(lvl1DirPath, "README",
                                                        lvl2RequiredFileTypes))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes)
        
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0  & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl1Required_README2", "PASS")
          XML_add_child("README2", "<checkLvl2READMEColumnNames/>")
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl2READMEColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "lvl2ProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
        if (length(flaggedMsgs) != 0){
          # Log input
          XML_add_attr("outputSelection_README2", "FAIL")
          XML_add_txt("outputSelection_README2", "ERROR: A level 2 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
          XML_add_attr("checkLvl2READMEColumnNames", "FAIL")
          XML_add_txt("checkLvl2READMEColumnNames", "ERROR: The column names check did not pass.")
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          #Log Input
          XML_add_attr("checkLvl2READMEColumnNames", "PASS")
          
          # README Tabular Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl2README(input, session, lvl1DirPath, 
                                                        numOfCheckboxes, study_name))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl2README(input, output, session, lvl1DirPath)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "README File(s)", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$lvl2OutputText <- renderPrint(flaggedMsgs)
    }
  )
  
  
  observeEvent(
    # Run code only if "Check DICT" button is clicked.
    input$lvl2CheckDICTButton, {
      
      study_name <- input$StudySelection
      
      # Clear output text section to make blank.
      output$lvl2OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl2OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl2AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      
      # Log input
      XML_add_child("checks", "<DICT2/>")
      XML_general_attr("DICT2", "lvl", "2")
      XML_add_child("DIC2T", "<outputSelection_DICT2/>" )
      XML_add_child("DICT2", "<outputMissing_DICT2/>")
      XML_add_child("DICT2", "<outputDoesNotExist_DICT2/>")
      
      if (length(lvl1DirPath) == 0){
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
        XML_add_attr("outputSelection_DICT2", "FAIL")
        XML_add_txt("outputSelection_DICT2", "ERROR: A level 2 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
      }
      else if (!dir.exists(lvl1DirPath)){
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
        #Log input 
        XML_add_attr("outputSelection_DICT2", 'PASS')
        XML_add_attr("outputMissing_DICT2", "FAIL")
        XML_add_txt("outputMissing_DICT2", "ERROR: The level 2 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
      }
      else if (length(checkForFileTypes(lvl1DirPath, "DICT.csv", lvl)) == 0){
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "DICT file", lvl)
        #Log input 
        XML_add_attr("outputSelection_DICT2", 'PASS')
        XML_add_attr("outputMissing_DICT2", "PASS")
        XML_add_attr("outputDoesNotExist_DICT2", "FAIL")
        XML_add_txt("outputDoesNotExist_DICT2" ,"ERROR: DICT file does not exist in the level 2 directory.
                         No checks can be performed.")
      }
      else{
        # Log input
        XML_add_attr("outputSelection_DICT2", 'PASS')
        XML_add_attr("outputMissing_DICT2", "PASS")
        XML_add_attr("outputDoesNotExist_DICT2", "PASS")
        # Declare variable for progress computation.
        numOfCheckboxes <- 11
        
        # Log input
        XML_add_child("DICT2", "<checkLvl2Required_DICT/>")
        XML_add_child("DICT2", "<checkLvl2DICTColumnNames/>")
        # MANDATORY CHECKS:
        # 1) Check that the README file and the DATAFILES folder exists in each level 2
        # directory, and that there is only 1 of each possible type.
        flaggedMsgs <- c(flaggedMsgs, checkLvl2Required(lvl1DirPath, "DICT"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes)
        
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl2Required_DICT", "PASS")
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl2DICTColumnNames(lvl1DirPath))
          flaggedMsgs <- c(flaggedMsgs, checkLvl2DATAColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "lvl2ProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
        if (length(flaggedMsgs) != 0){
          # Log input
          XML_add_attr("checkLvl2Required_DICT", "FAIL")
          XML_add_txt("checkLvl2Required_DICT","ERROR: The required check did not pass.")
          XML_add_attr("checkLvl2DICTColumnNames", "FAIL")
          XML_add_txt("checkLvl2DICTColumnNames", "ERROR: The column names check did not pass.")
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          # Log Input
          XML_add_attr("checkLvl2Required", "PASS")
          XML_add_attr("checkLvl2DICTColumnNames", "PASS")
          # DICT Tabular Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl2DICT(input, session, lvl1DirPath, 
                                                      numOfCheckboxes, study_name))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl2DICT(input, output, session, lvl1DirPath)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "DICT File(s)", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$lvl2OutputText <- renderPrint(flaggedMsgs)
    }
  )
  
  
  observeEvent(
    # Run code only if "Check DATA" button is clicked.
    input$lvl2CheckDATAButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Clear output text section to make blank.
      output$lvl2OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl2OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl2AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path, participant ID file path, and transfer ID file path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
      transferIDFilePath <- parseFilePaths(volumes, input$transferIDFile)
      
      # Log input
      XML_add_child("checks", "<DATA2/>")
      XML_general_attr("DATA2", "lvl", "2")
      XML_add_child("DATA2", "<outputSelection_DATA2/>" )
      XML_add_child("DATA2", "<outputMissing_DATA2/>")
      XML_add_child("DATA2", "<outputDoesNotExist_DATA2/>")
      
      # Making sure the participantFile is set as FALSE if "Package only checks"
      if (study_name == "Package only checks"){
        participantFile = FALSE
      }
      
      if (participantFile == TRUE){
        if (length(lvl1DirPath) == 0 || length(participantIDFilePath$datapath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_DATA2", "FAIL")
          XML_add_txt("outputSelection_DATA2", "ERROR: A level 2 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath) || !file.exists(participantIDFilePath$datapath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_DATA2", "PASS")
          XML_add_attr("outputMissing_DATA2", "FAIL")
          XML_add_txt("outputMissing_DATA2", "ERROR: The level 2 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        }
      
      else if (length(checkForFileTypes(lvl1DirPath, "DATA.csv", lvl)) == 0){
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "DATA file", lvl)
        # Log input
        XML_add_attr("outputSelection_DATA2", "PASS")
        XML_add_attr("outputMissing_DATA2", "PASS")
        XML_add_attr("outputDoesNotExist_DATA2", "FAIL")
        XML_add_txt("outputDoesNotExist_DATA2" ,"ERROR: DATA file does not exist in the level 2 directory.
                         No checks can be performed.")
      }
      else{
        # Log input
        XML_add_attr("outputSelection_DATA2", "PASS")
        XML_add_attr("outputMissing_DATA2", "PASS")
        XML_add_attr("outputDoesNotExist_DATA2", "PASS")
        XML_add_child("DATA2", "<checkLvl2Required_DATA/>")
        
        # Declare variable for progress computation.
        numOfCheckboxes <- 23
        
        # MANDATORY CHECKS:
        # 1) Check that the README file and the DATAFILES folder exists in each level 2
        # directory, and that there is only 1 of each possible type.
        flaggedMsgs <- c(flaggedMsgs, checkLvl2Required(lvl1DirPath, "DATA"))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes)
        
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0){
          # Log input
          XML_add_attr("checkLvl2Required_DATA", "PASS")
          XML_add_child("DATA2", "<checkLvl2DATAColumnNames/>")
          XML_add_child("DATA2", "<checkLvl2MISSINGColumnNames_DATA/>")
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl2DATAColumnNames(lvl1DirPath))
          flaggedMsgs <- c(flaggedMsgs, checkLvl1MISSINGColumnNames(lvl1DirPath))
          flaggedMsgs <- c(flaggedMsgs, checkLvl2MISSINGColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "lvl2ProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        # Log input
        XML_add_child("DATA", "<checkLvl2DirName_DATA/>")
        
        # 3) Check that the level 1 directory name is correct to get visit code
        # for subject ID, visit code, and date range checks.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(basename(lvl1DirPath)))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
        if (length(flaggedMsgs) != 0){
          # Log input
          XML_add_attr("checkLvl2Required_DATA", "FAIL")
          XML_add_txt("checkLvl2Required_DATA", "Error: Missing a Required file in the level 2 directory.")
          XML_add_attr("checkLvl2DirName_DATA", "FAIL")
          XML_add_txt("checkLvl2DirName_DATA", "Error: A Level 2 Directory does not follow proper naming convention.")
          XML_add_attr("checkLvl2DATAColumnNames", "FAIL")
          XML_add_txt("checkLvl2DATAColumnNames", "Error: File contains incorrect column names.")
          XML_add_attr("checkLvl2MISSINGColumnNames_DATA", "FAIL")
          XML_add_txt("checkLvl2MISSINGColumnNames_DATA", "Error: MISSING file contains incorrect column names.")
          
          
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          # Log input
          XML_add_attr("checkLvl2DirName_DATA", "PASS")
          XML_add_attr("checkLvl2DATAColumnNames", "PASS")
          XML_add_attr("checkLvl2MISSINGColumnNames_DATA", "PASS")
          
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
          flaggedMsgs <- c(flaggedMsgs, checkLvl2DATA(input, session, lvl1DirPath, 
                                                      numOfCheckboxes, participantIDDF,
                                                      transferIDDF, study_name, participantFile))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl2DATA(input, output, session, lvl1DirPath, participantIDDF,
                             transferIDDF)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "DATA File(s)", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$lvl2OutputText <- renderPrint(flaggedMsgs)
      
    }else{
      
      # If the study name is Package only checks we can skip the participantIDDF checks.
      if (length(lvl1DirPath) == 0){
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
        # Log input
        XML_add_attr("outputSelection_DATA2", "FAIL")
        XML_add_txt("outputSelection_DATA2", "ERROR: A level 2 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
      }
      else if (!dir.exists(lvl1DirPath)){
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
        # Log input
        XML_add_attr("outputSelection_DATA2", "PASS")
        XML_add_attr("outputMissing_DATA2", "FAIL")
        XML_add_txt("outputMissing_DATA2", "ERROR: The level 2 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
      }
      
      else if (length(checkForFileTypes(lvl1DirPath, "DATA.csv", lvl)) == 0){
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "DATA file", lvl)
        # Log input
        XML_add_attr("outputSelection_DATA2", "PASS")
        XML_add_attr("outputMissing_DATA2", "PASS")
        XML_add_attr("outputDoesNotExist_DATA2", "FAIL")
        XML_add_txt("outputDoesNotExist_DATA2" ,"ERROR: DATA file does not exist in the level 2 directory.
                         No checks can be performed.")
      }
      else{
        # Log input
        XML_add_attr("outputSelection_DATA2", "PASS")
        XML_add_attr("outputMissing_DATA2", "PASS")
        XML_add_attr("outputDoesNotExist_DATA2", "PASS")
        XML_add_child("DATA2", "<checkLvl2Required_DATA/>")
        
        # Declare variable for progress computation.
        numOfCheckboxes <- 23
        
          # Create the participant ID data frame based on the visit code in the level 1
          # directory name. Column names have already been capitalized.
          participantIDDF <- NULL
          transferIDDF <- NULL
          
          # MISSING Tabular Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl2DATA(input, session, lvl1DirPath, 
                                                      numOfCheckboxes, participantIDDF,
                                                      transferIDDF, study_name, participantFile))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl2DATA(input, output, session, lvl1DirPath, participantIDDF,
                             transferIDDF)
          
          flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "DATA File(s)", lvl)
          
          # Give static progress bar a final value of 100
          # at the end of computation.
          updateProgressBar(session = session, 
                            id = "lvl2ProgressBar",
                            value = 100)
        }
        
        # Print onto screen.
        output$lvl2OutputText <- renderPrint(flaggedMsgs)
      }
    }
  )
  
  
  observeEvent(
    # Run code only if "Check MISSING" button is clicked.
    input$lvl2CheckMISSINGButton, {
      
      # Log input
      XML_add_child("checks", "<MISSING2/>")
      XML_general_attr("MISSING2", "lvl", "2")
      
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Clear output text section to make blank.
      output$lvl2OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl2OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl2AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl2ProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path, participant ID file path, and transfer ID file path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
      transferIDFilePath <- parseFilePaths(volumes, input$transferIDFile)
    
      
      # Log input
      XML_add_child("MISSING2", "<outputSelection_MISSING2/>")
      XML_add_child("MISSING2", "<outputMissing_MISSING2/>")
      
      # Making sure the participantFile is set as FALSE if "Package only checks"
      if (study_name == "Package only checks"){
        participantFile = FALSE
      }
      
      if (participantFile == TRUE){
        if (length(lvl1DirPath) == 0 || length(participantIDFilePath$datapath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING2", "FAIL")
          XML_add_txt("outputSelection_MISSING2", "ERROR: A level 2 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath) || !file.exists(participantIDFilePath$datapath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING2", "PASS")
          XML_add_attr("outputMissing_MISSING2", "FAIL")
          XML_add_txt("outputMissing_MISSIN2G", "ERROR: The level 2 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        }
      }else{
        if (length(lvl1DirPath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING2", "FAIL")
          XML_add_txt("outputSelection_MISSING2", "ERROR: A level 2 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING2", "PASS")
          XML_add_attr("outputMissing_MISSING2", "FAIL")
          XML_add_txt("outputMissing_MISSIN2G", "ERROR: The level 2 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        }
      }
      
      # Log input
      XML_add_child("MISSING2", "<outputDoesNotExist_MISSING2/>")
      
      if (length(checkForFileTypes(lvl1DirPath, "MISSING.csv", lvl)) == 0){
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "MISSING file", lvl)
        XML_add_attr("outputDoesNotExist_MISSING2", "FAIL")
        XML_add_txt("outputDoesNotExist_MISSING2" ,"ERROR: MISSING file does not exist in the level 2 directory.
                         No checks can be performed.")
      }else{
        # Log Input
        XML_add_attr("outputSelection_MISSING2", "PASS")
        XML_add_attr("outputMissing_MISSING2", "PASS")
        XML_add_attr("outputDoesNotExist_MISSING2", "PASS")
        # Declare variable for progress computation.
        numOfCheckboxes <- 21
        
        # Log input
        XML_add_child("MISSING2", "<checkLvl2MISSINGColumnNames/>")
        XML_add_child("MISSING2", "<checkLvl2Required_MISSING/>")
        XML_add_child("MISSING2", "<checkLvl2DirName_MISSING/>")
        # MANDATORY CHECKS:
        # 1) Check that the README file and the DATAFILES folder exists in each level 2
        # directory, and that there is only 1 of each possible type.
        if (study_name != "Package only checks"){
          flaggedMsgs <- c(flaggedMsgs, checkLvl2Required(lvl1DirPath, "MISSING"))
          updateProgressBar(session = session, 
                            id = "lvl2ProgressBar", 
                            value = 100 / numOfCheckboxes)
        }
        
        
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl2Required_MISSING", "PASS")
          
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl2MISSINGColumnNames(lvl1DirPath))
          updateProgressBar(session = session,
                            id = "lvl2ProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        
        if( study_name != "Package only checks"){
          
        # 3) Check that the level 1 directory name is correct to get visit code
        # for subject ID, visit code, and date range checks.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(basename(lvl1DirPath)))
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        }
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
        if (length(flaggedMsgs) != 0 & study_name != "Package only checks"){
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
          # Log input
          XML_add_attr("checkLvl2Required_MISSING", "FAIL")
          XML_add_attr("checkLvl2MISSINGColumnNames", "FAIL")
          XML_add_attr("checkLvl2DirName_MISSING", "FAIL")
          XML_add_txt("checkLvl2Required_MISSING", "Error: Missing a Required file in the level 2 directory.")
          XML_add_txt("checkLvl2MISSINGColumnNames", "Error: File contains incorrect column names.")
          XML_add_txt("checkLvl12DirName_MISSING", "Error: A Level 2 Directory does not follow proper naming convention.")
          
        }
        else{
          # Log input
          XML_add_attr("checkLvl2MISSINGColumnNames", "PASS")
          XML_add_attr("checkLvl2DirName_MISSING", "PASS")
          
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
          flaggedMsgs <- c(flaggedMsgs, checkLvl2MISSING(input, session, lvl1DirPath, 
                                                         numOfCheckboxes, participantIDDF,
                                                         transferIDDF, study_name, participantFile))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl2MISSING(input, output, session, lvl1DirPath, participantIDDF,
                                transferIDDF)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "MISSING File(s)", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "lvl2ProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$lvl2OutputText <- renderPrint(flaggedMsgs)
    }
  )
}

# [END]