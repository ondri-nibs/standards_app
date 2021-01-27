# lvl3Server.R
#
# Purpose: Create the server logic for the level 3 tab for lvl 3 structure checks.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
#
# Date: 2019-07-02
#
# =======================================================================================

lvl3Server <- function(input, output, session){
  lvl3RequiredFileTypes <- c("FILELIST.csv")
  lvl <- 3
  
  # Disable checkboxes that are mandatory checks.
  disable("checkLvl3Required1")
  disable("checkLvl3Required2")
  disable("checkFILELISTColumnNames")
  disable("checkLvl3MISSINGColumnNames")
  disable("checkLvl1DirName5")
  disable("checkLvl1DirName6")
  
  observeEvent(
    # Run code only if "Check Directory" button is clicked.
    input$lvl3CheckDirButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      participantFile <- input$participantFile
      # Log file Input
      XML_add_child("checks", "<DIR3/>")
      XML_general_attr("DIR3", "lvl", "3")
      
      # Clear output text section to make blank.
      output$lvl3OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl3OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl3AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 0)
      
      # Log Input
      XML_add_child("DIR3", "<outputSelection_DIR3/>")
      # Get lvl 1 directory path and participant ID file path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
      
      # Making sure the participantFile is set as FALSE if "Package only checks"
      if (study_name == "Package only checks"){
        participantFile = FALSE
      }
      
      if (participantFile == TRUE){
        if (length(lvl1DirPath) == 0 || length(participantIDFilePath$datapath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          #Log Input
          XML_add_attr("outputSelection_DIR3", "FAIL")
          XML_add_txt("outputSelection_DIR3", "ERROR: A level 3 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath) || !file.exists(participantIDFilePath$datapath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          #Log Input
          XML_add_child("DIR3", "<outputMissing_DIR3/>")
          XML_add_attr("outputMissing_DIR3", "FAIL")
          XML_add_txt("outputMissing_DIR3", "ERROR: The level 3 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
          #Log Input
          XML_add_attr("outputSelection_DIR3", "PASS")
        
        }else{
          # Log Input
          XML_add_attr("outputSelection_DIR3", "PASS")
          XML_add_child("DIR3", "<outputMissing_DIR3/>")
          XML_add_attr("outputMissing_DIR3", "PASS")
          # Continue remaining checks
          # Declare variable for progress computation.
          numOfCheckboxes <- 4
          
          # Create the ID data frame based on the visit code in the level 1 directory name.
          # Column names have already been capitalized.
          participantIDDF <- getParticipantIDDF(participantIDFilePath$datapath, 
                                                basename(lvl1DirPath), study_name)
          
          # Lvl 3 Directory Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl3Directory(input, session, lvl1DirPath, 
                                                           numOfCheckboxes, 
                                                           lvl3RequiredFileTypes, 
                                                           participantIDDF, study_name))
          
          flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "Directory", lvl)
          
          # Give static progress bar a final value of 100
          # at the end of computation.
          updateProgressBar(session = session, 
                            id = "lvl3ProgressBar",
                            value = 100)
        }
          # Print onto screen.
          output$lvl3OutputText <- renderPrint(flaggedMsgs)
        
          
        
      }else{
        # If study selection is Package only checks
      if (length(lvl1DirPath) == 0){
        flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_attr("outputSelection_DIR3", "FAIL")
        XML_add_txt("outputSelection_DIR3", "ERROR: A level 3 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
      }
      else if (!dir.exists(lvl1DirPath)){
        flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
        #Log Input
        XML_add_child("DIR3", "<outputMissing_DIR3/>")
        XML_add_attr("outputMissing_DIR3", "FAIL")
        XML_add_txt("outputMissing_DIR3", "ERROR: The level 3 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
        #Log Input
        XML_add_attr("outputSelection_DIR3", "PASS")
        
      }else{
        # Log Input
        XML_add_attr("outputSelection_DIR3", "PASS")
        XML_add_child("DIR3", "<outputMissing_DIR3/>")
        XML_add_attr("outputMissing_DIR3", "PASS")
        # Continue remaining checks
          # Declare variable for progress computation.
          numOfCheckboxes <- 4
          
          # Create the ID data frame based on the visit code in the level 1 directory name.
          # Column names have already been capitalized.
          participantIDDF <- NULL
          
          # Lvl 3 Directory Checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl3Directory(input, session, lvl1DirPath, 
                                                           numOfCheckboxes, 
                                                           lvl3RequiredFileTypes, 
                                                           participantIDDF, study_name))
          
          flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "Directory", lvl)
          
          # Give static progress bar a final value of 100
          # at the end of computation.
          updateProgressBar(session = session, 
                            id = "lvl3ProgressBar",
                            value = 100)
      }
        # Print onto screen.
        output$lvl3OutputText <- renderPrint(flaggedMsgs)
      
      }
    }
  )
  
  
  observeEvent(
    # Run code only if "Check FILELIST" button is clicked.
    input$lvl3CheckFILELISTButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Log file Input
      XML_add_child("checks", "<FILELIST3/>")
      XML_general_attr("FILELIST3", "lvl", "3")
      
      # Clear output text section to make blank.
      output$lvl3OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl3OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl3AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 0)
      
      # Log Input
      XML_add_child("FILELIST3", "<outputSelection_FILELIST3/>")
      # Get lvl 1 directory path, participant ID file path, and transfer ID file path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
      transferIDFilePath <- parseFilePaths(volumes, input$transferIDFile)
      
      # Making sure the participantFile is set as FALSE if "Package only checks"
      if (study_name == "Package only checks"){
        participantFile = FALSE
      }
    
      if (participantFile == TRUE){
        if (length(lvl1DirPath) == 0 || length(participantIDFilePath$datapath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          #Log Input
          XML_add_attr("outputSelection_FILELIST3", "FAIL")
          XML_add_txt("outputSelection_FILELIST3", "ERROR: A level 3 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath) || !file.exists(participantIDFilePath$datapath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          #Log Input
          XML_add_child("FILELIST3", "<outputMissing_FILELIST3/>")
          XML_add_attr("outputMissing_FILELIST3", "FAIL")
          XML_add_txt("outputMissing_FILELIST3", "ERROR: The level 3 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
          #Log Input
          XML_add_attr("outputSelection_FILELIST3", "PASS")
          
        }else{
          # Log Input
          XML_add_attr("outputSelection_FILELIST3", "PASS")
          XML_add_child("FILELIST3", "<outputMissing_FILELIST3/>")
          XML_add_attr("outputMissing_FILELIST3", "PASS")
          # Declare variable for progress computation.
          numOfCheckboxes <- 22
          
          # MANDATORY CHECKS:
          # 1) Check that the DATAFILES folder exists in each level 2 directory, that the 
          # FILELIST file exists inside each DATAFILES folder, and that there is only
          # 1 of each possible file type.
          
          # Log input
          XML_add_child("FILELIST3", "<checkLvl3Required/>")
          flaggedMsgs <- c(flaggedMsgs, checkLvl3Required(lvl1DirPath, "FILELIST",
                                                          lvl3RequiredFileTypes))
          updateProgressBar(session = session, 
                            id = "lvl3ProgressBar", 
                            value = 100 / numOfCheckboxes)
          
          # Run only if 1) passes.
          if (length(flaggedMsgs) == 0){
            # Log input
            XML_add_attr("checkLvl3Required", "PASS")
            XML_add_child("FILELIST3", "<checkLvl3MISSINGColumnNames/>")
            # 2) Check that all the necessary column names exist, that they are correct,
            # and that there aren't any duplicates.
            # IMPORTANT: Need to check MISSING file in all 3 levels to aggregate
            # subject IDs.
            flaggedMsgs <- c(flaggedMsgs, checkFILELISTColumnNames(lvl1DirPath))
            flaggedMsgs <- c(flaggedMsgs, checkLvl1MISSINGColumnNames(lvl1DirPath))
            flaggedMsgs <- c(flaggedMsgs, checkLvl2MISSINGColumnNames(lvl1DirPath))
            flaggedMsgs <- c(flaggedMsgs, checkLvl3MISSINGColumnNames(lvl1DirPath))
            updateProgressBar(session = session, 
                              id = "lvl3ProgressBar", 
                              value = 100 / numOfCheckboxes * 2)
          }
        
          # 3) Check that the level 1 directory name is correct to get visit code
          # for subject ID, visit code, and date range checks.
          flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(basename(lvl1DirPath)))
          updateProgressBar(session = session, 
                            id = "lvl3ProgressBar", 
                            value = 100 / numOfCheckboxes * 3)
          
          
          # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
          if (length(flaggedMsgs) != 0){
            # Log input
            XML_add_attr("checkLvl3MISSINGColumnNames", "FAIL")
            XML_add_txt("checkLvl3MISSINGColumnNames", "ERROR: The column names check did not pass.")
            line <- paste(tags$span(class = "bold-category", 
                                    "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
            flaggedMsgs <- c(flaggedMsgs, line)
          }else{
            #Log Input
            XML_add_attr("checkLvl3MISSINGColumnNames", "PASS")
            # Create the participant ID data frame based on the visit code in the level 1 
            # directory name. Column names have already been capitalized.
            participantIDDF <- getParticipantIDDF(participantIDFilePath$datapath, 
                                                  basename(lvl1DirPath), study_name)
            
            # Create the transfer ID data frame.
            if (study_name == "OND01"){
              transferIDDF <- read.csv(transferIDFilePath$datapath, stringsAsFactors = FALSE)
            }else{
              transferIDDF <- NULL
            }
            
            # FILELIST Tabular Checks.
            flaggedMsgs <- c(flaggedMsgs, checkFILELIST(input, session, lvl1DirPath, 
                                                        numOfCheckboxes, participantIDDF,
                                                        transferIDDF, study_name, participantFile))
            
            # Get the subject IDs corresponding to the flagged messages.
            getInvalidFILELIST(input, output, session, lvl1DirPath, participantIDDF,
                               transferIDDF)
          }
          
          flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "FILELIST File(s)", lvl)
          
          # Give static progress bar a final value of 100
          # at the end of computation.
          updateProgressBar(session = session, 
                            id = "lvl3ProgressBar",
                            value = 100)
        
        }
        
        # Print onto screen.
        output$lvl3OutputText <- renderPrint(flaggedMsgs)
        
      }else{
        
        # If the study name is Package only checks we can skip the participantIDDF checks.
        if (length(lvl1DirPath) == 0){
          flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
          #Log Input
          XML_add_attr("outputSelection_FILELIST3", "FAIL")
          XML_add_txt("outputSelection_FILELIST3", "ERROR: A level 3 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          #Log Input
          XML_add_child("FILELIST3", "<outputMissing_FILELIST3/>")
          XML_add_attr("outputMissing_FILELIST3", "FAIL")
          XML_add_txt("outputMissing_FILELIST3", "ERROR: The level 3 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
          #Log Input
          XML_add_attr("outputSelection_FILELIST3", "PASS")
          
        }else{
          # Log Input
          XML_add_attr("outputSelection_FILELIST3", "PASS")
          XML_add_child("FILELIST3", "<outputMissing_FILELIST3/>")
          XML_add_attr("outputMissing_FILELIST3", "PASS")
          # Declare variable for progress computation.
          numOfCheckboxes <- 22
         
            # Create the participant ID data frame based on the visit code in the level 1 
            # directory name. Column names have already been capitalized.
            participantIDDF <- NULL
          
            # Create the transfer ID data frame.
            transferIDDF <- NULL
            
            # FILELIST Tabular Checks.
            flaggedMsgs <- c(flaggedMsgs, checkFILELIST(input, session, lvl1DirPath, 
                                                        numOfCheckboxes, participantIDDF,
                                                        transferIDDF, study_name, participantFile))
            
            # Get the subject IDs corresponding to the flagged messages.
            getInvalidFILELIST(input, output, session, lvl1DirPath, participantIDDF,
                               transferIDDF)
            
            flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "FILELIST File(s)", lvl)
            
            # Give static progress bar a final value of 100
            # at the end of computation.
            updateProgressBar(session = session, 
                              id = "lvl3ProgressBar",
                              value = 100)
        }
        }
      
      # Print onto screen.
      output$lvl3OutputText <- renderPrint(flaggedMsgs)
      }
  )
  
  
  observeEvent(
    # Run code only if "Check MISSING" button is clicked.
    input$lvl3CheckMISSINGButton, {
      
      # Saving the input selection from the selectUI
      study_name <- input$StudySelection
      
      # Saving radio button selection
      participantFile <- input$participantFile
      
      # Log input
      XML_add_child("checks", "<MISSING3/>")
      XML_general_attr("MISSING3", "lvl", "3")
      
      # Clear output text section to make blank.
      output$lvl3OutputText <- renderText("")
      # Clear output ID section first to make blank.
      output$lvl3OutputID <- renderText("")
      # Clear dropdown list.
      updateSelectInput(session, "lvl3AlgoSelection", label = "", choices = "")
      
      # Create an empty character vector in string format.
      flaggedMsgs <- character()
      
      # Give static progress bar a value of 0 before checking standards.
      updateProgressBar(session = session, 
                        id = "lvl3ProgressBar", 
                        value = 0)
      
      # Get lvl 1 directory path, participant ID file path, and transfer ID file path.
      volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
      lvl1DirPath <- parseDirPath(volumes, input$dir)
      participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
      transferIDFilePath <- parseFilePaths(volumes, input$transferIDFile)
      
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
          XML_add_attr("outputSelection_MISSING3", "FAIL")
          XML_add_txt("outputSelection_MISSING3", "ERROR: A level 3 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
        }
        else if (!dir.exists(lvl1DirPath) || !file.exists(participantIDFilePath$datapath)){
          flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
          # Log input
          XML_add_attr("outputSelection_MISSING3", "PASS")
          XML_add_attr("outputMissing_MISSING3", "FAIL")
          XML_add_txt("outputMissing_MISSING3", "ERROR: The level 3 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
          }
        }else{
          if (length(lvl1DirPath) == 0){
            flaggedMsgs <- outputSelectionErrorMsg(flaggedMsgs)
            # Log input
            XML_add_attr("outputSelection_MISSING3", "FAIL")
            XML_add_txt("outputSelection_MISSING3", "ERROR: A level 3 directory containing a non-tabular data package 
                       and/or a participant ID file and/or transfer ID file have not been
                       selected.")
          }
          else if (!dir.exists(lvl1DirPath)){
            flaggedMsgs <- outputMissingErrorMsg(flaggedMsgs)
            # Log input
            XML_add_attr("outputSelection_MISSING3", "PASS")
            XML_add_attr("outputMissing_MISSING3", "FAIL")
            XML_add_txt("outputMissing_MISSING3", "ERROR: The level 3 directory and/or the participant ID file and/or
                       the transfer ID file chosen are missing")
          }
        }
      # Log input
      XML_add_child("MISSING3", "<outputDoesNotExist_MISSING3/>")
      
      if (length(checkForFileTypes(lvl1DirPath, "MISSING.csv", lvl)) == 0){
        flaggedMsgs <- outputDoesNotExistMsg(flaggedMsgs, "MISSING file", lvl)
        XML_add_attr("outputDoesNotExist_MISSING3", "FAIL")
        XML_add_txt("outputDoesNotExist_MISSING3" ,"ERROR: MISSING file does not exist in the level 3 directory.
                         No checks can be performed.")
      }
      else{
        # Log Input
        XML_add_attr("outputSelection_MISSING3", "PASS")
        XML_add_attr("outputMissing_MISSING3", "PASS")
        XML_add_attr("outputDoesNotExist_MISSING3", "PASS")
        # Declare variable for progress computation.
        numOfCheckboxes <- 20
        
        # Log input
        XML_add_child("MISSING3", "<checkLvl3MISSINGColumnNames/>")
        XML_add_child("MISSING3", "<checkLvl3Required_MISSING/>")
        XML_add_child("MISSING3", "<checkLvl3DirName_MISSING/>")
        
        # Making sure the participantFile is set as FALSE if "Package only checks"
        if (study_name == "Package only checks"){
          participantFile = FALSE
        }
        
        # MANDATORY CHECKS:
        # 1) Check that DATAFILES folder exists in each level 2 directory, that the
        # FILELIST file exists inside each DATAFILES folder, and that there is only
        # 1 of each possible file type.
        if (participantFile == TRUE){
          
        flaggedMsgs <- c(flaggedMsgs, checkLvl3Required(lvl1DirPath, "MISSING",
                                                        lvl3RequiredFileTypes))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes)
        }
        # Run only if 1) passes.
        if (length(flaggedMsgs) == 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl3Required_MISSING", "PASS")
          # 2) Check that all the necessary column names exist, that they are correct,
          # and that there aren't any duplicates.
          flaggedMsgs <- c(flaggedMsgs, checkLvl3MISSINGColumnNames(lvl1DirPath))
          updateProgressBar(session = session, 
                            id = "lvl3ProgressBar", 
                            value = 100 / numOfCheckboxes * 2)
        }
        
        if( study_name != "Package only checks"){
        # 3) Check that the level 1 directory name is correct to get visit code
        # for subject ID, visit code, and date range checks.
        flaggedMsgs <- c(flaggedMsgs, checkLvl1DirName(basename(lvl1DirPath)))
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar", 
                          value = 100 / numOfCheckboxes * 3)
        }
        
        
        # ONLY PERFORM THE REST OF THE CHECKS IF THE ABOVE 2 CHECKS PASS.
        if (length(flaggedMsgs) != 0 & study_name != "Package only checks"){
          # Log input
          XML_add_attr("checkLvl3Required_MISSING", "FAIL")
          XML_add_attr("checkLvl3MISSINGColumnNames", "FAIL")
          XML_add_attr("checkLvl3DirName_MISSING", "FAIL")
          XML_add_txt("checkLvl3Required_MISSING", "Error: Missing a Required file in the level 3 directory.")
          XML_add_txt("checkLvl3MISSINGColumnNames", "Error: File contains incorrect column names.")
          XML_add_txt("checkLvl3DirName_MISSING", "Error: A Level 3 Directory does not follow proper naming convention.")
          
          line <- paste(tags$span(class = "bold-category", 
                                  "OTHER CHECKS WILL BE PERFORMED AFTER EXISTING ERRORS 
                                  ARE RESOLVED."))
          flaggedMsgs <- c(flaggedMsgs, line)
        }
        else{
          # Log input
          XML_add_attr("checkLvl3MISSINGColumnNames", "PASS")
          XML_add_attr("checkLvl3DirName_MISSING", "PASS")
          
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
          flaggedMsgs <- c(flaggedMsgs, checkLvl3MISSING(input, session, lvl1DirPath, 
                                                         numOfCheckboxes, participantIDDF,
                                                         transferIDDF, study_name, participantFile))
          
          # Get the subject IDs corresponding to the flagged messages.
          getInvalidLvl3MISSING(input, output, session, lvl1DirPath, participantIDDF,
                                transferIDDF)
        }
        
        flaggedMsgs <- combineFlaggedMsgs(flaggedMsgs, "MISSING File(s)", lvl)
        
        # Give static progress bar a final value of 100
        # at the end of computation.
        updateProgressBar(session = session, 
                          id = "lvl3ProgressBar",
                          value = 100)
      }
      
      # Print onto screen.
      output$lvl3OutputText <- renderPrint(flaggedMsgs)
    }
  )
}

# [END]