# selectServer.R

# Purpose: Create the server logic for the select tab to select the following by 
# browsing the user's local computer and home directory:
# 
#   1) A level 1 directory containing a non-tabular data package.
#   2) A participant ID file.
#   3) A transfer ID file.
#
# Author: Jedid Ahn (jahn@research.baycrest.org)
# Modified by: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2019-07-02
# Modified: 2020-06-11
# ========================================================================================

source("server/common/check/checkIftab.R")

selectServer <- function(input, output, session){
  
  # Computer volumes depend on OS: Windows, Mac, or Linux.
  volumes <- c(Home = fs::path_home(), getVolumes()())
  
  shinyDirChoose(input, id = "dir", roots = volumes,
                 restrictions = system.file(package = "base"))
  shinyFileChoose(input, id = "participantIDFile", roots = volumes, filetypes = c("csv"))
  shinyFileChoose(input, id = "transferIDFile", roots = volumes, filetypes = c("csv"))
  
  output$dirOutput <- renderText("No directory selected.")
  output$participantIDFileOutput <- renderText("No participant ID file selected.")
  output$transferIDFileOutput <- renderText("No transfer ID file selected.")
  
  # 1) Level 1 directory selection.
  observeEvent(
    input$dir, {
   
      dirPath <- parseDirPath(volumes, input$dir)
      
      # Reset output when selection of directory is cancelled.
      output$dirOutput <- renderText("No directory selected.")
      
      # Otherwise if directory has been selected, print path of directory to screen.
      if (length(dirPath) > 0){
        # Update output by printing new directory path.
        output$dirOutput <- renderPrint(dirPath)
        
        # produce output to show occurence of different file types
        extType <- c()
        dirPath <- parseDirPath(volumes, input$dir)
        lvl1FileNames <- list.files(path = dirPath, recursive = TRUE)
        for (item in lvl1FileNames){
          ext <- file_ext(item)
          extType <- c(ext, extType)
        }
        output$extTable <- renderTable(table(extType))
        
      }
    }
  )
  
  
  # 2) Participant ID file selection.
  observeEvent(
    input$participantIDFile, {
 
      
        participantIDFilePath <- parseFilePaths(volumes, input$participantIDFile)
        
        # Reset output when selection of participant ID file is cancelled.
        output$participantIDFileOutput <- renderText("No participant ID file selected.")
        # Otherwise if participant ID file has been selected, print path of participant ID 
        # file to screen.
        if (length(participantIDFilePath$datapath) > 0){
          # Update output by printing new participant ID file path.
          output$participantIDFileOutput <- renderText(participantIDFilePath$datapath)
        
      
      }
    }
  )
  
  
  # 3) Transfer ID file selection.
  observeEvent(
    input$transferIDFile, {
      transferIDFilePath <- parseFilePaths(volumes, input$transferIDFile)
      
      # Reset output when selection of transfer ID file is cancelled.
      output$transferIDFileOutput <- renderText("No transfer ID file selected.")
      
      # Otherwise if transfer ID file has been selected, print path of transfer ID file 
      # to screen.
      if (length(transferIDFilePath$datapath) > 0){
        # Update output by printing new transfer ID file path.
        output$transferIDFileOutput <- renderText(transferIDFilePath$datapath)
      }
    }
  )
  
  #Button to check if data is tabular and pop up for user to confirm 
  observeEvent(
    input$BeginCheck, {
      
      # Add user system information to the log file.
      XML_add_child("checks", "<user/>")
      user <- Sys.getenv("USERNAME")
      system_info <- paste(Sys.info()["sysname"], Sys.info()["release"])
      version <- R.Version()$version.string
      XML_general_attr("user", "user", user)
      XML_general_attr("user", "system", system_info)
      XML_general_attr("user", "version", version)
      XML_general_attr("user", "study_selection",  input$StudySelection)
      
      # Creating missing codes list and file_types lits which cannot be changed
      missingCodeList <- c("M_CB", "M_PI", "M_VR", "M_AE", "M_DNA", "M_TE", "M_NP", "M_ART", "M_TBC", "M_OTHER")
      file_typesList <- c("DATA.csv", "DICT.csv", "README.csv", "MISSING.csv", "FILELIST.csv", "METHODS.pdf",
                           "SUP.pdf", "SUP.txt", "SUP.csv", "GLOSSARY.pdf", "GLOSSARY.txt", "GLOSSARY.csv")
   

      # Make sure the codes input table has all columns filled
      if (len(studyCodeList) == 0 || len(cohortCodeList) == 0 || len(visitCodeList) == 0 || len(platformCodeList) == 0
          || len(siteCodeList) == 0 || len(missingCodeList) == 0 || len(file_typesList) == 0){
        shinyalert(title = "ERROR", 
                    text = "Please make sure all the columns in the 
                    code input table contains atleast one item.",
                    showCancelButton = FALSE,
                    showConfirmButton = TRUE,
                    confirmButtonText = "OK")
        
    }
    else{
      # Assume for now every section has been filled, (so no empty list or else we get an error)
      # Turn the code lists into a dataframe
      codes_df <- data.frame(substr(studyCodeList, 1,nchar(studyCodeList)-1), 
                             stringsAsFactors = FALSE)
      codes_df <- cbind.fill(codes_df, 
                             substr(cohortCodeList, 1,nchar(cohortCodeList)-1), 
                             substr(visitCodeList, 1,nchar(visitCodeList)-1),
                             substr(platformCodeList, 1,nchar(platformCodeList)-1),
                             substr(siteCodeList, 1,nchar(siteCodeList)-1),
                             missingCodeList, 
                             file_typesList,
                             fill = "")
      colnames(codes_df) <- c("ONDRI_CODES", "COHORT_CODES", "VISIT_CODES", 
                              "PLATFORM_CODES", "SITE_CODES", "MISSING_CODES",
                              "FILE_TYPES")
      
      # Converting variables to characters
      codes_df <- data.frame(sapply(codes_df, as.character), 
                             stringsAsFactors = FALSE)
      
      # Save the data
      # Duplicates are being saved as autofill, This needs to be removed.
      saveData(codes_df)
    }
      # check to see if user chose a study origin for data
      if (input$StudySelection == "- Choose A Study -"){
        # do nothing
      } else {

          dirpath <- parseDirPath(volumes, input$dir)
          
          type <- checkIfTab(dirpath)
          
          #the alert message depends on if the data is Tabular or Non-Tabular
          if (type == "non-tabular"){
            shinyalert(title = "Please Confirm", 
                       text = "We have detected that your
                     data may be of a non-tabular format. Is this correct?
                       
                       Note- If not the app will reset.",
                       showCancelButton = TRUE,
                       showConfirmButton = TRUE,
                       confirmButtonText = "Yes",
                       cancelButtonText = "No",
                       # using response
                       callbackR = function(response) {
                         if (response == FALSE){
                           # reset app
                           {js$reset()}
                         } 
                       })
            
            #enabling the tab if it was disabled
            js$disableTab("TabularChecks")
            js$enableTab("NonTabularChecks")
            
            #show the tabs to allow user to procced the standard checks
            showTab(inputId =  "tabs",target = "NonTabularChecks")
            showTab(inputId = "tabs", target = "TabularChecks")
            
            # hide select tab
            hideTab(inputId = "tabs", target = "Select")
            
            #switching user to 
            updateTabsetPanel(session, "tabs", selected = "NonTabularChecks")
            
          } else if (type == "tabular"){
            
            shinyalert(title = "Please Confirm", 
                       text = "We have detected that your
                    data may be of a tabular format. Is this correct?
                       
                        
                       Note- If not the app will reset .",
                       showCancelButton = TRUE,
                       showConfirmButton = TRUE,
                       confirmButtonText = "Yes",
                       cancelButtonText = "No",
                       # using response
                       callbackR = function(response) {
                         if (response == FALSE){
                           # reset app
                           {js$reset()}
                         } 
                       })
            
        
            #disabling the tab
            js$disableTab("NonTabularChecks")
            js$enableTab("TabularChecks")
            
            #show the tabs to allow user to procced the standard checks
            showTab(inputId =  "tabs",target = "NonTabularChecks")
            showTab(inputId = "tabs", target = "TabularChecks")
            
            
            # hide select tab
            hideTab(inputId = "tabs", target = "Select")
            
            #switching user to 
            updateTabsetPanel(session, "tabs", selected = "TabularChecks")
            
          }
          
      }
    }
  )
  
  # The id for the special pre saved additions will be Letters
  observeEvent(input$StudySelection, {
    # Removing items that are already in the table
    shiny::removeUI(
      selector = "#OND01_studyCode"
    )
    shiny::removeUI(
      selector = "#OND01_cohortCode"
    )
    shiny::removeUI(
      selector = "#OND01_visitCode"
    )
    shiny::removeUI(
      selector = "#OND01_platformCode"
    )
    shiny::removeUI(
      selector = "#OND01_siteCode"
    )
    # Removing items that are already in the table
    shiny::removeUI(
      selector = "#BEAM_studyCode"
    )
    shiny::removeUI(
      selector = "#BEAM_cohortCode"
    )
    shiny::removeUI(
      selector = "#BEAM_visitCode"
    )
    shiny::removeUI(
      selector = "#BEAM_platformCode"
    )
    shiny::removeUI(
      selector = "#BEAM_siteCode"
    )
    # Removing items that are already in the table
    shiny::removeUI(
      selector = "#OTHER_studyCode"
    )
    shiny::removeUI(
      selector = "#OTHER_cohortCode"
    )
    shiny::removeUI(
      selector = "#OTHER_visitCode"
    )
    shiny::removeUI(
      selector = "#OTHER_platformCode"
    )
    shiny::removeUI(
      selector = "#OTHER_siteCode"
    )
    # Removing items that are already in the table
    shiny::removeUI(
      selector = "#ReMiNDDTab_studyCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDDTab_cohortCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDDTab_visitCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDDTab_platformCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDDTab_siteCode"
    )
    # Removing items that are already in the table
    shiny::removeUI(
      selector = "#ReMiNDD_NonTab_studyCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDD_NonTab_cohortCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDD_NonTab_visitCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDD_NonTab_platformCode"
    )
    shiny::removeUI(
      selector = "#ReMiNDD_NonTab_siteCode"
    )
    # Making sure the lists are empty
    studyCodeList <<- c()
    cohortCodeList <<-c()
    visitCodeList <<- c()
    platformCodeList <<- c()
    siteCodeList <<- c()
    
    if (input$StudySelection == "BEAM"){
      # Adding a character at the end so these codes so don't get removed or lose a character
      # during the codes manipulation. (it tasks of the last character of the codes,
      # and the delete function refers to the last charater since it identifies it as an id).
      studyCodeList <<- c("BEM01B")
      cohortCodeList <<-c("CNCB")
      visitCodeList <<- c("01B")
      platformCodeList <<- c("CLINB", "NIMGB", "EYTKB", "SDOCTB", "GABLB", "NPSYB", "GNMCB")
      siteCodeList <<- c("BYCB", "CAMB", "SBHB", "SMHB", "TWHB")
     
      
      # For cohort code
      insertUI(
        selector = '#placeholder2',
        ui=div(style="background-color: #EA526F;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                    justify-content:space-between;
                    padding-right:5px;
             ",
               tags$p("CNC"),
               id = "BEAM_cohortCode"
        )
      )
        
        # For visit code
        insertUI(
          selector = '#placeholder3',
          ui=div(style="background-color: #EA526F;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                    justify-content:space-between;
                    padding-right:5px;
             ",
                 tags$p("01"),
                 id = "BEAM_visitCode"
          )
          )
        
        # For platform code
        insertUI(
          selector = '#placeholder4',
          ui=div(style="background-color: #EA526F;
                     height: 224px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
                 tags$p("CLIN"),
                 tags$p("NIMG"),
                 tags$p("EYTK"),
                 tags$p("SDOCT"),
                 tags$p("GABL"),
                 tags$p("NPSY"),
                 tags$p("GNMC"),
                 id = "BEAM_platformCode"
                 
          )
          
        )
        
        # For site code
        insertUI(
          selector = '#placeholder5',
          ui=div(style="background-color: #EA526F;
                     height: 164px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
                 tags$p("BYC"),
                 tags$p("CAM"),
                 tags$p("SBH"),
                 tags$p("SMH"),
                 tags$p("TWH"),
                 id = "BEAM_siteCode"
          )
          
        )
    }else if (input$StudySelection == "OND01"){
      
        # Adding a character at the end so these codes so don't get removed or lose a character
        # during the codes manipulation. (it taskes of the last character of the codes,
        # and the delete function refers to the last charater since it identifies it as an id).
        studyCodeList <<- c("OND01O", "OND02O", "OND03O", "OND04O", "OND05B", "OND06O", "OND07O")
        cohortCodeList <<-c("ADMCIO", "ALSO", "FTDO", "PDO", "VCIO")
        visitCodeList <<- c("01O", "02O", "03O", "04O", "05O", "06O", "07O", "08O")
        platformCodeList <<- c("CLINO", "NIMGO", "EYTKO", "SDOCO", "GABLO", "NPSYO", "GNMCO", "NPTHO", "NIBSO")
        siteCodeList <<- c("BYCO", "CAMO", "EBHO", "HDHO", "HGHO", "LHSO", "MCMO", "PCHO", "PKHO", "SBHO", "SMHO", "TBRO", "TOHO", "TWHO")
        
       
        
        # For cohort code
        insertUI(
          selector = '#placeholder2',
          ui=div(style="background-color: #EA526F;
                     height: 164px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
                 tags$p("ADMCI"),
                 tags$p("ALS"),
                 tags$p("FTD"),
                 tags$p("PD"),
                 tags$p("VCI"),
                 id = "OND01_cohortCode"
          )
        )
        
        # For visit code
        insertUI(
          selector = '#placeholder3',
          ui=div(style="background-color: #EA526F;
                     height: 254px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
                 tags$p("01"),
                 tags$p("02"),
                 tags$p("03"),
                 tags$p("04"),
                 tags$p("05"),
                 tags$p("06"),
                 tags$p("07"),
                 tags$p("08"),
                 id = "OND01_visitCode"
          )
        )

        # For platform code
        insertUI(
          selector = '#placeholder4',
          ui=div(style="background-color: #EA526F;
                     height: 284px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
                 tags$p("CLIN"),
                 tags$p("NIMG"),
                 tags$p("EYTK"),
                 tags$p("SDOC"),
                 tags$p("GABL"),
                 tags$p("NPSY"),
                 tags$p("GNMC"),
                 tags$p("NPTH"),
                 tags$p("NIBS"),
                 id = "OND01_platformCode"
                 
          )
          
        )
        
        # For site code
        insertUI(
          selector = '#placeholder5',
          ui=div(style="background-color: #EA526F;
                     height: 444px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
                 tags$p("BYC"),
                 tags$p("CAM"),
                 tags$p("EBH"),
                 tags$p("HDH"),
                 tags$p("HGH"),
                 tags$p("LHS"),
                 tags$p("MCM"),
                 tags$p("PCH"),
                 tags$p("PKH"),
                 tags$p("SBH"),
                 tags$p("SMH"),
                 tags$p("TBR"),
                 tags$p("TOH"),
                 tags$p("TWH"),
                 id = "OND01_siteCode"
          )
          
        )
        
    # Same study selection applies to both ReMiNDD tabular and non-tabular codes.
    }else if (input$StudySelection == "ReMiNDD"){
      # Adding a character at the end so these codes so don't get removed or lose a character
      # during the codes manipulation. (it taskes of the last character of the codes,
      # and the delete function refers to the last charater since it identifies it as an id).
      studyCodeList <<- c("OND06R")
      cohortCodeList <<-c("ALLR", "ADMCIR", "ALSR", "FTDR", "PDR", "VCIR")
      visitCodeList <<- c("01R")
      platformCodeList <<- c("CLINR", "GABLR", "NPSYR", "EYTKR", "GNMCR", "NPTHR", "SNSRR", "NIMGR")
      siteCodeList <<- c("SBHR")
      
      
      # For cohort code
      insertUI(
        selector = '#placeholder2',
        ui=div(style="background-color: #EA526F;
                     height: 194px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
               tags$p("ALL"),
               tags$p("ADMCI"),
               tags$p("ALS"),
               tags$p("FTD"),
               tags$p("PD"),
               tags$p("VCI"),
               id = "ReMiNDDTab_cohortCode"
        )
      )
      
      # For visit code
      insertUI(
        selector = '#placeholder3',
        ui=div(style="background-color: #EA526F;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
               tags$p("01"),
               id = "ReMiNDDTab_visitCode"
        )
      )
      
      # For platform code
      insertUI(
        selector = '#placeholder4',
        ui=div(style="background-color: #EA526F;
                     height: 284px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
               tags$p("CLIN"),
               tags$p("GABL"),
               tags$p("NPSY"),
               tags$p("EYTK"),
               tags$p("GNMC"),
               tags$p("NPTH"),
               id = "ReMiNDDTab_platformCode"
               
        )
        
      )
      
      # For site code
      insertUI(
        selector = '#placeholder5',
        ui=div(style="background-color: #EA526F;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                     flex-direction: column;
                    justify-content:space-between;
                    padding-right:5px;
             ",
               tags$p("SBH"),
               id = "ReMiNDDTab_siteCode"
        )
        
      )
    }else if (input$StudySelection == "Package only checks"){
      # Make the list NAs.
      studyCodeList <<- c("NA")
      cohortCodeList <<-c("NA")
      visitCodeList <<- c("NA")
      platformCodeList <<- c("NA")
      siteCodeList <<- c("NA")
    }
    
  })
  
  ## Codes editor Components
  
  ## Study code input functionality
  observeEvent(input$addStudyCode, {
    
      if(input$StudySelection == 'Custom'){
      nr <- input$addStudyCode
      id <- paste0("input",input$addStudyCode)
      insertUI(
        selector = '#placeholder',
        ui= div(id="OTHER_studyCode",
        
        div(style="background-color: #81D3EA;
                       height: 30px;
                       padding-left: 15px;
                       padding-top: 4px;
                       display: flex;
                      justify-content:space-between;
                      padding-right:5px;
               ",
          id = paste0("newInput",nr),
            tags$p(input$studyCode),
          actionButton(paste0('removeBtn',nr), 
                       label = '',
                       icon = icon("trash"))
        )
        )
      )
    }
    
    # Adding the code to a list
    studyCodeList <<- c(paste0(input$studyCode, nr), studyCodeList)

    observeEvent(input[[paste0('removeBtn',nr)]],{
      shiny::removeUI(
        selector = paste0("#newInput",nr),
        # Go through list and remove the item with the same
        # as the remove button which has been selected
        for (item in studyCodeList){
          if (str_sub(item, -1) == nr){
            studyCodeList <<- studyCodeList[studyCodeList %in% item == FALSE]
          }
        }
      )
    })
  })
  
  
  ## Cohort code input functionality
  # Keep track of elements inserted and not yet removed
  cohortCodeList <- c()
  
  observeEvent(input$addCohortCode, {
    nr2 <- input$addCohortCode
    id <- paste0("input",input$addCohortCode)
    insertUI(
      selector = '#placeholder2',
      ui= div(id="OTHER_cohortCode",
      div(style="background-color: #81D3EA;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                    justify-content:space-between;
                    padding-right:5px;
             ",
             id = paste0("newInput2",nr2),
             tags$p(input$cohortCode),
             actionButton(paste0('removeBtn2',nr2), 
                          label = '',
                          icon = icon("trash"))
      )
      )
    )
    
    # Adding the code to a list
    cohortCodeList <<- c(paste0(input$cohortCode, nr2), cohortCodeList)

    observeEvent(input[[paste0('removeBtn2',nr2)]],{
      shiny::removeUI(
        selector = paste0("#newInput2",nr2),
        # Go through list and remove the item with the same
        # as the remove button which has been selected
        for (item in cohortCodeList){
          if (str_sub(item, -1) == nr2){
            cohortCodeList <<- cohortCodeList[cohortCodeList %in% item == FALSE]
          }
        }
      )
    })
  })
  
  
  
  ## Visit code input functionality
  # Keep track of elements inserted and not yet removed
  visitCodeList <- c()
  
  observeEvent(input$addVisitCode, {
    nr3 <- input$addVisitCode
    id <- paste0("input",input$addVisitCode)
    insertUI(
      selector = '#placeholder3',
      ui=div(id="OTHER_visitCode",
      div(style="background-color: #81D3EA;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                    justify-content:space-between;
                    padding-right:5px;
             ",
             id = paste0("newInput3",nr3),
             tags$p(input$visitCode),
             actionButton(paste0('removeBtn3',nr3), 
                          label = '',
                          icon = icon("trash"))
      )
      )
    )
    
    # Adding the code to a list
    visitCodeList <<- c(paste0(input$visitCode, nr3), visitCodeList)

    observeEvent(input[[paste0('removeBtn3',nr3)]],{
      shiny::removeUI(
        selector = paste0("#newInput3",nr3),
        # Go through list and remove the item with the same
        # as the remove button which has been selected
        for (item in visitCodeList){
          if (str_sub(item, -1) == nr3){
            visitCodeList <<- visitCodeList[visitCodeList %in% item == FALSE]
          }
        }
      )
    })
  })
  
  ## Platform code input functionality
  # Keep track of elements inserted and not yet removed
  platformCodeList <- c()
  
  observeEvent(input$addPlatformCode, {
    nr4 <- input$addPlatformCode
    id <- paste0("input",input$addPlatformCode)
    insertUI(
      selector = '#placeholder4',
      ui=div(id="OTHER_platformCode",
      div(style="background-color: #81D3EA;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                    justify-content:space-between;
                    padding-right:5px;
             ",
             id = paste0("newInput4",nr4),
             tags$p(input$platformCode),
             actionButton(paste0('removeBtn4',nr4), 
                          label = '',
                          icon = icon("trash"))
      )
      )
    )
    
    # Adding the code to a list
    platformCodeList <<- c(paste0(input$platformCode, nr4), platformCodeList)

    observeEvent(input[[paste0('removeBtn4',nr4)]],{
      shiny::removeUI(
        selector = paste0("#newInput4",nr4),
        # Go through list and remove the item with the same
        # as the remove button which has been selected
        for (item in platformCodeList){
          if (str_sub(item, -1) == nr4){
            platformCodeList <<- platformCodeList[platformCodeList %in% item == FALSE]
          }
        }
      )
    })
  })
  ## Site code input functionality
  # Keep track of elements inserted and not yet removed
  siteCodeList <- c()
  
  observeEvent(input$addSiteCode, {
    nr5 <- input$addSiteCode
    id <- paste0("input",input$addSiteCode)
    insertUI(
      selector = '#placeholder5',
      ui=div(id="OTHER_siteCode",
      div(style="background-color: #81D3EA;
                     height: 30px;
                     padding-left: 15px;
                     padding-top: 4px;
                     display: flex;
                    justify-content:space-between;
                    padding-right:5px;
             ",
             id = paste0("newInput5",nr5),
             tags$p(input$siteCode),
             actionButton(paste0('removeBtn5',nr5), 
                          label = '',
                          icon = icon("trash"))
      )
      )
    )
    
    # Adding the code to a list
    siteCodeList <<- c(paste0(input$siteCode, nr5), siteCodeList)
    
    observeEvent(input[[paste0('removeBtn5',nr5)]],{
      shiny::removeUI(
        selector = paste0("#newInput5",nr5),
        # Go through list and remove the item with the same
        # as the remove button which has been selected
        for (item in siteCodeList){
          if (str_sub(item, -1) == nr5){
            siteCodeList <<- siteCodeList[siteCodeList %in% item == FALSE]
          }
        }
      )
    })
  })
  
}

# [END]