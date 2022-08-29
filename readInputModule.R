readInputDataUI <- function(id, label = "Read Input") {
  ns <- NS(id)
  tagList(
    div(id=ns("test"), class='dashboardPlotCard',
        tagList(
          uiOutput(ns("plotTitle")),
          div(id=ns("container1"), class="plotControlsAndBody",
              div(id=ns("controlsBanner"), class="bannerDivClass",
                  uiOutput(ns("bannerTest"))),
              div(id=ns("fileInfoContainer"), class="fileInfoContainer",
                  dataTableOutput(ns("fileInfoDT"))
              )
          )
        )

    )
  )
}


readInputDataServer <- function(id, inDept) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #########################
      # Output Variables      #
      #########################
      local.path <- "//Volumes//GoogleDrive//My Drive//"
      local.archive.path <- paste0(local.path, "ShinyApps//LocalDataArchive//")
      expenditures.path <- paste0(local.archive.path, "GEOGResearchExpenditures//")
      rds.path <- ".//Data//"
      `%out%` <- function(a,b) ! a %in% b
      fileInventoryLastUpdate <- c(NULL)
      fileInventoryFilename <- c(NULL)
      fileInventoryDescription <- c(NULL)
      fileInventoryMostRecent <- c(NULL)
      showDebugCues <- FALSE
      rvFI <- reactiveValues(fileInventory=NULL)
      classesStart <- data.frame(semester=c("202031", "202111", "202121", "202131", "202211", "202221"), startDate=as.Date(c("2020-08-19", "2021-01-19", "2021-06-01", "2021-08-30", "2022-01-18", "2022-05-31")))
      readFacultyListModuleUI <- function(id){
        ns <- NS(id)
        
        p("Reading Faculty List Data")
        
      }
      
      readFacultyListModuleServer <- function(id){
        moduleServer( id, function(input, output, session){
          require(readxl)
          
          ns <- session$ns
          
          ######################################
          # Data set up                        #
          ######################################
          toReturn <- NULL   
          
          ######################################
          # Data processing functions          #
          ######################################
          
          getMaxFaculty <- function(data){
            t.out <- data %>%
              mutate(end.date=case_when(
                (end.date==2050) ~ NA_real_,
                TRUE ~ end.date
              ))
            max(t.out[,c("start.date", "end.date")], na.rm=TRUE)
            
          }
          
          ######################################
          # Read Data                          #
          ######################################
          
          toReturn$faculty.list <- read_excel(".//Data//faculty.xlsx")
          toReturn$fileInventoryFilename <- c("faculty.xlsx")
          toReturn$fileInventoryDescription <- c("Names of faculty with start and end dates.")
          toReturn$fileInventoryLastUpdate <- c(format(file.info(".//Data//faculty.xlsx")$mtime, "%d-%b-%Y"))
          toReturn$fileInventoryMostRecent <- c(getMaxFaculty(toReturn$faculty.list))
          
          
          ######################################
          # Output functions                   #
          ######################################
          
          
          ######################################
          # Return Data                        #
          ######################################
          return(toReturn)
        })
      }      
      
      
      #########################
      # Input Control UIs     #
      #########################
      output$fileInfoDT <- renderDataTable({
        isolate(rvFI$fileInventory) %>%
          datatable(options=list(dom='t', ordering=F, pageLength=50), rownames=FALSE)
      })
      
      #########################
      # Card Elements         #
      #########################
      
      
      #########################
      # Observer Functions    #
      #########################
      
      
      
      #########################
      # Data processing and   #
      # preparation functions #
      #########################
     
      #available.faculty <- readRDS(".//Data//available.faculty.rds")
      available.faculty <- readRDS(".//Data/combinedData.rds") %>%
        select("Faculty", "shortSemester", "rank") %>%
        pivot_wider(names_from=shortSemester, values_from=rank)
      
      assign("available.faculty", available.faculty, pos=1)
      
      
      
      processExpenditures <- function(data){
        
        ####################################################3
        # This function is a quick fix.  It needs to be    #
        # modified to correctly identify the funds that    #
        # are truly classified as research.                #
        # 
        # Need to identify which funds are federal, state, #
        # foundation, private, etc.                        #
        # Numbers do not match what was originally         #
        # produced in Barbara's reports.  This is OK.      #
        # Better process will be to identify exactly which #
        # expenses should be classified as research.       #
        #
        # Challenge is how to classify things like         #
        # start-up.  T3, Pesca, x-grant.  Why are Cairns   #
        # expenditures so high?                            #
        # Some of the x-grant accounts are listed twice,   #
        # Why???                                           #
        ####################################################
        expenditures1a <- data
        names(expenditures1a) <- expenditures1a[1,]
        expenditures1a <- expenditures1a[-1,]
        names(expenditures1a) <- make.names(names(expenditures1a))
        groupedExpenditures <- expenditures1a %>%
          
          #filter((Function.Desc=="Research") | (str_detect(Sub.Account.Description, "X-Grant"))) %>%
          mutate(Function.Desc=case_when(
            str_detect(Sub.Account.Description, "X-Grant") ~ "Research",
            TRUE ~ Function.Desc
          )) %>%
          mutate(Function.Desc=case_when(
            str_detect(Sub.Account.Description, "T3") ~ "Research",
            TRUE ~ Function.Desc
          )) %>%
          mutate(Function.Desc=case_when(
            str_detect(Sub.Account.Description, "Pesca") ~ "Research",
            TRUE ~ Function.Desc
          )) %>%
          mutate(Fund.Source.Desc=case_when(
            str_detect(Sub.Account.Description, "X-Grant") ~ "X-Grant",
            TRUE ~ Fund.Source.Desc
          )) %>%
          mutate(Fund.Source.Desc=case_when(
            str_detect(Sub.Account.Description, "T3") ~ "T3",
            TRUE ~ Fund.Source.Desc
          )) %>%
          mutate(Fund.Source.Desc=case_when(
            str_detect(Sub.Account.Description, "Pesca") ~ "PESCA",
            TRUE ~ Fund.Source.Desc
          )) %>%
          mutate(Fund.Group.Desc=case_when(
            str_detect(Sub.Account.Description, "X-Grant") ~ "X-Grant",
            TRUE ~ Fund.Group.Desc
          )) %>%
          mutate(Fund.Group.Desc=case_when(
            str_detect(Sub.Account.Description, "T3") ~ "T3",
            TRUE ~ Fund.Group.Desc
          )) %>%
          mutate(Fund.Group.Desc=case_when(
            str_detect(Sub.Account.Description, "Pesca") ~ "PESCA",
            TRUE ~ Fund.Group.Desc
          )) %>%
          filter(Function.Desc=="Research") %>%
          filter(Fund.Source.Desc != "Gifts or Donations") %>%
          filter(Fund.Source.Desc != "Functional And General") %>%
          filter(Fund.Source.Desc != "Designated") %>%
          filter(Fund.Source.Desc != "Indirect Cost") %>%
          filter(Fund.Source.Desc != "Available University Fund") %>%
          filter(Fund.Group.Desc != "Functional And General") %>%
          filter(Fund.Group.Desc != "Designated") %>%
          filter(Fund.Group.Desc != "Indirect Cost") %>%
          filter(Fund.Group.Desc != "Available University Fund") %>%
          filter(!str_detect(P.I., "Greer")) %>%
          filter(!str_detect(P.I., "Hammond")) %>%
          select(starts_with("P") | starts_with("x")) %>%
          rename("faculty"="P.I.") %>%
          pivot_longer(-faculty, names_to="year", values_to="expenditures") %>%
          mutate(expendituresNumeric = as.numeric(expenditures)) %>%
          mutate(year = substring(year, 2,5)) %>%
          group_by(faculty, year) %>%
          filter(!is.na(expendituresNumeric)) %>%
          summarize(expenditures=sum(expendituresNumeric)) %>%
          arrange(year) %>%
          pivot_wider(id_cols=faculty, names_from="year", values_from="expenditures") %>%
          arrange(faculty)
        
        groupedExpenditures
      }
      read.researchExpenditures.July2019 <- function(raw.data.path, rds.data.path, fileInventoryName, fileInventoryDescription, fileInventoryLastUpdate, fileInventoryMostRecent){
        # check to see if expenditures.rds exists
        filename <- paste0(rds.data.path, "expenditures.rds")
        cat(yellow("[readInputDataServer] [read.researchExpendigures.July2019]"), green("beginning\n"))
        #cat(green("Beginning of read.researchExpenditures.July2019\n"))
        if(file.exists(filename)){
          cat(yellow("[readInputDataServer] [read.researchExpendigures.July2019]"), green("file exists\n"))
          #cat(green("file.exists\n"))
          # expenditures.rds exists : need to check if it is up-to-date
          expenditures <- readRDS(filename)
          current.date <- Sys.Date()
          current.year <- format(current.date, "%Y")     
          max.year.in.data <- unlist(expenditures[1,(dim(expenditures)[[2]]-1)])
          current.rds.file.creation.date <- attributes(expenditures)$file.creation.date
          
          ###########################################
          # Determine the newest file in the folder #
          #                                         #
          ###########################################
          in.pattern <- "*.xlsx"
          t.wd <- getwd()
          setwd(raw.data.path)
          t.fileinfo <- file.info(dir( pattern=in.pattern))
          csv.pattern <- "*.csv"
          t.fileinfo.csv <- file.info(dir(pattern=csv.pattern))
          setwd(t.wd)
          newest.creation.date <- max(t.fileinfo[,"mtime"])
          rawFileName <- rownames(t.fileinfo)[t.fileinfo[,"mtime"]==newest.creation.date]
          # set the raw.data.file name
          raw.data.file <- paste0(raw.data.path, rawFileName)
          cat("raw.data.file:", raw.data.file, "\n")

          latest.file.creation.date <- date(file.info(raw.data.file)$mtime)
          fileInventoryFilename <<- c(fileInventoryFilename, rawFileName)
          fileInventoryDescription <<- c(fileInventoryDescription, "Research Expenditures.")
          fileInventoryLastUpdate <<- c(fileInventoryLastUpdate, format(latest.file.creation.date, "%d-%b-%Y"))
          fileInventoryMostRecent <<- c(fileInventoryMostRecent, format(latest.file.creation.date, "%d-%b-%Y"))
          cat(red("current.rds.file.creation.date:", as.character(current.rds.file.creation.date), "\n"))
          cat(red("latest.file.creation.date:", as.character(latest.file.creation.date), "\n"))
          cat(green("before the ifs\n"))
          
          #####################################################
          # Determine if the creation dates for the CSV files #
          # are newer than the excel files.                   #
          #####################################################
          
          if(!is.null(current.rds.file.creation.date)){
            if(current.rds.file.creation.date==latest.file.creation.date){
              cat(green("No need to update\n"))
            } else {
              cat("before update with newest file\n")
              # update the data using the newest file
              raw.data.file <- paste0(raw.data.path, rawFileName)
              cat("raw.data.file:", raw.data.file, ".\n")
              assign("t.raw.data.file", raw.data.file, pos=1)
              assign("t.processExpenditures", processExpenditures, pos=1)
              
              expenditures <- read_excel(raw.data.file, sheet=1)  ###  read the first sheet ###
              
              expenditures1 <- read_excel(raw.data.file, sheet=3)
              expenditures1 <- processExpenditures(expenditures1)
              cat(green("Case1 **********************\n"))
              #assign("expenditures1", expenditures1, pos=1)
              attributes(expenditures)$file.creation.date <- date(file.info(raw.data.file)$mtime)
              saveRDS(expenditures, filename)
              #assign("expenditures", expenditures, pos=1)
              cat(green("Updated", filename, "\n"))        
            }
          } else {
            cat(yellow("[readInputDataServer] [read.researchExpendigures.July2019]"), green("in the else\n"))
            # update the data using the newest file
            #cat(red("expenditures file does not have a file creation date\n"))
            cat(green("Case2 *************************\n"))
            raw.data.file <- paste0(raw.data.path, rawFileName)
            expenditures <- read_excel(raw.data.file)   ### read the first sheet ###
            expenditures1 <- read_excel(raw.data.file, sheet=3)
            expenditures1 <- processExpenditures(expenditures1)
            #assign("expenditures1", expenditures1, pos=1)
            attributes(expenditures)$file.creation.date <- date(file.info(raw.data.file)$mtime)
            saveRDS(expenditures, filename)
            #assign("expenditures", expenditures, pos=1)
            cat("Wrote2", filename, "\n")
          }
        } else {
          # expenditures.rds does not exist.  Need to create it.
          cat(yellow("[readInputDataServer] [read.researchExpendigures.July2019]"), green("Case 3\n"))
          #cat(green("Case3 **********************\n"))
          in.pattern <- "*.xlsx"
          t.wd <- getwd()
          setwd(path)
          t.fileinfo <- file.info(dir( pattern=in.pattern))
          setwd(t.wd)
          newest.creation.date <- max(t.fileinfo[,"mtime"])
          rawFileName <- rownames(t.fileinfo)[t.fileinfo[,"mtime"]==newest.creation.date]
          
          raw.data.file <- paste0(raw.data.path, rawFileName)
          
          expenditures <- read_excel(raw.data.file, sheet=1)    ### read the first sheet   ####
          expenditures1 <- read_excel(raw.data.file, sheet=3)
          expenditures1 <- processExpenditures(expenditures1)
          #assign("expenditures1", expenditures1, pos=1)
          
          
          #################################################
          # Clean up the format of the expenditures file  #
          #################################################
          
          
          attributes(expenditures)$file.creation.date <- date(file.info(raw.data.file)$mtime)
          saveRDS(expenditures, filename)
          #assign("expenditures", expenditures, pos=1)
          #cat("Wrote", filename, "\n")
        }
        cat(yellow("[readInputDataServer] [read.researchExpendigures.July2019]"), green("before processing\n"))
        #cat("Before processing\n")
        assign("t.exp", expenditures, pos=1)
        # names(expenditures)[1] <- "facultyName"
        # cat(blue("Reached this stage.\n"))
        #View(expenditures)
        t.colNames <- expenditures[1,]
        t.colNames[1] <- "facultyName"
        t.colNames[2] <- "tenureStatus"
        t.colNames <- as.vector(unlist(t.colNames))
        t.colNames[c(3:(length(t.colNames)-1))] <- as.character(floor(as.numeric(t.colNames[c(3:(length(t.colNames)-1))])))
        t.colNames[length(t.colNames)] <- "Sum"
        names(expenditures) <- as.character(unlist(t.colNames))
        expenditures <- expenditures[c(-1,-nrow(expenditures)),-ncol(expenditures)]
        expenditures <- expenditures %>%
          filter(facultyName != "Ewing, Ryan C") %>%
          filter(facultyName != "Newman, Julie") %>%
          filter(facultyName != "Nielsen-Gammon, John W") %>%
          mutate(facultyName = case_when(
            facultyName == "Minguez, Julie" ~ "Loisel, Julie",
            TRUE ~ as.character(facultyName)))
        cat(yellow("[readInputDataServer] [read.researchExpendigures.July2019]"), green("after expenditures\n"))
        
        #cat(green("End of readExpenditures\n"))
        #assign("exp1", expenditures, pos=1)
        #assign("exp1a", expenditures1, pos=1)
        expenditures
      }
      read.researchExpenditures.new <- function(raw.data.path, rds.data.path){
        # check to see if expenditures.rds exists
        #cat("rds.data.path:", rds.data.path, "\n")
        #cat("raw.data.path:", raw.data.path, "\n")
        filename <- paste0(rds.data.path, "expenditures.rds")
        if(file.exists(filename)){
          # expenditures.rds exists : need to check if it is up-to-date
          expenditures <- readRDS(filename)
          current.date <- Sys.Date()
          current.year <- format(current.date, "%Y")     
          max.year.in.data <- unlist(expenditures[1,(dim(expenditures)[[2]]-1)])
          current.rds.file.creation.date <- attributes(expenditures)$file.creation.date
          raw.data.file <- paste0(raw.data.path, "GEOG-Expenditures_by_CY_and_P.I._Research_Accounts.xlsx")
          latest.file.creation.date <- date(file.info(raw.data.file)$mtime)
          cat("current.rds.file.creation.date:", as.character(current.rds.file.creation.date), "\n")
          cat("latest.file.creation.dateX:", as.character(latest.file.creation.date), "\n")
          
          if(!is.null(current.rds.file.creation.date)){
            if(current.rds.file.creation.date==latest.file.creation.date){
              cat("No need to update\n")
            } else {
              # update the data using the newest file
              raw.data.file <- paste0(raw.data.path, "GEOG-Expenditures_by_CY_and_P.I._Research_Accounts.xlsx")
              expenditures <- read_excel(raw.data.file)
              attributes(expenditures)$file.creation.date <- date(file.info(raw.data.file)$mtime)
              saveRDS(expenditures, filename)
              assign("expenditures", expenditures, pos=1)
              #cat("Wrote1", filename, "\n")        
            }
          } else {
            # update the data using the newest file
            #cat("expenditures file does not have a file creation date\n")
            raw.data.file <- paste0(raw.data.path, "GEOG-Expenditures_by_CY_and_P.I._Research_Accounts.xlsx")
            expenditures <- read_excel(raw.data.file)
            attributes(expenditures)$file.creation.date <- date(file.info(raw.data.file)$mtime)
            saveRDS(expenditures, filename)
            assign("expenditures", expenditures, pos=1)
            #cat("Wrote2", filename, "\n")
          }
        } else {
          # expenditures.rds does not exist.  Need to create it.
          raw.data.file <- paste0(raw.data.path, "GEOG-Expenditures_by_CY_and_P.I._Research_Accounts.xlsx")
          
          expenditures <- read_excel(raw.data.file)
          attributes(expenditures)$file.creation.date <- date(file.info(raw.data.file)$mtime)
          saveRDS(expenditures, filename)
          assign("expenditures", expenditures, pos=1)
          #cat("Wrote", filename, "\n")
        }
        #cat("At end of read.researchExpenditures.new\n")
      }
      read.researchExpenditures.CLGE <- function(raw.data.path){
        # read the data to allow comparison of CLGE research expenditures
        # between departments.
        geog.data.file <- paste0(raw.data.path, "GEOG-Expenditures by CY and P.I. Research Accounts.xlsx")
        atmo.data.file <- paste0(raw.data.path, "ATMO-Expenditures by CY and P.I. Research Accounts.xlsx")
        ocng.data.file <- paste0(raw.data.path, "OCNG-Expenditures by CY and P.I. Research Accounts.xlsx")
        geol.data.file <- paste0(raw.data.path, "GEOL-Expenditures by CY and P.I. Research Accounts.xlsx")
        
        geog.expenditures <- read_excel(geog.data.file)
        atmo.expenditures <- read_excel(atmo.data.file)
        ocng.expenditures <- read_excel(ocng.data.file)
        geol.expenditures <- read_excel(geol.data.file)
        
        geog.expenditures <- geog.expenditures[c(1,dim(geog.expenditures)[1]), c(3:7)]
        geog.expenditures <- data.frame(t(geog.expenditures))
        names(geog.expenditures) <- c("Year", "Expenditures")
        geog.expenditures <- data.frame(geog.expenditures, Dept="GEOG")
        
        atmo.expenditures <- atmo.expenditures[c(1,dim(atmo.expenditures)[1]), c(3:7)]
        atmo.expenditures <- data.frame(t(atmo.expenditures))
        names(atmo.expenditures) <- c("Year", "Expenditures")
        atmo.expenditures <- data.frame(atmo.expenditures, Dept="ATMO")
        
        ocng.expenditures <- ocng.expenditures[c(1,dim(ocng.expenditures)[1]), c(3:7)]
        ocng.expenditures <- data.frame(t(ocng.expenditures))
        names(ocng.expenditures) <- c("Year", "Expenditures")
        ocng.expenditures <- data.frame(ocng.expenditures, Dept="OCNG")
        
        geol.expenditures <- geol.expenditures[c(1,dim(geol.expenditures)[1]), c(3:7)]
        geol.expenditures <- data.frame(t(geol.expenditures))
        names(geol.expenditures) <- c("Year", "Expenditures")
        geol.expenditures <- data.frame(geol.expenditures, Dept="GEOL")
        
        out.data <- rbind(geog.expenditures, atmo.expenditures, geol.expenditures, ocng.expenditures)
        
        out.data
      }
      
      
      
      readCommitteeReport <- function(year=2016, semester="A", path, report.stem){
        require(dplyr)
        #cat("In readCommitteeReport\n")
        #cat("path:", path, "\n")
        #cat("report.stem:", report.stem, "\n")
        #cat("year:", year, "\n")
        #cat("semester:", semester, "\n")
        cat("Processing:", paste0(path, report.stem, year, semester, ".csv"), "\n")
        t.1 <- read.table(paste0(path, report.stem, year, semester, ".csv"), sep=",", header=TRUE)
        
        # Filter for GEOG faculty
        # Some GEOG faculty are classified as CLGE due to ad-loc
        #
        
        t.geog1 <- t.1 %>%
          filter((FACULTYDEPT == "GEOG") | 
                   (FACULTYNAME == "Houser, Christopher") |
                   (FACULTYNAME == "Bednarz, Sarah W.") |
                   (FACULTYNAME == "Brannstrom, Christian")) %>%
          filter(!grepl("Gold Bouchot", FACULTYNAME)) %>%
          mutate(Year= year) %>%
          mutate(Semester=paste0(year, semester))
        
        t.geog1
      }
      
      determineIfUpdateNeeded <- function(rds.path, rds.filename, local.archive.path, 
                                          local.archive.subdirectory, in.pattern){
        path <- paste0(local.archive.path, local.archive.subdirectory)
        t.wd <- getwd()
        setwd(path)
        t.fileinfo <- file.info(dir( pattern=in.pattern))
        setwd(t.wd)
        newest.creation.date <- max(t.fileinfo[,"mtime"])
        if(file.exists(paste0(rds.data.path, rds.filename))) {
          rdsInfo <- file.info(paste0(rds.data.path, rds.filename))
          if(is.na(file.info(paste0(rds.path, rds.filename))$mtime)==TRUE)
            force.create <- TRUE
          else
            force.create <- FALSE
          if(newest.creation.date > rdsInfo$mtime)
            force.create <- TRUE
        } else {
          force.create <- TRUE
        }
        
        force.create
      }
      
      
      createGraduateCommitteesHistorical <- function(rds.data.path, local.archive.path, 
                                                     local.archive.subdirectory, force.create){
        #path <- "//Volumes//GoogleDrive//My Drive//ShinyApps//LocalDataArchive//GraduateCommitteeData//"
        #path <- paste0(local.archive.path, "GraduateCommitteeData//")
        #cat(green("In createGraduateCommitteesHistorical\n"))
        path <- paste0(local.archive.path, local.archive.subdirectory)
        report.stem <- "SRC_AL_GRAD_COMM_GE_CSV_"
        require(dplyr)
        
        #rework the next few lines into a checkForUpdates function that is 
        #generic and can work in many situations.  Will need arguements for 
        #rds.path, local.archive.path, and pattern (i.e. "*.csv")
        
        
        
        #t.wd <- getwd()
        #setwd(path)
        #t.fileinfo <- file.info(dir( pattern="*.csv"))
        #setwd(t.wd)
        #newest.creation.date <- max(t.fileinfo[,"mtime"])
        #rdsInfo <- file.info(paste0(rds.data.path, "gradcommittees.rds"))
        #if(is.na(file.info(paste0(rds.path, "gradcommittees.rds"))$mtime)==TRUE)
        #force.create <- TRUE
        if (showDebugCues==TRUE) cat(green("before needUpdate\n"))
        needUpdate <- determineIfUpdateNeeded(rds.data.path, "gradcommittees.rds", local.archive.path,
                                              local.archive.subdirectory, in.pattern="*.csv")
        #cat(green("paste needUpdate\n"))
        #if((newest.creation.date > rdsInfo$mtime) | (force.create==TRUE)){
        if((needUpdate==TRUE)|(force.create==TRUE)){
          #cat("update the data\n")
          all.files <- dir(paste0(local.archive.path, "GraduateCommitteeData"), pattern="*.csv")
          for(i in 1:length(all.files)){
            in.year <- as.numeric(substr(all.files[i], regexpr("\\.", all.files[i])-5, regexpr("\\.",all.files[i])-2))
            in.semester <- substr(all.files[i], regexpr("\\.", all.files[i])-1, regexpr("\\.",all.files[i])-1)
            if(i == 1){
              fullData <- readCommitteeReport(in.year, in.semester, path, report.stem)
            } else {
              fullData <- bind_rows(fullData, readCommitteeReport(in.year, in.semester, path, report.stem))
            }
          }
          saveRDS(fullData, paste0(rds.path, "gradcommittees.rds"))
        } else {
          #1 cat("Data are up to date\n")
          fullData <- readRDS(paste0(rds.path, "gradcommittees.rds"))
        }
        # 
        fullData
      }
      
      readIDCData <- function(fileInventoryFilename, fileInventoryDescription, fileInventoryLastUpdate){
        require(readxl)
        require(crayon)
        require(dplyr)
        
        #######################################################
        # Function Definintions                               #
        #######################################################
        processSplits <- function(inData, geogPIs.local=NULL, debugFlag=FALSE){
          outData <- inData
          #filter to only process the DEPT IDC
          t.deptAllocations <- inData %>%
            filter((idcType=="DEPT") | (idcType=="Dept"))
          
          #if(debugFlag) print(t.months)
          for(i in 1:dim(t.deptAllocations)[1]){
            targetAccount <- as.vector(unlist(t.deptAllocations[i, "projectAccount"]))
            targetYear <- as.vector(unlist(t.deptAllocations[i, "fiscalYear"]))
            targetMonth <- as.vector(unlist(t.deptAllocations[i, "fyMonth"]))
            if(debugFlag) print(targetMonth)
            #tempData <- t.new %>%
            tempData <- inData %>%
              filter(projectAccount == targetAccount) %>%
              filter(fiscalYear == targetYear) %>%
              filter(fyMonth == targetMonth)
            
            #Need to proceed only with the non-geography PIs.
            # find a way to further filter the data and then 
            # perform a check so that null data aren't processed.
            
            if(debugFlag) cat(red("##################################\n"))
            if(debugFlag==TRUE) print(tempData)
            
            
            if((dim(tempData)[1]) == 2) {
              #if(debugFlag) cat(green("Single PI case\n"))
              # single PI case
              PIData <- tempData %>%
                filter(idcType=="PI")
              thePI <- substr(PIData$description, 13, nchar(PIData$description))
              targetAccount <- as.vector(unlist(t.deptAllocations[i, "projectAccount"]))
              targetYear <- as.vector(unlist(t.deptAllocations[i, "fiscalYear"]))
              targetMonth <- as.vector(unlist(t.deptAllocations[i, "fyMonth"]))
              
              #print(table(outData$facultyName))
              #cat(blue("**************\n"))
              outData <- outData %>%
                mutate(facultyName = case_when(
                  ((projectAccount == targetAccount) &
                     (fiscalYear == targetYear) &
                     (fyMonth == targetMonth) &
                     (idcType=="DEPT")) ~ thePI,
                  TRUE ~ facultyName
                ))
              #print(table(outData$facultyName))
            } else if((dim(tempData)[1]) > 1){
              # Multiple GEOG co-PIs.
              if(debugFlag) cat(green("Multiple GEOG co-PIs\n"))
              PIData <- tempData %>%
                filter(idcType=="PI")
              thePI <- substr(PIData$description, 13, nchar(PIData$description))
              
              t.deptData <- tempData %>%
                filter((idcType=="DEPT") | (idcType=="Dept")) %>%
                mutate(facultyName="Mouse, Mickey")
              t.PIdata <- tempData %>%
                filter(idcType=="PI")
              
              for(i in 1:dim(t.PIdata)[1]){
                sumPct <- sum(as.numeric(t.PIdata$pctAllocation))
                singlePIProportion <- as.numeric(t.PIdata$pctAllocation[i])/sumPct
                idcContribution <- as.numeric(t.deptData$budget)*singlePIProportion
                
                if(i==1){
                  if(debugFlag) cat(green("First co-PI\n"))
                  targetAccount <- as.vector(unlist(t.PIdata[i, "projectAccount"]))
                  targetYear <- as.vector(unlist(t.PIdata[i, "fiscalYear"]))
                  targetMonth <- as.vector(unlist(t.PIdata[i, "fyMonth"]))
                  thePI.first <- substr(PIData$description[i], 13, nchar(PIData$description[i]))
                  outData <- outData %>%
                    mutate(budget=case_when(
                      ((projectAccount == targetAccount) &
                         (fiscalYear == targetYear) &
                         (fyMonth == targetMonth) &
                         ((idcType == "DEPT")|(idcType=="Dept"))) ~ as.character(idcContribution),
                      TRUE ~ budget
                    )) %>%
                    mutate(facultyName = case_when(
                      ((projectAccount == targetAccount) &
                         (fiscalYear == targetYear) &
                         (fyMonth == targetMonth) &
                         ((idcType=="DEPT") | (idcType=="Dept"))) ~ thePI.first,
                      TRUE ~ facultyName
                    ))
                } else {
                  targetAccount <- as.vector(unlist(t.PIdata[i, "projectAccount"]))
                  targetYear <- as.vector(unlist(t.PIdata[i, "fiscalYear"]))
                  targetMonth <- as.vector(unlist(t.PIdata[i, "fyMonth"]))
                  thePI.next <- substr(PIData$description[i], 13, nchar(PIData$description[i]))
                  if(debugFlag) cat(green("Next co-PI", thePI.next, "not", thePI.first, "\n"))
                  outData <- outData %>%
                    rbind(t.deptData) %>%
                    mutate(budget=case_when(
                      ((projectAccount == targetAccount) &
                         (fiscalYear == targetYear) &
                         (fyMonth == targetMonth) &
                         ((idcType == "DEPT")|(idcType=="Dept"))) ~ as.character(idcContribution),
                      TRUE ~ budget
                    )) %>%
                    mutate(facultyName = case_when(
                      ((projectAccount == targetAccount) &
                         (fiscalYear == targetYear) &
                         (fyMonth == targetMonth) &
                         ((idcType=="DEPT") | (idcType=="Dept")) &
                         (facultyName != thePI.first)) ~ thePI.next,  #This works when the PI is in another department.  Need a better comparison.  
                      TRUE ~ facultyName
                    ))
                }
              }
              #cat(red("##########################################\n"))
            } else {
              if(debugFlag) cat(green("############### Unusual Case ################\n"))
              # Take care of the unusual case where the PI is Nielsen-Gammon for Brent McRoberts project.
              outData <- outData %>%
                mutate(facultyName = case_when(projectAccount == 41449100001 ~ "Mcroberts, Douglas",
                                               TRUE ~ facultyName))
            }
            if(debugFlag)cat(red("##################################\n"))
          }
          outData
        }
        loopThroughFiles <- function(t.files, inData=NULL){
          IDCData <- inData
          t.vector <- as.vector(unlist(geogPIs))
          # t.notGeographers <- as.character(unlist(IDCDataNew$facultyName)) %in% t.vector
          # t.new <- IDCDataNew[!t.notGeographers,]
          # t.unique <- unique(t.new)
          # View(t.new)
          for(i in 1:length(t.files)){
            #Date of file creation from fileName.
            fileFormat <- NULL
            cat("Reading:", t.files[i], "\n")
            IDCDataIncremental <- read_excel(path=paste0(local.archive.path, "IDC_Data//", t.files[i]), sheet="IDC detail",
                                             col_names=TRUE,
                                             col_types="text")
            if(t.files[i]=="2017 July_CLGE IDC Distributions with PI Dept_FINAL.xlsx") assign("t.1", IDCDataIncremental, pos=1)
            fieldNames <- names(IDCDataIncremental)
            if("Direct Expenditures" %in% fieldNames) 
              directExpendituresFieldName <- "Direct Expenditures"
            else if ("Monthly Direct Expenditures" %in% fieldNames)
              directExpendituresFieldName <- "Monthly Direct Expenditures"
            
            IDCDataIncremental <- IDCDataIncremental %>%
              mutate(`IDC Type` = case_when(
                (`IDC Type`=="Dept") ~ "DEPT",
                TRUE ~ `IDC Type`
              ))
            IDCDataIncremental <- IDCDataIncremental %>%
              filter(`PI Dept`=="GEOG") %>%
              filter((`IDC Type`=="DEPT") | (`IDC Type`=="PI")) %>%
              filter(`Offset SA Dept`=="GEOG") %>%
              select(fiscalYear=FY, fyMonth=`FM`, projectAccount=`Project Account`, idcMonth=`IDC Month`,
                     projectTitle=`Project Title`, sponsorName=`Sponsor Name`,
                     facultyName=`SL Responsible Person Name`,
                     description=`Offset SA Account Desc`,
                     pctAllocation=`IDC Allocation %`,
                     directExpenditures= directExpendituresFieldName, #`Direct Expenditures`,
                     monthlyIDC=`Monthly IDC`, budget=Budget, idcType=`IDC Type`) %>%
              mutate(fyMonth=as.numeric(gsub("_.*$", "",.$idcMonth)))
            
            
            #if(t.files[i]=="2017 July_CLGE IDC Distributions with PI Dept_FINAL.xlsx") assign("t.1a", IDCDataIncremental, pos=1)
            if(t.files[i]=="placeholder") {
              t.debug <- TRUE
            } else {
              t.debug <- FALSE
            }
            
            IDCDataIncremental <- processSplits(IDCDataIncremental, debugFlag=t.debug)
            if(t.debug) assign("IDCDataIncremental", IDCDataIncremental, pos=1)  #Debug purposes only
            # IDCDataIncremental <- IDCDataIncremental %>%
            #   mutate(facultyName = case_when(
            #     ((projectAccount == targetAccount) &
            #        (fiscalYear == targetYear) &
            #        (fyMonth == targetMonth) &
            #        (idcType=="DEPT")) ~ thePI,
            #     TRUE ~ facultyName
            #   ))
            IDCDataIncremental <- IDCDataIncremental %>%
              mutate(facultyName = case_when(
                (.$idcType=="PI") ~  substr(.$description, 13, nchar(.$description)),
                TRUE ~ facultyName
              ))
            
            if(!is.null(IDCData)){
              IDCData <- rbind(IDCData, IDCDataIncremental) 
            } else {
              IDCData <- IDCDataIncremental
            }
          }
          if(length(t.files) > 0) {
            saveRDS(IDCData, paste0(rds.path, "idc.rds"))
            cat(red("Wrote idc.rds\n"))
          }
          IDCData
        }
        
        
        #######################################################
        # Main Body                                           #
        #######################################################
        
        # set up geogPIs 
        geogPIs <- readRDS(paste0(rds.path, "faculty_pis.rds"))
        
        # check for existence of idc.rds
        if(file.exists(paste0(rds.path, "idc.rds"))){
          # read the IDC data from idc.rds
          IDCData <- readRDS(paste0(rds.path, "idc.rds"))
          # check to see if it is up-to-date
          lastUpdate <- file.mtime(paste0(rds.path, "idc.rds"))
          ldir <- paste0(local.archive.path, "IDC_Data")
          finf <- file.info(dir(path = ldir, full.names = TRUE), extra_cols = FALSE)
          finf <- cbind(finf, path=dir(path = ldir), stringsAsFactors=FALSE)
          newest.creation.date <- max(finf[,"mtime"])
          
          # select files with newest.creation.data > lastUpdate
          finf <- finf %>%
            filter(mtime > lastUpdate)
          fileList <- finf$path
          
          # append these files to the IDCData
          
          if(length(fileList > 0)){
            IDCData <- loopThroughFiles(fileList, inData=IDCData)
            IDCData <- IDCData %>%
              arrange(fiscalYear, fyMonth)
            
            t.months <- c("1_SEP", "2_OCT", "3_NOV", "4_DEC", "5_JAN", "6_FEB", "7_Mar", "8_APR", "9_MAY", "10_JUN", "11_JUL", "12_AUG")
            IDCData$idcMonth <- factor(IDCData$idcMonth, levels=t.months, ordered=TRUE)       
          }
        } else {
          t.files <- dir(paste0(local.archive.path, "IDC_Data"))
          IDCData <- loopThroughFiles(t.files, inData=NULL)
          IDCData <- IDCData %>%
            arrange(fiscalYear, fyMonth)
          
          t.months <- c("1_SEP", "2_OCT", "3_NOV", "4_DEC", "5_JAN", "6_FEB", "7_Mar", "8_APR", "9_MAY", "10_JUN", "11_JUL", "12_AUG")
          IDCData$idcMonth <- factor(IDCData$idcMonth, levels=t.months, ordered=TRUE)  
        }
        
        fileInventoryFilename <<- c(fileInventoryFilename, "idc.rds")
        fileInventoryDescription <<- c(fileInventoryDescription, "IDC Data.")
        fileInventoryLastUpdate <<- c(fileInventoryLastUpdate, format(newest.creation.date, "%d-%b-%Y"))
        IDCData
      }
      
      ##########################################
      # Student Flow Functions                 #
      ##########################################
      student.flow <- function(current, previous, grads, major="GIST"){
        
        inCurrent <- current
        path <- paste0(local.archive.path, "ActiveStudentData//")
        
        file <- paste("SRC_CS_POPSEL_GEOG_MAJR_CSV_", current, ".xls", sep="")
        if(!file.exists(paste0(path, file))) {
          file <- paste("SRC_CS_POPSEL_GEOG_MAJR_CSV_", current, ".csv", sep="")
          cat(yellow("Using the .csv version\n"))
        }
        cat(green("1about to read:", file, "\n"))
        assign("t.path", paste0(path, file), pos=1)
        current.students <- read.csv(paste(path, file, sep=""), sep="|")
        cat(green(paste("in student.flow", current), "\n"))
        #limit analysis to undergraduates
        current.students <- current.students %>%
          filter((CLASSFICATION=="U1")|(CLASSFICATION=="U2")|(CLASSFICATION=="U3")|(CLASSFICATION=="U4"))
        
        file <- paste0("SRC_CS_POPSEL_GEOG_MAJR_CSV_", previous, ".xls")
        if(!file.exists(paste0(path, file))) {
          file <- paste("SRC_CS_POPSEL_GEOG_MAJR_CSV_", current, ".csv", sep="")
          cat(yellow("Using the .csv version\n"))
        }
        #cat(red("about to read:", file, "\n"))
        prev.sem.students <- read.csv(paste(path, file, sep=""), sep="|")
        #cat(" read previous")
        
        prev.sem.students <- prev.sem.students %>%
          filter((CLASSFICATION=="U1")|(CLASSFICATION=="U2")|(CLASSFICATION=="U3")|(CLASSFICATION=="U4"))
        path <- paste0(local.archive.path, "GraduationData//")
        file <- paste("SRC_AL_FINAL_GRAD_AL_CSV_", grads, ".xls", sep="")
        if(!(file.exists(paste0(path, file)))){
          file <- paste0("SRC_AL_FINAL_GRAD_AL_CSV_", grads, ".csv", sep="")
        }
        cat(red("about to read:", file, "\n"))
        #cat(paste0(path, file))
        fileList <- list.files(paste0(local.archive.path, "GraduationData"))
        #t.index<- grep("19C", fileList)
        #file2Read <- fileList[t.index]
        file2Read <- file
        tryCatch(
          { #message("This is the try part")
            cat(cyan("1Trying to read:", file2Read, "\n"))
            assign("t1.out", paste0(local.archive.path, "GraduationData//", file2Read), pos=1)
            grads17A <- read.csv(paste0(local.archive.path, "GraduationData//", file2Read), sep="|")
          },
          error = function(cond){
            message("1Graduation data is not a text delimited flat file.  Reading it as an .xls file.")
            grads17A <<- readxl::read_excel(paste0(local.archive.path, "GraduationData//", file2Read))
          },
          finally = {
            #cat("here\n")
          }
        )
        #grads17A <- read.csv(paste(path, file, sep=""), sep="|")
        
        current.text <- newSemesterCodes %>%
          filter(current==inCurrent) %>%
          select(current.code) %>%
          unlist()
        cat(blue(paste0("Processed student flow data for active ", major, " students in ", current.text, " and graduates in ", grads,".\n")))
        #cat(" read grads")
        
        # beyond here, need current.students, prev.sem.students, and grads17A
        
        #############################################################
        #         Determine all GIST students in current semester   #
        #############################################################
        
        current.gis.students <- current.students[current.students$MAJOR1_1ST_DEG==major,]
        current.gis.students.1 <- current.students[current.students$MAJOR1_2ND_DEG==major,]
        current.gis.students <- rbind(current.gis.students, current.gis.students.1)
        t.g6 <- current.gis.students$CLASSFICATION == "G6"
        t.g7 <- current.gis.students$CLASSFICATION == "G7"
        t.g8 <- current.gis.students$CLASSFICATION == "G8"
        t.grads <- as.logical(t.g6+t.g7+t.g8)
        current.gis.students <- current.gis.students[!t.grads,]
        t.c <- current.gis.students
        # the following is a fix for NAs in the input file
        t.c <- t.c[!is.na(t.c[,1]),]
        current.gis.students <- current.gis.students[!is.na(current.gis.students[,1]),]
        #remove graduate students
        
        
        #############################################################
        #         Determine all GIST students in previous semester  #
        #############################################################
        #cat("major is:", major,"\n")
        prev.gis.students <- prev.sem.students[prev.sem.students$MAJOR1_1ST_DEG==major,]
        prev.gis.students.1 <- prev.sem.students[prev.sem.students$MAJOR1_2ND_DEG==major,]
        prev.gis.students <- rbind(prev.gis.students, prev.gis.students.1)
        t.p <- prev.gis.students
        #cat("dim(prev.gis.students):", dim(prev.gis.students), "\n")
        
        # the following is a fix for NAs in the input file
        t.p <- t.p[!is.na(t.p[,1]),]
        prev.gis.students <- prev.gis.students[!is.na(prev.gis.students[,1]),]
        #assign("prev.gis.students", prev.gis.students, pos=1)
        
        #############################################################
        #         Determine all GIST grads in previous semester  #
        #############################################################
        
        prev.grads <- grads17A[grads17A$MAJR==major,]
        t.g <- prev.grads
        
        #############################################################
        # Determine which students are new to the major (intake)    #
        #############################################################
        t.match <- match(prev.gis.students$TNUMBER, current.gis.students$TNUMBER)
        t.in <- current.gis.students$TNUMBER %in% prev.gis.students$TNUMBER
        #students with FALSE in t.in are NOT in previous.gis.students
        #these are students new to the major
        t.new <- current.gis.students[!t.in,]
        
        #############################################################
        # Determine which students are leaving the major (outflow)  #
        #############################################################
        t.in <- prev.gis.students$TNUMBER %in% current.gis.students$TNUMBER
        #students with FALSE in t.in have left the major (lost or grad)
        students.leaving <- prev.gis.students[!t.in,]
        #cat("dim(students.leaving):", dim(students.leaving), "\n")
        
        #############################################################
        # Determine which leaving students are grads                #
        #############################################################
        
        # all the grads should be in the leaving students list
        
        graduates <- students.leaving$TNUMBER %in% prev.grads$TNUM
        lost.students <- students.leaving[!graduates,]
        
        #tally up the important numbers for each semester
        # number of majors, new majors, graduates, lost students
        output <- c(dim(t.c)[1], dim(t.new)[1], dim(t.g)[1], dim(lost.students)[1])
        #cat(" leaving student.flow\n")
        output
      }
      
      createStudentFlowData <- function(inSemesterCodes, major="GIST"){
        
        t.df <- inSemesterCodes
        grad.semesters <- inSemesterCodes[,"grads"]
        cat(blue("In createStudentFlowData\n"))
        for(i in 1:dim(t.df)[1]){
          t.out <- student.flow(t.df[i,1], t.df[i,2], t.df[i,3], major=major) 
          #cat("after student.flow\n")
          #print(t.out)
          if(i==1){
            #create data.frame
            student.flow.data <- matrix(nrow=1, ncol=length(t.out)+1, data=0)
            
            student.flow.data[i,c(2:(length(t.out)+1))] <- t.out
            student.flow.data <- as.data.frame(student.flow.data)
            names(student.flow.data) <- c("semester", "majors", "new", "grads", "lost")
            #cat("after first matrix\n")
          } else{
            # student.flow.data[i,1] <- t.semester
            student.flow.data[i,c(2:(length(t.out)+1))] <- t.out
            #cat("after student.flow.data, i=", i, "\n")
          }
        }
        #print(grad.semesters)
        
        #cat("length(grad.semesters)", length(grad.semesters), "\n")
        #student.flow.data[,1] <- grad.semesters[2:(length(grad.semesters))]
        student.flow.data[,1] <- inSemesterCodes[,"current.code"]
        cat(blue("end of student.flow.data\n"))
        student.flow.data <- data.frame(student.flow.data, major=major)
        
        student.flow.data
      }  
      
      createStudentFlowDataNew <- function(inData){
        
        determineAvailableGraduationData <- function(){
          #local.path
          #local.archive.path
          graduationDataPath <- paste0(local.archive.path, "GraduationData")
          theFiles <- dir(graduationDataPath)
          theSemesters <- unlist(lapply(theFiles, substr, start=26, stop=28))
          #sessionValues$mostRecentGraduationDataSemester <- max(theSemesters)  #uncomment when finished
          assign("theSemesters", theSemesters, pos=1)
        }
        determineAvailableCurrentSemesterData <- function(){
          currentSemesterPath <- paste0(local.archive.path, "ActiveStudentData")
          theFiles <- dir(currentSemesterPath)
          theSemesters <- unlist(lapply(theFiles, substr, start=29, stop=34))
          assign("theSemestersCurrent", theSemesters, pos=1)
        }
        
        #inData contains the current semester data.  graduation semester will be
        #one semester prior to current semester.
        determineSemesterCodes <- function(inData){
          outData <- semester.codes %>%
            mutate(c.current=as.character(current)) %>%
            filter(c.current %in% inData) %>%
            select(current.code) %>%
            arrange(current.code) %>%
            unlist() %>%
            as.vector()
          outData
        }
        #Determine which grad.semesters are available should be done by reading 
        #the available graduate data files.
        if (showDebugCues==TRUE) cat(green("Before available data\n"))
        availableGraduationSemesters <- determineAvailableGraduationData()
        availableCurrentActiveStudentSemesters <- determineAvailableCurrentSemesterData()
        availableCurrentActiveStudentSemestersCodes <- determineSemesterCodes(availableCurrentActiveStudentSemesters)
        #print(availableCurrentActiveStudentSemestersCodes)
        if (showDebugCues==TRUE) cat(green("After available data\n"))
        
        #determine if any new data are available.  If new data are available, then
        #need to recrete the student.flow dataset
        #set which semesters to include before calling creatStudentFlowData
        useSemesterCodes <- newSemesterCodes %>%
          filter(grads %in% availableGraduationSemesters) %>%
          filter(current %in% theSemestersCurrent)
        #print(useSemesterCodes)
        
        #determine which semesters are in inData
        semestersAlreadyProcessed <- unique(inData$semester)
        t.setdiff <- setdiff(useSemesterCodes$current.code, semestersAlreadyProcessed)
        if(length(t.setdiff)!=0) {
          cat("Already Processed:", semestersAlreadyProcessed, "\n")
          cat("inData Semesters :", useSemesterCodes$current.code, "\n")
          cat("setdiff:", t.setdiff, "\n")
          gist <- createStudentFlowData(useSemesterCodes, major="GIST")
          geog <- createStudentFlowData(useSemesterCodes, major="GEOG")
          usge <- createStudentFlowData(useSemesterCodes, major="USGE")
          t.out <- rbind(geog, gist, usge)
        } else {
          t.out <- inData
        }
        saveRDS(t.out, paste0(rds.path, "student.flow.all.rds"))
        t.out
      }
      
      ##########################################
      # Active Student Data  Functions         #
      ##########################################
      readActiveStudentData <- function(force.create=FALSE){
        determineAvailableCurrentSemesterData <- function(){
          currentSemesterPath <- paste0(local.archive.path, "ActiveStudentData")
          theFiles <- dir(currentSemesterPath)
          theSemesters <- unlist(lapply(theFiles, substr, start=29, stop=34))
          theSemesters
        }
        determineSemesterCodes <- function(inData){
          outData <- semester.codes %>%
            mutate(c.current=as.character(current)) %>%
            filter(c.current %in% inData) %>%
            select(current.code) %>%
            arrange(current.code) %>%
            unlist() %>%
            as.vector()
          outData
        }
        availableCurrentActiveStudentSemesters <- determineAvailableCurrentSemesterData()
        availableCurrentActiveStudentSemestersCodes <- determineSemesterCodes(availableCurrentActiveStudentSemesters)
        readAndAppendActiveStudentData <- function(existingData=NULL, currentSemester){
          #######################################
          # required global values              #
          #      local.arhcive.path             #
          #######################################
          path <- paste0(local.archive.path, "ActiveStudentData//")
          file <- paste("SRC_CS_POPSEL_GEOG_MAJR_CSV_", currentSemester, ".xls", sep="")
          if(!file.exists(paste0(path, file))) {
            file <- paste("SRC_CS_POPSEL_GEOG_MAJR_CSV_", currentSemester, ".csv", sep="")
            cat(yellow("1Using the .csv version\n"))
          }
          cat("Reading", file, "\n")
          currentStudents <- read.csv(paste(path, file, sep=""), sep="|")
          has2ndMajor <- currentStudents %>%
            filter(!is.na(MAJOR2_1ST_DEG))
          if(dim(has2ndMajor)[[1]]>0)
            print(has2ndMajor)
          # Not sure that this routine handles double majors appropriately.
          #
          currentStudents <- currentStudents %>%
            filter((CLASSFICATION=="U1")|(CLASSFICATION=="U2")|(CLASSFICATION=="U3")|(CLASSFICATION=="U4")) %>%
            mutate(degree = case_when(
              MAJOR1_1ST_DEG == "GEOG" ~ "Geography",
              MAJOR1_1ST_DEG == "GIST" ~ "GIST",
              MAJOR2_1ST_DEG == "GEOG" ~ "Geography",
              MAJOR2_1ST_DEG == "GIST" ~ "GIST",
              TRUE ~ "1")) %>%
            filter(degree != "1") %>%
            mutate(semester = currentSemester) %>%
            mutate(major = PROGRAM1_1ST_DEG) %>% 
            select(major, semester, degree) %>%
            mutate(major=as.character(major))
          if(is.null(existingData))
            t.out <- as.data.frame(currentStudents)
          else {
            t.out <- rbind(existingData, currentStudents)
          }
          t.out
        }
        
        activeStudents <- NULL
        semesterList <- availableCurrentActiveStudentSemesters
        maxAvailableSemester <- max(semesterList)
        # determine latest semester in the .rds file
        if(file.exists(paste0(rds.path, "active.students.rds")) & (force.create != TRUE)){
          activeStudents <- readRDS(paste0(rds.path, "active.students.rds"))
          maxSemester <- max(activeStudents$semester)
        } else {
          maxSemester <- NULL
        }
        #cat("maxSemester:", maxSemester, "maxAvailableSemester:", maxAvailableSemester, "\n")
        needMore <- FALSE
        if(!is.null(maxSemester)) {
          if(maxSemester < maxAvailableSemester)
            needMore <- TRUE
          else needMore <- FALSE
        }
        if(is.null(maxSemester) | (force.create==TRUE) | (needMore==TRUE)){
          for(i in 1:length(semesterList)){
            #cat(blue("Processing", semesterList[i], "\n"))
            #cat(blue(dim(activeStudents)))
            #cat("\n")
            activeStudents <- readAndAppendActiveStudentData(existingData=activeStudents, currentSemester=semesterList[i])
          }
          saveRDS(activeStudents, paste0(rds.path, "active.students.rds"))
          cat("Wrote", paste0(rds.path, "active.students.rds\n"))
        }
        activeStudents
      }
      
      readActiveGraduateStudentData <- function(force.create=FALSE){
        determineAvailableCurrentSemesterData <- function(){
          currentSemesterPath <- paste0(local.archive.path, "ActiveStudentData")
          theFiles <- dir(currentSemesterPath)
          theSemesters <- unlist(lapply(theFiles, substr, start=29, stop=34))
          
          theSemesters
        }
        determineSemesterCodes <- function(inData){
          outData <- semester.codes %>%
            mutate(c.current=as.character(current)) %>%
            filter(c.current %in% inData) %>%
            select(current.code) %>%
            arrange(current.code) %>%
            unlist() %>%
            as.vector()
          outData
        }
        availableCurrentActiveStudentSemesters <- determineAvailableCurrentSemesterData()
        availableCurrentActiveStudentSemestersCodes <- determineSemesterCodes(availableCurrentActiveStudentSemesters)
        readAndAppendActiveStudentData <- function(existingData=NULL, currentSemester){
          #######################################
          # required global values              #
          #      local.arhcive.path             #
          #######################################
          path <- paste0(local.archive.path, "ActiveStudentData//")
          file <- paste("SRC_CS_POPSEL_GEOG_MAJR_CSV_", currentSemester, ".xls", sep="")
          if(!file.exists(paste0(path, file))) {
            file <- paste("SRC_CS_POPSEL_GEOG_MAJR_CSV_", currentSemester, ".csv", sep="")
            cat(yellow("1Using the .csv version\n"))
          }
          cat("Reading", file, "\n")
          currentStudents <- read.csv(paste(path, file, sep=""), sep="|")
          has2ndMajor <- currentStudents %>%
            filter(!is.na(MAJOR2_1ST_DEG))
          if(dim(has2ndMajor)[[1]]>0)
            print(has2ndMajor)
          # Not sure that this routine handles double majors appropriately.
          #
          currentStudents <- currentStudents %>%
            filter((CLASSFICATION=="G6")|(CLASSFICATION=="G7")|(CLASSFICATION=="G8")) %>%
            mutate(semester.numeric=currentSemester) %>%
            mutate(semester.numeric=as.character(semester.numeric)) %>%
            left_join(semester.codes, by=c("semester.numeric"="current")) %>%
            mutate(semester.abbrv=current.code) %>%
            filter((MAJOR1_1ST_DEG == "GEOG") | (MAJOR1_1ST_DEG == "GEOS")) %>%
            select(NAMES, UIN, CLASSFICATION, PROGRAM1_1ST_DEG, semester.abbrv, semester.numeric) 
          if(is.null(existingData))
            t.out <- as.data.frame(currentStudents)
          else {
            t.out <- rbind(existingData, currentStudents)
          }
          t.out
        }
        
        
        
        activeStudents <- NULL
        semesterList <- availableCurrentActiveStudentSemesters
        maxAvailableSemester <- max(semesterList)  #this is from the file directory
        # determine latest semester in the .rds file
        if(file.exists(paste0(rds.path, "active.graduate.students.rds")) & (force.create != TRUE)){
          activeStudents <- readRDS(paste0(rds.path, "active.graduate.students.rds"))
          maxSemester <- max(as.character(activeStudents$semester.numeric))
        } else {
          maxSemester <- NULL
        }
        #cat("maxSemester:", maxSemester, "maxAvailableSemester:", maxAvailableSemester, "\n")
        needMore <- FALSE
        if(!is.null(maxSemester)) {
          if(maxSemester < maxAvailableSemester)
            needMore <- TRUE
          else needMore <- FALSE
        }
        #cat("maxSemester:", maxSemester, "force.create:", force.create, "needMore:", needMore, "\n")
        if(is.null(maxSemester) | (force.create==TRUE) | (needMore==TRUE)){
          for(i in 1:length(semesterList)){
            #cat(blue("Processing", semesterList[i], "\n"))
            #cat(blue(dim(activeStudents)))
            #cat("\n")
            activeStudents <- readAndAppendActiveStudentData(existingData=activeStudents, currentSemester=semesterList[i])
          }
          saveRDS(activeStudents, paste0(rds.path, "active.graduate.students.rds"))
          cat("Wrote", paste0(rds.path, "active.graduate.students.rds\n"))
        }
        activeStudents
      }
      
      readMinorsData <- function(force.create=FALSE){
        `%out%` <- function(a,b) ! a %in% b
        
        determineAvailableCurrentSemesterData <- function(inMinor="GEOG"){
          currentSemesterPath <- paste0(local.archive.path, "Minors//", inMinor,"_Minors//")
          theFiles <- dir(currentSemesterPath)
          if(inMinor=="GEOG") {
            start.pos=29
            end.pos=34
          }
          if(inMinor=="GIST") {
            start.pos=23
            end.pos=28
          }
          theSemesters <- unlist(lapply(theFiles, substr, start=start.pos, stop=end.pos))
          theSemesters
        }
        determineSemesterCodes <- function(inData){
          outData <- semester.codes %>%
            mutate(c.current=as.character(current)) %>%
            filter(c.current %in% inData) %>%
            select(current.code) %>%
            arrange(current.code) %>%
            unlist() %>%
            as.vector()
          outData
        }
        available.minors <- c("GEOG", "GIST")
        readSingleMinorsFile <- function(inSemester, inMinor){
          #cat("In readMinorFile\n")
          GEOGStem <- "SRC_CS_POPSEL_GEOG_MINR_CSV_"
          GISTStem <- "SRC_CS_GIST_MINOR_CSV_"
          #path <- paste0(local.archive.path, "ActiveStudentData//")
          path <- paste0(local.archive.path, "Minors//",inMinor,"_Minors//")
          if(inMinor=="GEOG") 
            file <- paste0(GEOGStem, inSemester, ".xls")
          else 
            file <- paste0(GISTStem, inSemester, ".xls")
          
          if(!file.exists(paste0(path, file))){
            if(inMinor=="GEOG")
              file <- paste0(GEOGStem, inSemester, ".csv")
            else
              file <- paste0(GISTStem, inSemester, ".csv")
          }
          #cat("Reading", paste0(path,file, "\n"))
          if(inMinor=="GEOG")
            sepChar <- "|"
          else
            sepChar <- ","
          tryCatch(
            { #message("This is the try part")
              currentMinors <- read.csv(paste0(path, file), sep=sepChar)
            },
            error = function(cond){
              grads17A <<- readxl::read_excel(paste0(path, file))
            },
            finally = {
              #cat("here\n")
            }
          )
          # if(inMinor == "GEOG"){
          #   currentMinors <- read.csv(paste0(path, file), sep="|")      
          # } else {
          #   currentMinors <- read_excel(paste0(path, file))
          # }
          
          
        }
        
        if(file.exists(paste0(rds.path, "all.minors.rds")) & (force.create != TRUE)){
          existingMinorsData <- readRDS(paste0(rds.path, "all.minors.rds"))
          if (showDebugCues==TRUE) cat(red("RDS exists\n"))
          semesterListGEOG <- determineAvailableCurrentSemesterData(inMinor="GEOG")
          semesterListGIST <- determineAvailableCurrentSemesterData(inMinor="GIST")
          if (showDebugCues==TRUE) cat("semesterListGIST:", semesterListGIST, "\n")
          if (showDebugCues==TRUE) cat("semesterListGEOG:", semesterListGEOG, "\n")
          # check if the existingMinorsData are up-to-date
          uniqueGEOGSemestersInRDS <- existingMinorsData %>%
            filter(minor=="GEOG") %>%
            left_join(semester.codes, by=c("semester"="current.code")) %>%
            select("current") %>%
            unlist() %>%
            as.vector() %>%
            unique()
          uniqueGISTSemestersInRDS <- existingMinorsData %>%
            filter(minor=="GIST") %>%
            left_join(semester.codes, by=c("semester"="current.code")) %>%
            select("current") %>%
            unlist() %>%
            as.vector() %>%
            unique()
          if (showDebugCues==TRUE) cat("uniqueGISTSemestersInRDS:", uniqueGISTSemestersInRDS, "\n")
          if (showDebugCues==TRUE) cat("%out% :", semesterListGIST %out% uniqueGISTSemestersInRDS, "\n")
          #missingAvailableGEOGData <- uniqueGEOGSemestersInRDS[uniqueGEOGSemestersInRDS %out% semesterListGEOG]
          #missingAvailableGISTData <- uniqueGISTSemestersInRDS[uniqueGISTSemestersInRDS %out% semesterListGIST]
          missingAvailableGEOGData <- semesterListGEOG[semesterListGEOG %out% uniqueGEOGSemestersInRDS]
          missingAvailableGISTData <- semesterListGIST[semesterListGIST %out% uniqueGISTSemestersInRDS]
          if (showDebugCues==TRUE) cat("missingAvailableGEOGData", missingAvailableGEOGData, "\n")
          if (showDebugCues==TRUE) cat("missingavailableGISTData", missingAvailableGISTData, "\n")
          
        } else if (force.create==TRUE){
          existingMinorsData <- NULL
          missingAvailableGISTData <- determineAvailableCurrentSemesterData(inMinor="GIST")
          missingAvailableGEOGData <- determineAvailableCurrentSemesterData(inMinor="GEOG")
        } else {
          existingMinorsData <- NULL
          missingAvailableGISTData <- determineAvailableCurrentSemesterData(inMinor="GIST")
          missingAvailableGEOGData <- determineAvailableCurrentSemesterData(inMinor="GEOG")
        }
        
        UpdateFlag <- TRUE
        if((length(missingAvailableGEOGData)==0) & (length(missingAvailableGISTData)==0)){
          #cat("Minors data are up-to-date\n")
          UpdateFlag <- FALSE
        } else {
          for(i in 1:length(available.minors)){
            targetMinor <- available.minors[i]
            if(targetMinor=="GEOG"){
              semesterList <- missingAvailableGEOGData
            } else {
              semesterList <- missingAvailableGISTData
            }
            cat("These semesters have been downloaded, but are not yet in the database for", targetMinor, "minors:", semesterList, "\n")
            
            #semesterList <- determineAvailableCurrentSemesterData(inMinor=targetMinor)
            if(length(semesterList) > 0){
              for(j in 1:length(semesterList)){
                cat(blue("Reading", semesterList[j], "for", targetMinor, "\n"))
                t.out <- readSingleMinorsFile(semesterList[j], targetMinor)
                
                if(targetMinor=="GEOG"){
                  t.out <- t.out %>%
                    filter((MINOR1_1ST_DEG=="GEOG") |
                             (MINOR2_1ST_DEG=="GEOG") |
                             (MINOR1_1ST_DEG=="GEOI") |
                             (MINOR2_1ST_DEG=="GEOI")) %>%
                    mutate(minor = case_when(
                      MINOR1_1ST_DEG == "GEOG" ~ "GEOG",
                      MINOR1_1ST_DEG == "GEOI" ~ "GEOI",
                      MINOR2_1ST_DEG == "GEOG" ~ "GEOG",
                      MINOR2_1ST_DEG == "GEOI" ~ "GEOI",
                      TRUE ~ "1")) %>%
                    filter(minor != "1") %>%
                    select("classification" = "CLASSFICATION", "major"= "MAJOR1_1ST_DEG", "minor") %>%
                    mutate(semester = determineSemesterCodes(semesterList[j]))
                } 
                if(targetMinor=="GIST"){
                  t.out <- t.out %>%
                    select("classification" = "STUDENT_CLASS", "major"="MAJOR1", "minor"="MINOR") %>%
                    mutate(semester = determineSemesterCodes(semesterList[j])) %>%
                    mutate(minor = "GIST")
                }
                if(!is.null(existingMinorsData)){
                  existingMinorsData <- rbind(existingMinorsData, t.out)
                } else {
                  existingMinorsData <- t.out
                }
              }
            }
          }
        }
        
        if(UpdateFlag==TRUE){
          saveRDS(existingMinorsData, paste0(rds.path, "all.minors.rds"))
        }
        existingMinorsData
        
      }
      
      ##########################################
      # Annual Productivity Data Functions     #
      ##########################################
      
      readAnnualProductivityData <- function(force.create=FALSE){
        # Reads in data produced by Faculty 180 for Annual Activities Reporting
        #
        
        if(file.exists(paste0(rds.path, "annualActivities.rds")) & (force.create==FALSE)){
          cat("Annual activities rds file exists")
        } else {
          # need to create the rds file
          # determine which years are available in the folder
          activitiesFolder <- paste0(local.archive.path, "AnnualReviewData//")
          committeesFolder <- paste0(local.path, "ShinyApps//GEOG_Committees//Data//")
          availableYears <- dir(activitiesFolder)
          #outList <- NULL
          
          for(i in 1:length(availableYears)){
            yearOfInterest <- availableYears[i]
            pathOfInterest <- paste0(activitiesFolder, yearOfInterest)
            pathOfInterest1 <- paste0(activitiesFolder, yearOfInterest, "a")
            #print(dir(pathOfInterest))
            journalPublications <- read.csv(paste0(pathOfInterest, "//scholarly-contributions-creative-productions-and-patents-journal-article_Export.csv"), stringsAsFactors=FALSE)
            assign("t.journal", journalPublications, pos=1)
            assign("t.yoi", yearOfInterest, pos=1)
            journalPublications <- journalPublications %>%
              filter(Year.1==2020)
            facultyListing <- read.csv(paste0(pathOfInterest, "//faculty-listing_Export.csv"), stringsAsFactors=FALSE)
            assign("t.facultyListing", facultyListing, pos=1)
            awardsAndHonors <- read.csv(paste0(pathOfInterest, "//awards-and-honors_Export.csv"), stringsAsFactors = FALSE) %>%
              filter(Year.Conferred==2020)
            
            assign("t.awards", awardsAndHonors, pos=1)
            grantsAndContracts <- read.csv(paste0(pathOfInterest, "//grants-contracts_Export.csv"), stringsAsFactors=FALSE) %>%
              filter((Year==2020) | (Status=="Funded - In Progress"))
            conferenceProceedings <- read.csv(paste0(pathOfInterest, "//scholarly-contributions-creative-productions-and-patents-conference-proceedings_Export.csv"), stringsAsFactors=FALSE) %>%
              filter(Year.1==2020)
            bookChapters <- read.csv(paste0(pathOfInterest, "//scholarly-contributions-creative-productions-and-patents-chapter_Export.csv"), stringsAsFactors=FALSE) %>%
              filter(Year.1==2020)
            presentations <- read.csv(paste0(pathOfInterest, "//scholarly-contributions-creative-productions-and-patents-presentation_Export.csv"), stringsAsFactors=FALSE) %>%
              filter(Year.1==2020)
            teaching <- read.csv(paste0(pathOfInterest, "//teaching_Export.csv"), stringsAsFactors=FALSE) %>%
              filter(Year==2020)
            faculty <- read.csv(paste0(pathOfInterest, "//faculty-listing_Export.csv"), stringsAsFactors=FALSE)
            serviceCommitteesF180 <- read.csv(paste0(pathOfInterest, "//institutional-committees_Export.csv"), stringsAsFactors=FALSE) %>%
              filter(Starting.Year <= 2020)
            serviceCommitteesShiny <- read_excel(paste0(committeesFolder, "CommitteeService.xlsx"))
            professionalService <- read.csv(paste0(pathOfInterest, "//service_Export.csv"), stringsAsFactors=FALSE) %>%
              filter(Starting.Year <= 2020)
            assign("t.faculty", faculty, pos=1)
            newDF <- faculty %>%
              select("Faculty.ID", "First.Name", "Last.Name", "Email") %>%
              unique() %>%
              mutate(firstInitial=substring(First.Name,1,1)) %>%
              mutate(templateName=case_when((Last.Name == "Guneralp") ~ paste0(Last.Name, ", ", firstInitial, "."),
                                            (Last.Name == "Minguez") ~ "Loisel",
                                            TRUE ~ Last.Name))
            #save all elements for a single year into a list named by the year (e.g. 2020)
            t.list <- list(journalPublications=journalPublications,
                           awardsAndHonors=awardsAndHonors,
                           grantsAndContracts=grantsAndContracts,
                           professionalService=professionalService,
                           conferenceProceedings=conferenceProceedings,
                           bookChapters=bookChapters,
                           presentations=presentations,
                           teaching=teaching,
                           faculty=newDF,
                           committeeServiceF180=serviceCommitteesF180,
                           committeeServiceShiny=serviceCommitteesShiny)
            if(i<=1){
              annualActivityList <- list("2020"=t.list)
            } else annualActivityList[[as.name(as.character(yearOfInterest))]] <- t.list
            
            assign("annualActivityList", annualActivityList, pos=1)
          }
        }
      }
      
      ############################################
      # Graduate Committee Membership  Functions #
      ############################################
      createGraduateCommitteesHistorical <- function(rds.data.path, local.archive.path, 
                                                     local.archive.subdirectory, force.create){
        readCommitteeReport <- function(year=2016, semester="A", path, report.stem){
          
          cat("Graduate Committee Processing:", paste0(report.stem, year, semester, ".csv"), "\n")
          t.1 <- read.table(paste0(path, report.stem, year, semester, ".csv"), sep=",", header=TRUE)
          if (showDebugCues==TRUE) assign("t.1", t.1, pos=1)
          # Filter for GEOG faculty
          # Some GEOG faculty are classified as CLGE due to ad-loc
          #
          
          t.geog1 <- t.1 %>%
            filter((FACULTYDEPT == "GEOG") | 
                     (FACULTYNAME == "Houser, Christopher") |
                     (FACULTYNAME == "Bednarz, Sarah W.") |
                     (FACULTYNAME == "Brannstrom, Christian") |
                     (grepl("Roark, Erin", FACULTYNAME)) |
                     (FACULTYNAME == "Tchakerian, Vatche ")) %>%
            filter(!grepl("Gold Bouchot", FACULTYNAME)) %>%
            mutate(Year= year) %>%
            mutate(Semester=paste0(year, semester))
          
          t.geog1
        }
        determineIfUpdateNeeded <- function(rds.path, rds.filename, local.archive.path, 
                                            local.archive.subdirectory, in.pattern){
          path <- paste0(local.archive.path, local.archive.subdirectory)
          t.wd <- getwd()
          setwd(path)
          t.fileinfo <- file.info(dir( pattern=in.pattern))
          setwd(t.wd)
          newest.creation.date <- max(t.fileinfo[,"mtime"])
          rdsInfo <- file.info(paste0(rds.path, rds.filename))
          
          if(file.exists(paste0(rds.data.path, rds.filename))) {
            
            rdsInfo <- file.info(paste0(rds.data.path, rds.filename))
            if(is.na(file.info(paste0(rds.path, rds.filename))$mtime)==TRUE)
              force.create <- TRUE
            else
              force.create <- FALSE
            if(newest.creation.date > rdsInfo$mtime)
              force.create <- TRUE
          } else {
            force.create <- TRUE
          }
          
          force.create
        }  
        path <- paste0(local.archive.path, local.archive.subdirectory)
        report.stem <- "SRC_AL_GRAD_COMM_GE_CSV_"
        
        needUpdate <- determineIfUpdateNeeded(rds.data.path, "gradcommittees.rds", local.archive.path,
                                              local.archive.subdirectory, in.pattern="*.csv")
        if (showDebugCues==TRUE) cat(green("Before needUpdate==TRUE\n"))
        #force.create <- TRUE
        if((needUpdate==TRUE)|(force.create==TRUE)){
          cat("Updating historical graduate committee membership data.\n")
          all.files <- dir(paste0(local.archive.path, "GraduateCommitteeData"), pattern="*.csv")
          for(i in 1:length(all.files)){
            in.year <- as.numeric(substr(all.files[i], regexpr("\\.", all.files[i])-5, regexpr("\\.",all.files[i])-2))
            in.semester <- substr(all.files[i], regexpr("\\.", all.files[i])-1, regexpr("\\.",all.files[i])-1)
            if(i == 1){
              fullData <- readCommitteeReport(in.year, in.semester, path, report.stem)
            } else {
              fullData <- bind_rows(fullData, readCommitteeReport(in.year, in.semester, path, report.stem))
            }
          }
          saveRDS(fullData, paste0(rds.path, "gradcommittees.rds"))
        } else {
          #1 cat("Data are up to date\n")
          fullData <- readRDS(paste0(rds.path, "gradcommittees.rds"))
        }
        # 
        fullData
      }
      

      
      makeCompleteGraduateListing <- function(force.create=FALSE, inDept="GEOG"){
        
        ########################################
        # No need for separate directories for #
        # different departments because data   #
        # files are entire university.         #
        ########################################
        
        needsUpdating <- FALSE
        path <- paste0(local.archive.path, "GraduationData//")
        t.combined <- NULL
        maxSemesterGraduated <- NULL
        maxSemesterGraduatedInRDS <- NULL
        t.dir <- dir(path)
        sem.codes <- substring(t.dir, nchar(t.dir)-6, nchar(t.dir)-4)
        maxSemesterInFiles <- max(sem.codes)
        
        if(inDept=="GEOG"){
          rdsFilename <- "all.graduated.students.rds"
        } else {
          rdsFilename <- paste0("all.graduated.students.", inDept, ".rds")
        }
        if(file.exists(paste0(rds.path, rdsFilename))){
          t.combined <- readRDS(paste0(rds.path, rdsFilename))
          assign("t.combined", t.combined, pos=1)
          maxSemesterGraduatedInRDS <- t.combined %>%
            mutate(two.digit=as.character(calendar.year)) %>%
            mutate(two.digit=substring(two.digit, 3,4)) %>%
            mutate(two.digit.code = paste0(two.digit, semester)) %>%
            select("two.digit.code") %>%
            unlist() %>%
            as.vector() %>%
            max()
        }
        if(is.null(maxSemesterGraduatedInRDS)) {
          needsUpdating <- TRUE
        } else if(maxSemesterInFiles > maxSemesterGraduatedInRDS){
          needsUpdating <- TRUE
        }
        mostRecentSemesterInRDS <- maxSemesterGraduated
        target.semester <- PreviousSemester(CurrentSemester(numeric=TRUE), semester.codes, numeric=FALSE)
        
        sem.codes <- substring(t.dir, nchar(t.dir)-6, nchar(t.dir)-4)
        #force.create=FALSE
        if(inDept=="GEOG"){
          keepMajors <- c("GEOG", "GIST", "SPSG", "USGE", "GEOS")
        }
        if(inDept=="OCNG"){
          keepMajors <- c("OCNG", "OCST")
        }
        if(inDept=="ATMO"){
          keepMajors <- c("METR", "ATMO")
        }
        if(inDept=="GEPL"){
          keepMajors <- c("GEOP", "GEOL")
        }
        if(inDept=="ENVP"){
          keepMajors <- c("ENGS", "ENST")
        }
        
        if((needsUpdating == TRUE) | (force.create==TRUE)){
          if (showDebugCues==TRUE) cat("Here I am!!!!!!!!!!!!!!!!!!!!!\n")
          t.combined <- NULL
          for(i in 1:length(t.dir)){
            #################################
            # Rewrite the if statement as a #
            # try - catch structure to      #
            # eliminate the i>30 hack       #
            #################################
            # tryCatch(
            #   { #message("This is the try part")
            #     currentMinors <- read.csv(paste0(path, file), sep="|")
            #   },
            #   error = function(cond){
            #     grads17A <<- readxl::read_excel(paste0(path, file))
            #   },
            #   finally = {
            #     #cat("here\n")
            #   }
            # )
            if(i>30){
              tryCatch(
                {
                  assign("t.out1", paste0(path, t.dir[i]), pos=1)
                  grads <- read_excel(paste0(path, t.dir[i]))
                  grads$GRAD_DATE <- as.character(grads$GRAD_DATE, "%d-%b-%Y")
                },
                error = function(cond){
                  cat("i=", i, t.dir[i],"\n")
                  grads <- read.table(paste0(path, t.dir[i]), header=TRUE, sep="|")
                }
              )
            } else {
              grads <- read.table(paste0(path, t.dir[i]), header=TRUE, sep="|")
            }
            
            if(i < 3){
              grads <- grads %>%
                #filter(COLLEGE=="GE") %>%
                #filter((MAJOR=="GEOG") | (MAJOR=="GIST") | (MAJOR=="SPSG") | (MAJOR=="USGE") | (MAJOR=="GEOS")) %>%
                filter(MAJOR %in% keepMajors) %>%
                select("GRAD_DATE", "NAME", "DEGREE", "MAJOR", "UIN") 
            }
            if(i == 3){
              names(t.combined) <- c("GRAD_DATE", "STUDENT", "DEGREE", "MAJR", "UIN")
              grads <- grads %>%
                #filter(COLLEGE == "GE") %>%
                #filter((MAJR == "GEOG") | (MAJR == "GIST") | (MAJR=="SPSG") | (MAJR=="USGE") | (MAJR=="GEOS")) %>%
                filter(MAJR %in% keepMajors) %>%
                select("GRAD_DATE", "NAME", "DEGREE", "MAJR", "UIN") %>%
                rename(c("STUDENT"="NAME")) 
            }
            if(i > 3){
              if (showDebugCues==TRUE) cat(blue("i>3\n"))
              names(t.combined) <- c("GRAD_DATE", "STUDENT", "DEGC", "MAJR", "UIN")
              grads <- grads %>%
                #filter(COLL == "GE") %>%
                #filter((MAJR == "GEOG") | (MAJR == "GIST") | (MAJR=="SPSG") | (MAJR=="USGE") | (MAJR=="GEOS")) %>%
                filter(MAJR %in% keepMajors) %>%
                select("GRAD_DATE", "STUDENT", "DEGC", "MAJR", "UIN")
            }
            cat("Processed", t.dir[i], "\n")
            if (showDebugCues==TRUE) cat("names(t.combined:", names(t.combined), "\n")
            if (showDebugCues==TRUE) cat("names(grads):", names(grads), "\n")
            if(i>1){
              t.combined <- rbind(t.combined, grads)
            } else {
              t.combined <- grads
            }
            
          }
          # add in the calendar year and fiscal year fields
          t.year <- unlist(lapply(as.character(t.combined$GRAD_DATE), substr, 8,11))
          t.dec <- grep("Dec", as.character(t.combined$GRAD_DATE))
          t.may <- grep("May", as.character(t.combined$GRAD_DATE))
          t.aug <- grep("Aug", as.character(t.combined$GRAD_DATE))
          t.semester <- as.numeric(t.year)
          t.semester[t.dec] <- "C"
          t.semester[t.may] <- "A"
          t.semester[t.aug] <- "B"
          t.year <- as.numeric(t.year)
          t.fiscal <- as.numeric(t.year)
          t.fiscal[t.dec] <- t.fiscal[t.dec]+1
          t.combined <- data.frame(t.combined, calendar.year=t.year, fiscal.year=t.fiscal, semester=t.semester)
          t.combined <- t.combined %>%
            droplevels("DEGC") %>%
            droplevels("MAJR")
          t.combined <- t.combined %>%
            mutate(UIN=as.character(UIN))
          t.combined
        }
        if((needsUpdating==TRUE)|(force.create==TRUE)){
          #save the RDS file
          #saveRDS(t.combined, paste0(rds.path, "all.graduated.students.rds"))
        }
        t.combined
      }  
      

      ############################################
      # Teaching Evlaluation  Functions          #
      ############################################
      readDepartmentSummaryFile <- function(dataPath, fileName){
        t.data <- read_excel(paste0(dataPath, fileName))
        t.data
      }
      
      readTeachingEvaluationData <- function(){
        needsUpdate <- TRUE
        `%out%` <- function(a,b) ! a %in% b
        if(file.exists(paste0(rds.path, "all.teaching.evaluations.rds"))){
          combinedData <- readRDS(paste0(rds.path, "all.teaching.evaluations.rds"))
          semestersInRDS <- unique(as.character(combinedData$Term))
          # which semesters are in the local archive directory
          theFiles <- dir(paste0(local.archive.path, "TeachingEvaluationData"))
          availableSemesters <- substr(theFiles, 1,5)
          missingAvailableTeachingEvaluationData <- availableSemesters[availableSemesters %out% semestersInRDS]
          if(length(missingAvailableTeachingEvaluationData)==0)
            needsUpdate <- FALSE
        } 
        if(needsUpdate==TRUE) {
          dataPath <- paste0(local.archive.path, "TeachingEvaluationData//")
          allFiles <- dir(dataPath)
          target <- "DeptStatisticsReport"
          t.keep <- unlist(lapply(target, grepl, allFiles, fixed=TRUE))
          allFiles <- allFiles[t.keep]
          for(i in 1:length(allFiles)){
            if(i ==1){
              combinedData <- readDepartmentSummaryFile(dataPath, allFiles[i])
            } else {
              #cat("reading", allFiles[i], "\n")
              t.temp <- readDepartmentSummaryFile(dataPath, allFiles[i])
              combinedData <- rbind(combinedData, t.temp)
            }
          }
          saveRDS(combinedData, paste0(rds.path, "all.teaching.evaluations.rds"))
        }
        combinedData
      }
      
      ############################################
      # Course Demographics Functions            #
      ############################################
      
      read.student.count <- function(path, filename){
        
        read.counts <- function(data){
          # intake one string with format "1-AGEC,3-GEOG," and convert
          # into a vector of majors AGEC, GEOG, GEOG, GEOG
          # that can be used to generate counts and further processed.
          #cat("String =", data, "\n")
          
          # There should be no more than 1 trailing , at the end of the string.
          # remove all but 1 trailing comma when there are more than 1
          i <- nchar(data)
          t.substr <- substr(data,i,i)
          while(t.substr == ","){
            i <- i-1
            t.substr <- substr(data, i,i)
          }
          data <- substr(data, 1, i+1)
          data1 <- paste(",",data, sep="")
          expanded.major <- c(NULL)
          length.of.string <- nchar(data1)
          locations.of.commas <- as.data.frame(str_locate_all(pattern =",", data1))
          locations.of.hyphens <- as.data.frame(str_locate_all(pattern="-", data1))
          num.majors <- dim(locations.of.commas)[1]
          major <- c(NULL)
          count <- c(NULL)
          for(i in 1:(num.majors-1)){
            major <- c(major, substr(data1,locations.of.commas[i+1,1]-4,locations.of.commas[i+1,1]-1))
            count <- c(count, substr(data1,locations.of.commas[i,1]+1,locations.of.hyphens[i,1]-1))
          }
          count <- as.numeric(count)
          # change the output to be individual students instead of count
          # one line for each enrolled student.
          for(i in 1:length(count)){
            #cat(i, count[i], "\n")
            expanded.major <- c(expanded.major, rep(major[i], count[i]))
          }
          # t.out <- data.frame(major=major, count=count)
          expanded.major
        }
        
        data <- read_excel(paste(path, filename, sep=""))
        # keep only CLGE courses
        t.geog <- ((data$SUBJECT == "GEOG") | (data$SUBJECT == "OCNG") | (data$SUBJECT == "ATMO") | (data$SUBJECT == "METR") | (data$SUBJECT == "GEOS") | (data$SUBJECT == "GEOL") | (data$SUBJECT == "GEOP"))
        data <- data[t.geog,]
        data$COURSE_SECTION_NUMBER <- as.numeric(data$COURSE_SECTION_NUMBER)
        t.low <- data$COURSE_SECTION_NUMBER < 100
        data <- data[!t.low,]
        # remove any NAs
        t.na <- is.na(data$COURSE_SECTION_NUMBER)
        data <- data[!t.na,]
        
        for(i in 1:dim(data)[1]){
          if(i==1) out.data <- c(NULL)
          semester <- data$ACADEMIC_PERIOD[i]
          year <- as.numeric(substring(as.character(data$ACADEMIC_PERIOD[i]),1,4))
          semester.out <- as.numeric(substring(as.character(data$ACADEMIC_PERIOD[i]), 5,5))
          subject <- data$SUBJECT[i]
          course.number <- data$COURSE_NUMBER[i]
          section <- data$COURSE_SECTION_NUMBER[i]
          major.counts <- data$STUDENT_COUNT[i]
          if(!is.na(major.counts)){
            course.designation <- paste(subject, course.number)
            course.designation.1 <- paste(subject, course.number, "-", section, sep="")
            #cat(subject, course.number, section, "\n")
            expanded.majors <- read.counts(major.counts)
            temp.data <- data.frame(semester=semester, subject=subject, course.number=course.number, section=section, major=expanded.majors, designation=course.designation, designation1=course.designation.1)
            out.data <- rbind(out.data, temp.data)     
          }
        }
        out.data
      }   
      processEnrollmentFileCSV <- function(inPath, inFileName){
        read.counts <- function(data){
          # intake one string with format "1-AGEC,3-GEOG," and convert
          # into a vector of majors AGEC, GEOG, GEOG, GEOG
          # that can be used to generate counts and further processed.
          #cat("String =", data, "\n")
          
          # There should be no more than 1 trailing , at the end of the string.
          # remove all but 1 trailing comma when there are more than 1
          i <- nchar(data)
          t.substr <- substr(data,i,i)
          while(t.substr == ","){
            i <- i-1
            t.substr <- substr(data, i,i)
          }
          data <- substr(data, 1, i+1)
          data1 <- paste(",",data, sep="")
          expanded.major <- c(NULL)
          length.of.string <- nchar(data1)
          locations.of.commas <- as.data.frame(str_locate_all(pattern =",", data1))
          locations.of.hyphens <- as.data.frame(str_locate_all(pattern="-", data1))
          num.majors <- dim(locations.of.commas)[1]
          major <- c(NULL)
          count <- c(NULL)
          for(i in 1:(num.majors-1)){
            major <- c(major, substr(data1,locations.of.commas[i+1,1]-4,locations.of.commas[i+1,1]-1))
            count <- c(count, substr(data1,locations.of.commas[i,1]+1,locations.of.hyphens[i,1]-1))
          }
          count <- as.numeric(count)
          # change the output to be individual students instead of count
          # one line for each enrolled student.
          for(i in 1:length(count)){
            #cat(i, count[i], "\n")
            expanded.major <- c(expanded.major, rep(major[i], count[i]))
          }
          # t.out <- data.frame(major=major, count=count)
          expanded.major
        }
        
        
        fullPath <- paste0(inPath, inFileName)
        # #print(file.info(fullPath))
        # #creationDateTime <- file.info(fullPath)$mtime
        # 
        # #cat(fullPath, "\n")
        # cat(format(creationDateTime, "%Y-%m-%d"), "\n")
        # cat("Reading", fullPath, "\n")
        rawFile <- read.table(fullPath, header=FALSE, sep=";", stringsAsFactors = FALSE)
        cat("Read", fullPath, "\n")
        i <- 1
        columnNames <- strsplit(rawFile[i,], ",")
        rawFile <- rawFile[-1,]
        academicPeriod <- str_extract(rawFile, "[^,]+")
        rawFile <- gsub("[^,]*,(.*)", "\\1",rawFile)
        subject <- str_extract(rawFile, "[^,]+")
        rawFile <- gsub("[^,]*,(.*)", "\\1",rawFile)
        #subject <- str_extract(rawFile, "(?<=,)[^,]*(?=,)")
        courseNumber <- str_extract(rawFile, "[^,]+")
        rawFile <- gsub("[^,]*,(.*)", "\\1",rawFile)
        courseSection <- str_extract(rawFile, "[^,]+")
        enrollmentData <- gsub("[^,]*,(.*)", "\\1",rawFile)
        outputData <- data.frame(academicPeriod=academicPeriod, subject=subject, courseNumber=courseNumber, courseSection=courseSection, enrollment=enrollmentData)
        
        keepSubjects <- c("GEOG", "GEOS", "OCNG", "ATMO", "METR", "GEOL", "GEOP")
        outputData <- outputData %>%
          filter(subject %in% keepSubjects) %>%
          mutate(courseSection=as.numeric(as.character(courseSection))) %>%
          filter(!is.na(courseSection)) %>%
          filter(courseSection > 100) 
        cat("dim(outputData):", dim(outputData))
        
        for(i in 1:dim(outputData)[1]){
          if(i ==1) finalOutputData <- c(NULL)
          semester <- outputData$academicPeriod[i]
          year <- as.numeric(substring(as.character(outputData$academicPeriod[i]),1,4))
          semester.out <- as.numeric(substring(as.character(outputData$academicPeriod[i]),5,5))
          subject <- outputData$subject[i]
          course.number <- outputData$courseNumber[i]
          section <- outputData$courseSection[i]
          major.counts <- as.character(outputData$enrollment[i])
          #cat(as.character(subject), as.character(course.number), section, major.counts, "\n")
          if(!is.na(major.counts)){
            expanded.majors <- read.counts(major.counts)
            temp.data <- data.frame(semester=semester, subject=subject, courseNumber=course.number,
                                    courseSection=section, major=expanded.majors, stringsAsFactors = FALSE)
            finalOutputData <- rbind(finalOutputData, temp.data)
          }
        }
        
        # finalOutputData <- finalOutputData %>%
        #   mutate(course.designation=paste(subject, courseNumber)) %>%
        #   mutate(course.designation1=paste0(subject,courseNumber,"-",courseSection)) %>%
        #   mutate(dateCreated=format(creationDateTime, "%Y-%m-%d")) %>%
        #   mutate(timeStamp = creationDateTime)
        
        finalOutputData
        
      }
      readCLGECourseDemographicData <- function(){
        showDebugCues <- FALSE
        if (showDebugCues) cat(red("In readCLGECourseDemographicData\n"))
        buildFromScratch <- TRUE
        `%out%` <- function(a,b) ! a %in% b
        additionalProcessing <- function(output.data){
          year <- output.data$semester %/% 100
          output.data <- data.frame(output.data, year=year)
          Semester.1 <- (output.data$semester %% 100) %/% 10
          output.data <- data.frame(output.data, Semester.1=Semester.1)
          output.data$Semester.1 <- factor(output.data$Semester.1, levels=c(1,2,3), labels=c("Spring", "Summer", "Fall"))
          full.semester <- paste(output.data$Semester.1, output.data$year)
          output.data <- data.frame(output.data, full.semester=full.semester)
          Course <- paste(output.data$subject, output.data$course.number, sep="")
          output.data <- data.frame(output.data, Course)
          
          # There are a bunch of section 550 data that only have 1 person enrolled.
          # These sections do not appear in Howdy.  The following code is to remove 
          # these sections from the database.
          
          output.data <- output.data %>%
            filter(section != 550)    
        }
        
        demographicPath <- paste0(local.archive.path, "EnrollmentDemographics//")
        if(file.exists(paste0(rds.path, "clge.enrollment.demographics.rds"))){
          output.data <- readRDS(paste0(rds.path, "clge.enrollment.demographics.rds"))
          semestersInRDS <- unique(output.data$semester)
          buildFromScratch <- FALSE
        } else {
          semestersInRDS <- NULL
        }
        

        #Determine which semesters are available as .xls files, but are not in the RDS dataset
        files <- dir(demographicPath, "*.xls*")
        semestersAvailable <- substr(files, 1,6)
        filesCSV <- dir(demographicPath, "*.csv")
        semestersAvailableCSV <- substr(filesCSV, 1,6)
        if (showDebugCues==TRUE) cat("semestersAvailable as .csv files:\n")
        if (showDebugCues==TRUE) print(semestersAvailableCSV)
        semestersAvailable <- c(semestersAvailable, semestersAvailableCSV)
        filesCombined <- c(files, filesCSV)
        missingAvailableDemoData <- semestersAvailable[semestersAvailable %out% semestersInRDS]
        if (showDebugCues==TRUE) cat("missingAvailableDemoData:")
        if (showDebugCues==TRUE) print(missingAvailableDemoData)
        if (showDebugCues==TRUE) print(filesCombined)
        #Now only process files that are listed in missingAvailableDemoData.
        t.files <- filesCombined[pmatch(missingAvailableDemoData, filesCombined)]
        if (showDebugCues==TRUE) cat("files that need to be processed:")
        if (showDebugCues==TRUE) print(t.files)
        #cat("length(t.files):", length(t.files), "\n")
        if(length(t.files)>0){
          for(i in 1:length(t.files)){
            cat("Processing", t.files[i], "\n")
            if(substr(t.files[i],8,11)=="csv") {
              if (showDebugCues==TRUE) cat("insert code\n")
              t.data <- processEnrollmentFileCSV(demographicPath, t.files[i])
              t.data <- t.data %>%
                mutate(semester=as.numeric(as.character(semester))) %>%
                rename(c("section" = "courseSection")) %>%
                rename(c("course.number"="courseNumber")) %>%
                mutate(designation=paste(subject, course.number)) %>%
                mutate(designation1=paste0(subject,course.number, "-",section)) 
              
            } else {
              t.data <- read.student.count(demographicPath, t.files[i])
            }
            if((buildFromScratch==TRUE) & (i==1)){
              output.data <- additionalProcessing(t.data)
            }
            else{
              output.data <- rbind(output.data, additionalProcessing(t.data))
            }
          }    
        }
        
        
        if(length(missingAvailableDemoData)>0){
          saveRDS(output.data, paste0(rds.path, "clge.enrollment.demographics.rds"))
        }
        showDebugCues <- FALSE
        output.data
      }
      createCourseEnrollments <- function(inData){
        t.enrollments <- inData %>%
          group_by(semester, designation1, section, Course, year, Semester.1) %>%
          summarise(Enrollment=n(), .groups="drop") %>%
          rename(Full.Name=designation1, Year=year, Section=section, Semester=semester) %>% 
          select("Full.Name", "Enrollment", "Course", "Section", "Year", "Semester", "Semester.1")
        t.enrollments
      }
      
      getMaxFaculty <- function(data){
        t.out <- data %>%
          mutate(end.date=case_when(
            (end.date==2050) ~ NA_real_,
            TRUE ~ end.date
          ))
        max(t.out[,c("start.date", "end.date")], na.rm=TRUE)
        
      }
      
      getMaxIDCDate <- function(data){
        t.out <- data %>%
          filter(fiscalYear==max(fiscalYear)) %>%
          filter(fyMonth==max(fyMonth)) %>%
          mutate(month=substr(.$idcMonth, start=str_length(.$idcMonth)-2, stop=str_length(.$idcMonth))) %>%
          mutate(my=paste0(month, "-", fiscalYear)) %>%
          select("my") %>%
          unique() %>%
          unlist() %>%
          as.vector()
        
        
        t.out
      }
      
      createColorsList <- function(data1=expenditures, data2=idcData){
        #read in the faculty names from expenses
        #read in the faculty names from idc
        
        expNames <- fix.names(data1$facultyName)
        idcNames <- fix.names(data2$facultyName)
        
        allNames <- c(expNames, idcNames)
        t.kor <- (allNames=="O'reilly")
        allNames[t.kor] <- "O'Reilly"
        allNames.unique <- as.vector(unlist(unique(allNames)))
        allNames.unique <- sort(allNames.unique)
        t.colors <- rep(c("#500000", "#003C71", "#5B6236", "#744F28", "#998542", "#332C2C", "#707373", "#D6D3C4", 
                          "#833333", "#336FA4", "#8E9569", "#A7825B", "#CCB875", "#332C2C", "#A3A6A6", "#332C2C"),4)
        t.colors <- t.colors[1:length(allNames.unique)]
        
        names(t.colors) <- allNames.unique
        
        names(t.colors)[match("B. Guneralp", allNames.unique)] <- "B_ Guneralp"
        names(t.colors)[match("I. Guneralp", allNames.unique)] <- "I_ Guneralp"
        t.colors
      }
      
      fix.names <- function(data){
        check.duplicate.names <- function(in.data){
          t.substr <- gsub(",.*$", "", in.data)
          t.first <- gsub('.*,\\s*','', in.data)
          t.first <- substr(t.first, 1,1)
          
          if((t.substr=="Bednarz")|(t.substr=="Guneralp"))
            t.out <- paste0(t.first, ". ", t.substr)
          else
            t.out <- t.substr
          t.out
        }
        t.out <- lapply(data, check.duplicate.names)
        t.out
      }
      processFileInventory <- function(data){
        
        outputData <- data %>%
          mutate(recent=as.character(recent)) %>%
          left_join(semester.codes, by=c("recent"="current")) %>%
          mutate(new.recent=case_when(
            !is.na(longSemester) ~ longSemester,
            TRUE ~ recent
          )) %>%
          select("filename", "description", "updated", "recent", "new.recent") %>%
          left_join(semester.codes, by=c("recent"="sem4")) %>%
          mutate(new.recent=case_when(
            !is.na(longSemester) ~ longSemester,
            TRUE ~ new.recent
          )) %>%
          select("filename", "description", "updated", "recent", "new.recent") %>%
          left_join(semester.codes, by=c("recent"="current.code")) %>%
          mutate(new.recent=case_when(
            !is.na(longSemester) ~ longSemester,
            TRUE ~ new.recent
          )) %>%
          select("filename", "description", "updated", "recent"="new.recent") 
        
        outputData
      }
      
      PreviousSemester <- function(current.semester, semester.codes, numeric=FALSE, digits=4){
        if (numeric==TRUE){
          t.1 <- semester.codes %>%
            filter(current==current.semester) %>%
            select(previous)
        } else {
          t.1 <- semester.codes %>%
            filter(current==current.semester) %>%
            select(grads)
        }
        t.1
      }
      CurrentSemester <- function(numeric=FALSE, digits=4){
        t.year <- year(Sys.Date())
        year.digit <- t.year
        
        two.digit.year <- year.digit %% 1000
        t.c <- paste(t.year,"-12-15", sep="")
        t.b <- paste(t.year,"-8-25", sep="")
        t.a <- paste(t.year,"-5-15", sep="")
        current.date <- as.character(Sys.Date())
        if(digits==4) {paste.year <- year.digit}
        if(digits==2) {paste.year <- two.digit.year}
        if(numeric){
          paste.year <- year.digit
          if(current.date < t.a) {
            t.letter <- "11"
          } else if (current.date < t.b) {
            t.letter <- "21"
          } else t.letter <- "31"
          current.semester1 <- paste(paste.year, t.letter, sep="") 
          current.semester1 <- as.numeric(current.semester1)
        } else{
          if(current.date < t.a) {
            t.letter <- "A"
          } else if (current.date < t.b) {
            t.letter <- "B"
          } else t.letter <- "C"
          current.semester1 <- paste(paste.year, t.letter, sep="")    
        }
        current.semester1
      }
      
      ############################################
      # Graduate Update Functions                #
      ############################################
      
      updateActiveGraduateStudents <- function(inData, inActiveStudents,
                                               inGraduationData){
        
        if (showDebugCues==TRUE) cat(green("Initial number of records in inData:", nrow(inData), "\n"))
        newData <- inActiveStudents %>%
          mutate(UIN=as.character(UIN)) %>%
          mutate(semester.numeric=as.numeric(semester.numeric)) %>%
          anti_join(inData, by=c("UIN"="UIN", "semester.numeric"="semester.numeric")) %>%
          left_join(inData, by=c("NAMES"="NAMES", "UIN"="UIN", "semester.numeric"="semester.numeric",
                                 "CLASSFICATION"="CLASSFICATION", 
                                 "PROGRAM1_1ST_DEG"="PROGRAM1_1ST_DEG", "semester.abbrv")) %>%
          mutate(LastName=str_extract(NAMES, "(?<=)(.*?)(?=\\,)")) %>%
          mutate(FirstName=str_extract(NAMES, "(?<=, )(.*?)(?= |$)")) %>%
          mutate(MiddleName=str_extract(NAMES, "\\s([A-Za-z.]+)$")) %>%
          mutate(degreeSought=str_extract(PROGRAM1_1ST_DEG, "(?<=)(.*?)(?=\\-)")) %>%
          left_join(inData[,c("UIN", "Photo")], by=c("UIN"="UIN")) %>%
          mutate(Photo.x = Photo.y) %>%
          select(-"Photo.y") %>%
          rename(Photo=Photo.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "Email")], by=c("UIN"="UIN")) %>%
          mutate(Email.x = Email.y) %>%
          select(-"Email.y") %>%
          rename(Email=Email.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "LinkedIn")], by=c("UIN"="UIN")) %>%
          mutate(LinkedIn.x = LinkedIn.y) %>%
          select(-"LinkedIn.y") %>%
          rename(LinkedIn=LinkedIn.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "Facebook")], by=c("UIN"="UIN")) %>%
          mutate(Facebook.x = Facebook.y) %>%
          select(-"Facebook.y") %>%
          rename(Facebook=Facebook.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "Twitter")], by=c("UIN"="UIN")) %>%
          mutate(Twitter.x = Twitter.y) %>%
          select(-"Twitter.y") %>%
          rename(Twitter=Twitter.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "Reddit")], by=c("UIN"="UIN")) %>%
          mutate(Reddit.x = Reddit.y) %>%
          select(-"Reddit.y") %>%
          rename(Reddit=Reddit.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "Instagram")], by=c("UIN"="UIN")) %>%
          mutate(Instagram.x = Instagram.y) %>%
          select(-"Instagram.y") %>%
          rename(Instagram=Instagram.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "ResearchGate")], by=c("UIN"="UIN")) %>%
          mutate(ResearchGate.x = ResearchGate.y) %>%
          select(-"ResearchGate.y") %>%
          rename(ResearchGate=ResearchGate.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "Orcid")], by=c("UIN"="UIN")) %>%
          mutate(Orcid.x = Orcid.y) %>%
          select(-"Orcid.y") %>%
          rename(Orcid=Orcid.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "Github")], by=c("UIN"="UIN")) %>%
          mutate(Github.x = Github.y) %>%
          select(-"Github.y") %>%
          rename(Github=Github.x) %>%
          unique() %>%
          left_join(inGraduationData[,c("UIN", "DEGC", "GRAD_DATE")], by=c("UIN"="UIN", "degreeSought"="DEGC")) %>%
          mutate(GRAD_DATE.x=GRAD_DATE.y) %>%
          select(-"GRAD_DATE.y") %>%
          rename(GRAD_DATE=GRAD_DATE.x) %>%
          unique() %>%
          left_join(inGraduationData[,c("UIN", "DEGC", "STUDENT")], by=c("UIN"="UIN", "degreeSought"="DEGC")) %>%
          mutate(STUDENT.x=STUDENT.y) %>%
          select(-"STUDENT.y") %>%
          rename(STUDENT=STUDENT.x) %>%
          unique() %>%
          left_join(inGraduationData[,c("UIN", "DEGC", "MAJR")], by=c("UIN"="UIN", "degreeSought"="DEGC")) %>%
          mutate(MAJR.x=MAJR.y) %>%
          select(-"MAJR.y") %>%
          rename(MAJR=MAJR.x) %>%
          unique() %>%
          left_join(inGraduationData[,c("UIN", "DEGC", "calendar.year")], by=c("UIN"="UIN", "degreeSought"="DEGC")) %>%
          mutate(calendar.year.x=calendar.year.y) %>%
          select(-"calendar.year.y") %>%
          rename(calendar.year=calendar.year.x) %>%
          unique() %>%
          left_join(inGraduationData[,c("UIN", "DEGC", "fiscal.year")], by=c("UIN"="UIN", "degreeSought"="DEGC")) %>%
          mutate(fiscal.year.x=fiscal.year.y) %>%
          select(-"fiscal.year.y") %>%
          rename(fiscal.year=fiscal.year.x) %>%
          unique() %>%
          left_join(inGraduationData[,c("UIN", "DEGC", "semester")], by=c("UIN"="UIN", "degreeSought"="DEGC")) %>%
          mutate(semester.x=semester.y) %>%
          select(-"semester.y") %>%
          rename(semester=semester.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "DateSubmitted")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(DateSubmitted.x=DateSubmitted.y) %>%
          select(-"DateSubmitted.y") %>%
          rename(DateSubmitted=DateSubmitted.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "DateApprovedByFaculty")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(DateApprovedByFaculty.x=DateApprovedByFaculty.y) %>%
          select(-"DateApprovedByFaculty.y") %>%
          rename(DateApprovedByFaculty=DateApprovedByFaculty.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "ProposalDefense")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(ProposalDefense.x=ProposalDefense.y) %>%
          select(-"ProposalDefense.y") %>%
          rename(ProposalDefense=ProposalDefense.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "ProposalSubmitDate")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(ProposalSubmitDate.x=ProposalSubmitDate.y) %>%
          select(-"ProposalSubmitDate.y") %>%
          rename(ProposalSubmitDate=ProposalSubmitDate.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "ResidencyReq")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(ResidencyReq.x=ResidencyReq.y) %>%
          select(-"ResidencyReq.y") %>%
          rename(ResidencyReq=ResidencyReq.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "PreliminaryExamDate")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(PreliminaryExamDate.x=PreliminaryExamDate.y) %>%
          select(-"PreliminaryExamDate.y") %>%
          rename(PreliminaryExamDate=PreliminaryExamDate.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "PreliminaryExamResult")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(PreliminaryExamResult.x=PreliminaryExamResult.y) %>%
          select(-"PreliminaryExamResult.y") %>%
          rename(PreliminaryExamResult=PreliminaryExamResult.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "ThesisDefenseDate")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(ThesisDefenseDate.x=ThesisDefenseDate.y) %>%
          select(-"ThesisDefenseDate.y") %>%
          rename(ThesisDefenseDate=ThesisDefenseDate.x) %>%
          unique() %>%
          left_join(inData[,c("UIN", "degreeSought", "ThesisSubmitDate")], by=c("UIN"="UIN", "degreeSought"="degreeSought")) %>%
          mutate(ThesisSubmitDate.x=ThesisSubmitDate.y) %>%
          select(-"ThesisSubmitDate.y") %>%
          rename(ThesisSubmitDate=ThesisSubmitDate.x) %>%
          unique() 
        
        outData <- rbind(inData, newData)
        outData <- outData %>%
          group_by(UIN, degreeSought) %>%
          mutate(GRAD_DATE=last(GRAD_DATE)) %>%
          mutate(STUDENT=last(STUDENT)) %>%
          mutate(MAJR=last(MAJR)) %>%
          mutate(calendar.year=last(calendar.year)) %>%
          mutate(fiscal.year=last(fiscal.year)) %>%
          mutate(semester=last(semester)) %>%
          mutate(X=first(X)) %>%
          mutate(termAdmitted=first(termAdmitted)) %>%
          mutate(termAdmitted=case_when(is.na(termAdmitted) ~ min(semester.numeric),
                                        TRUE ~ termAdmitted)) %>%
          mutate(Photo=case_when(is.na(Photo) ~ "no_image_placeholder.png",
                                 TRUE ~ Photo)) %>%
          ungroup()
        
        if(showDebugCues==TRUE) cat(green("Final number of records in outData:", nrow(outData), "\n"))
        outData
        
      }
      
      ############################################
      # Graduate Application Functions           #
      ############################################
      
      readApplicantInfo <- function(inData, force.create=FALSE, inDataPath, outDataPath){
        
        keepDepartments <- c("GEOG", "OCNG", "ATMO", "GEPL")
        cat(yellow("[readInputDataServer]\n"), green("keepDepartments:", keepDepartments, "\n"))
        if(is.null(inData)|force.create==TRUE){
          # read all of the files available
          outData <- inData
          existingFiles <- dir(inDataPath)
          #eliminate locked files from the list
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          fileInfo <- file.info(existingFilesFullPath)
          for(i in 1:nrow(fileInfo)){
            tryCatch(
              { 
                cat("Trying to read:", row.names(fileInfo)[i], "\n")
                partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                  data.frame()
              },
              error = function(cond){
                #message("Data are not an excel file.  Reading it as a .csv file.")
                #browser()
                
                partialData <<- read.csv(row.names(fileInfo)[i], na.strings="") 
                #print(names(partialData))
                
                t.names <- names(partialData)
                potentialProblems <- c("COMPLETE.DATE", "SENT.DEPT.DATE")
                if(sum(potentialProblems %in% t.names)!=2){
                  cat(blue("missing fields\n"))
                  partialData <- partialData %>%
                    mutate(COMPLETE.DATE=NA) %>%
                    mutate(SENT.DEPT.DATE=NA)
                }
                    
                partialData <<- partialData %>%
                  rename("DECISION DATE"="DECISION.DATE", "FEE PAID"="FEE.PAID",
                         "COMPLETE DATE"="COMPLETE.DATE", "SENT DEPT DATE"="SENT.DEPT.DATE",
                         "GRE-Official"="GRE.Official", "GR-V"="GR.V","GR-Q"="GR.Q",
                         "GR-W"="GR.W", "TOEP-Official"="TOEP.Official") %>%
                  data.frame() 
                
                cat(red("past read.csv\n"))
                
              },
              finally = {
                #cat("here\n")
              }
            )
            
            partialDataTest <- partialData %>%
              select("TERM", "ADMT_CODE", "DEPARTMENT", "MAJOR", "PROGRAM", "UIN", "LASTNAME",
                     "FIRSTNAME", "MIDDLENAME", "DECISION_CODE",
                     "ETHDesc", "GN", "RS", "DECISION.DATE", "APPL_DATE") %>%
              mutate(ETHDesc=str_trim(ETHDesc)) %>%
              filter(DEPARTMENT %in% keepDepartments) %>%
              #filter(DEPARTMENT == "GEOG") %>%
              #filter(MAJOR != "WMHS") %>%
              mutate(degreeSought=str_extract(PROGRAM, "(?<=)(.*?)(?=\\-)")) %>%
              mutate(DECISION.DATE=as.POSIXct.default(DECISION.DATE, tryFormat=("%m/%d/%Y"))) %>%
              mutate(APPL_DATE=as.POSIXct.default(APPL_DATE, tryFormat="%m/%d/%Y"))
            
            outData <- rbind(outData, partialDataTest) %>%
              unique()
          }
          saveRDS(outData, paste0(outDataPath, "applicants.rds"))
        } else {
          cat(yellow("[readInputDataServer]"), red("File exists (applicants.rds) \n"))
          rdsCreationDate <- file.info(paste0(outDataPath, "applicants.rds"))$mtime
          existingFiles <- dir(inDataPath)
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          assign("t.effp", existingFilesFullPath, pos=1)
          fileInfo <- file.info(existingFilesFullPath)
          processFiles <- fileInfo %>%
            filter(mtime > rdsCreationDate)
          outData <- inData
          #print(processFiles)
          #cat(red("Need to process", nrow(processFiles), "files.\n"))
          if(nrow(processFiles)>0){
            for(i in 1:dim(processFiles)[1]){
              tryCatch(
                { 
                  cat("Trying to read:", row.names(fileInfo)[i], "\n")
                  partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                    data.frame()
                },
                error = function(cond){
                  #message("Data are not an excel file.  Reading it as an .csv file.")
                  partialData <<- read.csv(row.names(fileInfo)[i], na.strings="") %>%
                    rename("DECISION DATE"="DECISION.DATE", "FEE PAID"="FEE.PAID",
                           "COMPLETE DATE"="COMPLETE.DATE", "SENT DEPT DATE"="SENT.DEPT.DATE",
                           "GRE-Official"="GRE.Official", "GR-V"="GR.V","GR-Q"="GR.Q",
                           "GR-W"="GR.W", "TOEP-Official"="TOEP.Official") %>%
                    data.frame() 
                  
                },
                finally = {
                  #cat("here\n")
                }
              )
              
              partialDataTest <- partialData %>%
                select("TERM", "ADMT_CODE", "DEPARTMENT", "MAJOR", "PROGRAM", "UIN", "LASTNAME",
                       "FIRSTNAME", "MIDDLENAME", "DECISION_CODE",
                       "ETHDesc", "GN", "RS", "DECISION.DATE", "APPL_DATE") %>%
                mutate(ETHDesc=str_trim(ETHDesc)) %>%
                filter(DEPARTMENT %in% keepDepartments) %>%
                #filter(DEPARTMENT == "GEOG") %>%
                #filter(MAJOR != "WMHS") %>%
                mutate(degreeSought=str_extract(PROGRAM, "(?<=)(.*?)(?=\\-)")) %>%
                mutate(DECISION.DATE=as.POSIXct.default(DECISION.DATE, tryFormat=("%m/%d/%Y"))) %>%
                mutate(APPL_DATE=as.POSIXct.default(APPL_DATE, tryFormat="%m/%d/%Y"))
              
              # remove duplicates.
              outData <- rbind(outData, partialDataTest) %>%
                unique()

              # write new rds file 
              saveRDS(outData, paste0(outDataPath, "applicants.rds"))
            }
          }
        }
        outData
      }
      
      ############################################
      # Enrolled Student Functions               #
      ############################################
      
      readEnrolledStudents <- function(inData, force.create=TRUE, inDataPath, outDataPath){
        if (force.create==TRUE) {
          inData <- NULL
        }
        outData <- inData
        if(is.null(inData)|force.create==TRUE){
          # read all of the files available
          existingFiles <- dir(inDataPath)
          #eliminate locked files from the list
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          fileInfo <- file.info(existingFilesFullPath)
          for(i in 1:nrow(fileInfo)){
            tryCatch(
              { 
                cat("2Trying to read:", row.names(fileInfo)[i], "\n")
                partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                  data.frame()
              },
              error = function(cond){
                message("2Data are not an excel file.  Reading it as a .csv file.")
                partialData <<- read.csv(row.names(fileInfo)[i], na.strings="") %>%
                  select("LAST_NAME", "FIRST_NAME", "TAMU_UIN", "STUDENT_CLASSIFICATION", "COLLEGE", 
                         "DEPARTMENT", "MAJOR", "DEGREE", "FIRST_TERM_GR",
                         "MIDDLE_NAME", "FIRST_CONCENTRATION", "COLLEGE") %>%
                  mutate(TERM=max(as.numeric(FIRST_TERM_GR)))
                
                
              },
              finally = {
                #cat("here\n")
              }
            )
            cat(names(partialData), "\n")
            
            cat("Before the rbind\n")
            outData <- rbind(outData, partialData)
          }
          saveRDS(outData, paste0(outDataPath, "enrolledStudents.rds"))
        } else {
          #cat(red("File exists\n"))
          rdsCreationDate <- file.info(paste0(outDataPath, "enrolledStudents.rds"))$mtime
          existingFiles <- dir(inDataPath)
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          #assign("t.effp", existingFilesFullPath, pos=1)
          fileInfo <- file.info(existingFilesFullPath)
          processFiles <- fileInfo %>%
            filter(mtime > rdsCreationDate)
          outData <- inData
          #print(processFiles)
          #cat(red("Need to process", nrow(processFiles), "files.\n"))
          if(nrow(processFiles)>0){
            for(i in 1:dim(processFiles)[1]){
              tryCatch(
                { 
                  cat("3Trying to read:", row.names(fileInfo)[i], "\n")
                  partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                    data.frame()
                },
                error = function(cond){
                  message("3Data are not an excel file.  Reading it as an .csv file.")
                  partialData <<- read.csv(row.names(fileInfo)[i], na.strings="") %>%
                    select("LAST_NAME", "FIRST_NAME", "TAMU_UIN", "STUDENT_CLASSIFICATION", "COLLEGE", 
                           "DEPARTMENT", "MAJOR", "DEGREE", "FIRST_TERM_GR",
                           "MIDDLE_NAME", "FIRST_CONCENTRATION", "COLLEGE") %>%
                    mutate(TERM=max(as.numeric(FIRST_TERM_GR)))
                  
                },
                finally = {
                  #cat("here\n")
                }
              )
              
              outData <- rbind(outData, partialData)
              
              # Here is where a new rds file needs to be written.
              saveRDS(outData, paste0(outDataPath, "enrolledStudents.rds"))
            }
          }
        }
        outData
      }
      

      ############################################
      # Student Supervisory Committee Functions  #
      ############################################
      
      readStudentSupervisoryCommittees <- function(inData, inDept, force.create=FALSE, inDataPath, outDataPath){
        outData <- inData
        
        readSingleCommitteeFile <- function(inFileName){
          #################################################
          # Establish a file connection.  Then read each  #
          # line of the file into a separate element of a #
          # vector with number of elements equal to the   #
          # number of lines. Tidy up by unlinking the     #
          # connection.                                   #
          #################################################
          #cat(green(inFileName, "\n"))
          
          fil <- file(inFileName)
          t.out <- readLines(fil, n=-1)
          #cat(green(t.out, "\n"))
          unlink(fil)
          close(fil)
          
          ##################################################
          # Process each line.  The first line will        #
          # establish the separator for the file.          #
          # The current term is in the parameters at the   #
          # end of the file.                               #
          #                                                #
          ##################################################
          
          theSeparator <- "|"
          t.outNames <- unlist(str_split(t.out[2], fixed(theSeparator)))
          #cat(t.outNames, "\n")
          t.outData <- str_split(t.out[3:length(t.out)], fixed(theSeparator))
          
          shortLines <- NULL
          for(i in 3:length(t.outData)){
            if(length(unlist(t.outData[i])) != length(t.outNames)){
              #cat("i:", i, "length:", length(unlist(t.outData[i])), "\n")
              shortLines <- c(shortLines, i)
              
            }
          }
          # remove the shortLines from the data
          allIndices <- c(1:length(t.outData))
          goodIndices <- allIndices %out% shortLines
          t.outDataGood <- t.outData[goodIndices]
          
          # subset out the parameter data
          t.parameterData <- t.outData[shortLines]
          #print(t.parameterData)
          #cat("after t.parameterData\n")
          fixedParameterData <- NULL
          for(i in 1:length(t.parameterData)){
            #cat("in the loop\n")
            if(length(t.parameterData[[i]]) > 1){
              #cat("setting fixedParameterData\n")
              fixedParameterData <- c(fixedParameterData, t.parameterData[i])
            }
          }
          #cat("Before fixedParameterData\n")
          #print(fixedParameterData)
          theTerm <- fixedParameterData[[1]][3]
          #cat("After fixedParameterData\n")
          #cat(red(theTerm, "\n"))
          
          # convert the good data into a matrix
          unlistedData <- unlist(t.outDataGood)
          theMatrix <- matrix(unlist(t.outDataGood), ncol=length(t.outNames), byrow=TRUE)

          theDataFrame <- data.frame(theMatrix)
          names(theDataFrame) <- t.outNames
          theDataFrame %>%
            mutate(TERM=theTerm)
        }
        
        if(is.null(inData)|force.create==TRUE){
          # read all of the files available
          existingFiles <- dir(inDataPath)
          #eliminate locked files from the list
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          fileInfo <- file.info(existingFilesFullPath)
          
          for(i in 1:nrow(fileInfo)){
            #for(i in 1:1){
            tryCatch(
              {
                message(paste("Reading", row.names(fileInfo)[i]))
                partialData <<- readSingleCommitteeFile(row.names(fileInfo)[i]) %>%
                  select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                         "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") 
                
              },
              error = function(cond){
                cat("Trying to read:", row.names(fileInfo)[i], "\n")
                partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                  select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                         "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") %>%
                  data.frame()
              },
              finally = {
                #cat("here\n")
              }
            )
            cat("nrow(partialData):", nrow(partialData), "\n")
            outData <- rbind(outData, partialData) 
          }
          saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
        } else {
          #cat(red("File exists\n"))
          rdsCreationDate <- file.info(paste0(outDataPath, "studentSupervisoryCommittees.rds"))$mtime
          existingFiles <- dir(inDataPath)
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          fileInfo <- file.info(existingFilesFullPath)
          processFiles <- fileInfo %>%
            filter(mtime > rdsCreationDate)
          outData <- inData
          if(nrow(processFiles)>0){
            for(i in 1:dim(processFiles)[1]){
              tryCatch(
                { 
                  message(paste("Reading", row.names(fileInfo)[i]))
                  partialData <<- readSingleCommitteeFile(row.names(fileInfo)[i]) %>%
                    select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                           "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") 
                  
                },
                error = function(cond){
                  cat("Trying to read:", row.names(fileInfo)[i], "\n")
                  partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                    select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                           "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") %>%
                    data.frame()
                  
                },
                finally = {
                  #cat("here\n")
                }
              )
              #cat("Before the rbind: exists(partialData)=", exists("partialData"), ", exists(partialDataTest)=", exists("partialDataTest"), "\n")
              outData <- rbind(outData, partialData)
              
              # Here is where a new rds file needs to be written.
              saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
            }
          }
        }
        
        outData
      }
      
      ############################################
      # Degree Progress Integration Functions    #
      ############################################
      
      integrateStudentMilestoneData <- function(inData, inDept, force.create=FALSE, inDataPath, outDataPath){
        if(is.null(inData)){
          cat(red("Cannot process Milestone Data becasues inData is NULL!\n"))
        } else {
          outData <- inData
          inDataPath <- paste0(inDataPath,inDept, "//")
          processDegreeMilestoneFile <- function(inData){
            # Process the degree milestone file to match field names in fullStudentData
            #
            keepCodes <- c("M1DP", "M2PR", "M2RS", "M4FE", "M5TH", "P1DP", "P2PL", "P2PR", "P3AC", "P2PF", "P2RS","P4FE","P5TH" )
            # Handle the legacy data
            tNames <- names(inData)
            
            if ("SHRNCRS.SHRNCRS_SEQ_NO" %in% tNames) {
              ##########################################
              # Convert legacy file forman names into  #
              # names appropriate for the rest of this #
              # procedure.                             #
              ##########################################
              theData <- inData %>%
                select("UIN"="odsmgr.FWS_GET_TAMU_UIN.MST_GENERAL_STUDENT.PERSON_UID.",
                       "degreeSought"="MST_GENERAL_STUDENT.DEGREE",
                       "ADATE"="SHRNCRS.SHRNCRS_NCST_DATE",
                       "DESC"="NCRQ_DESC",
                       "NON_CRS_REQ_CODE"="SHRNCRS.SHRNCRS_NCRQ_CODE") %>% #,
                # "DEGREE_OUTCOME"="OUTCOME") %>%
                filter(NON_CRS_REQ_CODE %in% keepCodes) %>%
                mutate(letter = substr(NON_CRS_REQ_CODE,1,1)) %>%
                mutate(letter2 = substr(degreeSought,1,1)) %>%
                filter(letter==letter2) %>%
                unique() %>%
                mutate(DESC=case_when(DESC=="Doctoral Degree Plan" ~ "DateSubmitted",
                                      DESC=="Master's Degree Plan" ~ "DateSubmitted",
                                      DESC=="Doctoral Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Master's Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Preliminary Examination" ~ "PreliminaryExamDate",
                                      DESC=="Final Examination/Defense" ~ "ThesisDefenseDate",
                                      DESC=="Thesis" ~ "ThesisSubmitDate",
                                      DESC=="Master's Residence Req" ~ "ResidencyReq",
                                      DESC=="Doctoral Residence Req" ~ "ResidencyReq",
                                      TRUE ~ "Other")) %>%
                filter(DESC != "Other") %>%
                select("UIN", "degreeSought", "ADATE", "DESC") %>%
                unique() %>%
                group_by(UIN, degreeSought, DESC) %>%
                mutate(ADATE1=last(ADATE)) %>%
                mutate(ADATE=ADATE1) %>%
                select("UIN", "degreeSought", "ADATE", "DESC") %>%
                unique() %>%
                mutate(ADATE=as.Date(ADATE, "%m/%d/%Y")) %>%
                mutate(ADATE=as.character(ADATE)) %>%
                pivot_wider(names_from=DESC, values_from=ADATE)
              assign("t.legacy", theData, pos=1)
            } else {
              theData <- inData %>%
                select("UIN"="TAMU_UIN", "degreeSought"="DEGREE_CODE", 
                       "ADATE"="SHRNCRS_ACTIVITY_DATE", "DESC"="NON_CRS_REQ_CODE_DESC",
                       "NON_CRS_REQ_CODE", "DEGREE_OUTCOME") %>%
                filter(NON_CRS_REQ_CODE %in% keepCodes) %>%
                mutate(letter = substr(NON_CRS_REQ_CODE,1,1)) %>%
                mutate(letter2 = substr(degreeSought,1,1)) %>%
                filter(letter==letter2) %>%
                mutate(DESC=case_when(DESC=="Doctoral Degree Plan" ~ "DateSubmitted",
                                      DESC=="Master's Degree Plan" ~ "DateSubmitted",
                                      DESC=="Doctoral Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Master's Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Preliminary Examination" ~ "PreliminaryExamDate",
                                      DESC=="Final Examination/Defense" ~ "ThesisDefenseDate",
                                      DESC=="Thesis" ~ "ThesisSubmitDate",
                                      DESC=="Master's Residence Req" ~ "ResidencyReq",
                                      DESC=="Doctoral Residence Req" ~ "ResidencyReq",
                                      TRUE ~ "Other")) %>%
                filter(DESC != "Other") %>%
                select("UIN", "degreeSought", "ADATE", "DESC") %>%
                unique() %>%
                mutate(ADATE=as.Date(ADATE, "%m-%d-%Y")) %>%
                mutate(ADATE=as.character(ADATE)) %>%
                pivot_wider(names_from=DESC, values_from=ADATE)
            }
            theData
          }
          readSingleDegreeMilestoneFile <- function(inFileName){
            theData <- read.csv(inFileName)
            theData
          }
          
          if(inDept=="GEOG"){
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", ".rds"))$mtime
          } else {
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", inDept, ".rds"))$mtime  
          }
          
          existingFiles <- dir(inDataPath)
          if(length(existingFiles)==0){
            cat(red("There are no files\n"))
            outData <- inData
          } else {
            existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
            existingFilesFullPath <- paste0(inDataPath, existingFiles)
            fileInfo <- file.info(existingFilesFullPath)
            if(force.create==TRUE){
              processFiles <- fileInfo
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
              #print(processFiles)
            } else {
              processFiles <- fileInfo %>%
                filter(mtime > rdsCreationDate)
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
            }
            #processFiles <- fileInfo
            outData <- inData
            #cat(green("before if\n"))
            if(nrow(processFiles)>0){
              #cat(green("nrow(processFiles)>0\n"))
              keepNames <- names(outData)
              #cat("keepNames:", keepNames, "\n")
              for(i in 1:dim(processFiles)[1]){
                #for(i in 1:1){
                
                message(paste("Reading", row.names(fileInfo)[i]))
                pD <- readSingleDegreeMilestoneFile(row.names(fileInfo)[i])
                pD <- processDegreeMilestoneFile(pD)
                
                
                ##########################################
                # Integrate the data from pD into inData #
                ##########################################
                
                pD <- pD %>%
                  mutate(UIN=as.character(UIN))
                outData <- outData %>%
                  mutate(UIN=as.character(UIN)) %>%
                  left_join(pD, by=c("UIN", "degreeSought")) %>%
                  mutate_if(is.Date, as.character) 
                
                #cat(blue(names(outData)), "\n")
                if("DateSubmitted.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(DateSubmitted=case_when(!is.na(DateSubmitted.y) ~ DateSubmitted.y,
                                                   TRUE ~ DateSubmitted.x)) 
                }
                if("ResidencyReq.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ResidencyReq=case_when(!is.na(ResidencyReq.y) ~ ResidencyReq.y,
                                                  TRUE ~ ResidencyReq.x)) 
                }
                if("PreliminaryExamDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(PreliminaryExamDate=case_when(!is.na(PreliminaryExamDate.y) ~ PreliminaryExamDate.y,
                                                         TRUE ~ PreliminaryExamDate.x)) 
                }
                if("ProposalSubmitDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ProposalSubmitDate=case_when(!is.na(ProposalSubmitDate.y) ~ ProposalSubmitDate.y,
                                                        TRUE ~ ProposalSubmitDate.x)) 
                }
                if("ThesisDefenseDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ThesisDefenseDate=case_when(!is.na(ThesisDefenseDate.y) ~ ThesisDefenseDate.y,
                                                       TRUE ~ ThesisDefenseDate.x))
                }
                ##############################################
                # Add mutate statements for ThesisSubmitDate #
                # in an if statment                          #
                ##############################################
                if("ThesisSubmitDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ThesisSubmitDate=case_when(!is.na(ThesisSubmitDate.y) ~ ThesisSubmitDate.y,
                                                      TRUE ~ ThesisSubmitDate.x))
                }
                ##############################################
                # Eliminate the new fields with .x and .y    #
                ##############################################
                
                possibleNames <- c(keepNames, "DateSubmitted", "ResidencyReq", "PreliminaryExamDate", "ProposalSubmitDate", 
                                   "ThesisDefenseDate", "ThesisSubmitDate")
                actualNames <- union(keepNames, possibleNames)
                theFinalNames <- intersect(names(outData), actualNames)
                
                outData <- outData %>%
                  select(all_of(theFinalNames))
                #select(all_of(keepNames))
                #cat(green(names(outData), "\n"))
                # Here is where a new rds file needs to be written.
                #saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
              }
            }
          }
        }
        #cat(blue("before mutates\n"))
        theDateFormat <- "%Y-%m-%d"
        if(!("DateSubmitted" %in% names(outData))){
          outData <- outData %>%
            mutate(DateSubmitted=NA) %>%
            mutate(DateSubmitted=as.Date(DateSubmitted))
        }
        if(!("DateApprovedByFaculty" %in% names(outData))){
          outData <- outData %>%
            mutate(DateApprovedByFaculty =NA) %>%
            mutate(DateApprovedByFaculty=as.Date(DateApprovedByFaculty))
        }
        if(!("ProposalDefense" %in% names(outData))){
          outData <- outData %>%
            mutate(ProposalDefense = NA) %>%
            mutate(ProposalDefense = as.Date(ProposalDefense))
        }
        if(!("ProposalSubmitDate" %in% names(outData))){
          outData <- outData %>%
            mutate(ProposalSubmitDate = NA) %>%
            mutate(ProposalSubmitDate = as.Date(ProposalSubmitDate))
        }
        if(!("ResidencyReq" %in% names(outData))){
          outData <- outData %>%
            mutate(ResidencyReq = NA) %>%
            mutate(ResidencyReq = as.Date(ProposalSubmitDate))
        }
        if(!("PreliminaryExamDate" %in% names(outData))){
          outData <- outData %>%
            mutate(PreliminaryExamDate = NA) %>%
            mutate(PreliminaryExamDate = as.Date(PreliminaryExamDate))
        }
        if(!("ThesisDefenseDate" %in% names(outData))){
          outData <- outData %>%
            mutate(ThesisDefenseDate = NA) %>%
            mutate(ThesisDefenseDate = as.Date(ThesisDefenseDate))
        }
        if(!("ThesisSubmitDate" %in% names(outData))){
          outData <- outData %>%
            mutate(ThesisSubmitDate = NA) %>%
            mutate(ThesisSubmitDate = as.Date(ThesisSubmitDate))
        }
        if(!("PreliminaryExamResult" %in% names(outData))){
          outData <- outData %>%
            mutate(PreliminaryExamResult = NA)
        }
        # if(!("DateSubmitted" %in% names(outData))){
        #   #cat(green("No DP fields in outData\n"))
        #   outData <- outData %>%
        #     mutate(DateSubmitted=NA) %>%
        #     mutate(DateSubmitted=as.Date(DateSubmitted)) %>%
        #     mutate(DateApprovedByFaculty=NA) %>%
        #     mutate(DateApprovedByFaculty=as.Date(DateApprovedByFaculty)) %>%
        #     mutate(ProposalDefense=NA) %>%
        #     mutate(ProposalDefense=as.Date(ProposalDefense)) %>%
        #     mutate(ProposalSubmitDate=NA) %>%
        #     mutate(ProposalSubmitDate=as.Date(ProposalSubmitDate)) %>%
        #     mutate(ResidencyReq=NA) %>%
        #     mutate(ResidencyReq=as.Date(ResidencyReq)) %>%
        #     mutate(PreliminaryExamDate=NA) %>%
        #     mutate(PreliminaryExamDate=as.Date(PreliminaryExamDate)) %>%
        #     mutate(ThesisDefenseDate=NA) %>%
        #     mutate(ThesisDefenseDate=as.Date(ThesisDefenseDate)) %>%
        #     mutate(ThesisSubmitDate=NA) %>%
        #     mutate(ThesisSubmitDate=as.Date(ThesisSubmitDate)) %>%
        #     mutate(PreliminaryExamResult=NA)
        # } else {
        outData <- outData %>%
          mutate(DateSubmitted=as.Date(DateSubmitted, theDateFormat)) %>%
          mutate(DateApprovedByFaculty=as.Date(DateApprovedByFaculty, theDateFormat)) %>%
          mutate(ProposalDefense= as.Date(ProposalDefense, theDateFormat)) %>%
          mutate(ProposalSubmitDate=as.Date(ProposalSubmitDate, theDateFormat)) %>%
          mutate(ResidencyReq=as.Date(ResidencyReq, theDateFormat)) %>%
          mutate(PreliminaryExamDate=as.Date(PreliminaryExamDate, theDateFormat)) %>%
          mutate(ThesisDefenseDate=as.Date(ThesisDefenseDate, theDateFormat)) %>%
          mutate(ThesisSubmitDate=as.Date(ThesisSubmitDate, theDateFormat)) %>%
          unique()
        # }
        
        outData
      }
      
      ############################################
      # Social Media Integration Functions       #
      ############################################
      
      integrateSocialMediaData <- function(inData, inDept, force.create=FALSE, inDataPath, outDataPath){
        if(is.null(inData)){
          cat(red("Cannot process Social Media Data becasues inData is NULL!\n"))
        } else {
          outData <- inData
          inDataPath <- paste0(inDataPath,inDept, "//")
          #cat("test1","***\n")
          
          readSingleSocialMediaFile <- function(inFileName){
            #cat("inFileName:", inFileName, "\n")
            theData <- read.csv(inFileName)
            #Remove everything except UIN and Social Media Fields
            keepNames <- c("UIN", "LinkedIn", "Facebook", "Twitter", "Reddit", "Instagram", "ResearchGate", "Orcid", "Github")
            theNames <- intersect(keepNames, names(theData))
            theData <- theData %>%
              select(all_of(theNames))
            
            theData
          }
          
          if(inDept=="GEOG"){
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", ".rds"))$mtime
          } else {
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", inDept, ".rds"))$mtime  
          }
          #cat("inDataPath:", inDataPath, "***\n")
          existingFiles1 <- dir("//Volumes//GoogleDrive//My Drive//ShinyApps//LocalDataArchive//SocialMediaData//GEOG//")
          existingFiles <- dir(inDataPath)
          if(length(existingFiles)==0){
            cat(green("There are no social media files\n"))
            outData <- inData
          } else {
            
            existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
            existingFilesFullPath <- paste0(inDataPath, existingFiles)
            fileInfo <- file.info(existingFilesFullPath)
            if(force.create==TRUE){
              processFiles <- fileInfo
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
            } else {
              processFiles <- fileInfo %>%
                filter(mtime > rdsCreationDate)
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
            }
            #processFiles <- fileInfo
            outData <- inData
            #cat(green("before if\n"))
            #cat(green("Number of social media data files to process:", nrow(processFiles),"\n"))
            if(nrow(processFiles)>0){
              #cat(green("nrow(processFiles)>0\n"))
              keepNames <- names(outData)
              for(i in 1:dim(processFiles)[1]){
                
                message(paste("Reading", row.names(fileInfo)[i]))
                newSMData <- readSingleSocialMediaFile(row.names(fileInfo)[i])
                
                
                #################################################
                # Integrate the data from newSMData into inData #
                #################################################
                
                newSMData <- newSMData %>%
                  mutate(UIN=as.character(UIN))
                outData <- outData %>%
                  mutate(UIN=as.character(UIN)) %>%
                  left_join(newSMData, by=c("UIN")) 
                if("Facebook.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Facebook=case_when(!is.na(Facebook.y) ~ Facebook.y,
                                              TRUE ~ Facebook.x)) 
                }
                if("LinkedIn.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(LinkedIn=case_when(!is.na(LinkedIn.y) ~ LinkedIn.y,
                                              TRUE ~ LinkedIn.x)) 
                }
                if("Twitter.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Twitter=case_when(!is.na(Twitter.y) ~ Twitter.y,
                                             TRUE ~ Twitter.x)) 
                }
                if("Reddit.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Reddit=case_when(!is.na(Reddit.y) ~ Reddit.y,
                                            TRUE ~ Reddit.x)) 
                }
                if("Instagram.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Instagram=case_when(!is.na(Instagram.y) ~ Instagram.y,
                                               TRUE ~ Instagram.x))
                }
                if("ResearchGate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ResearchGate=case_when(!is.na(ResearchGate.y) ~ ResearchGate.y,
                                                  TRUE ~ ResearchGate.x))
                }
                if("Orcid.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Orcid=case_when(!is.na(Orcid.y) ~ Orcid.y,
                                           TRUE ~ Orcid.x))
                }
                if("Github.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Github=case_when(!is.na(Github.y) ~ Github.y,
                                            TRUE ~ Github.x))
                }
                
                ##############################################
                # Eliminate the new fields with .x and .y    #
                ##############################################
                #cat(green("Names to keep:", all_of(keepNames), "\n"))
                outData <- outData %>%
                  select(all_of(keepNames))
                
                # Here is where a new rds file needs to be written.
                #saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
              }
            }
          }
        }
        if(!("LinkedIn" %in% names(outData))){
          #cat(green("No DP fields in outData\n"))
          outData <- outData %>%
            mutate(LinkedIn=NA) %>%
            mutate(Facebook=NA) %>%
            mutate(Twitter=NA) %>%
            mutate(Reddit=NA) %>%
            mutate(Instagram=NA) %>%
            mutate(Orcid=NA) %>%
            mutate(ResearchGate=NA) %>%
            mutate(Github=NA) 
        } 
        
        outData
      }

      #########################################
      #                                       #
      #         Read faculty data             #
      #                                       #
      #########################################
      faculty.list <- read_excel(".//Data//faculty.xlsx")
      fileInventoryFilename <- c(fileInventoryFilename, "faculty.xlsx")
      fileInventoryDescription <- c(fileInventoryDescription, "Names of faculty with start and end dates.")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(".//Data//faculty.xlsx")$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, getMaxFaculty(faculty.list))
      
      assign("faculty.list", faculty.list, pos=1)
      cat(blue("Read faculty data\n"))
      txt <- "Read Faculty Data"
      containerId <- paste0("testScheduling-", "progressBoxContainer")
      insertUI(paste0("#", containerId), where = "beforeEnd",
               ui = tagList(
                 br(),
                 paste0(txt, "\n", collapse = "")))
      #########################################
      #                                       #
      #         Read SCH/WSCH data            #
      #                                       #
      #########################################
      sch <- read_excel(".//Data//SCH_WSCH_Data.xlsx") 
      fileInventoryFilename <- c(fileInventoryFilename, "SCH_WSCH_Data.xlsx")
      fileInventoryDescription <- c(fileInventoryDescription, "SCH/WSCH Data from DARS")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(".//Data//SCH_WSCH_Data.xlsx")$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(sch$Semester))
      
      assign("inputSCH", sch, pos=1)
      cat(blue("Read SCH/WSCH data\n"))
      
      
      #########################################
      #                                       #
      #   Read Annual Productivity data       #
      #                                       #
      #########################################
      annualProductivity <- readAnnualProductivityData(force.create=FALSE)
      
      # fileInventoryFilename <- c(fileInventoryFilename, "SCH_WSCH_Data.xlsx")
      # fileInventoryDescription <- c(fileInventoryDescription, "SCH/WSCH Data from DARS")
      # fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(".//Data//SCH_WSCH_Data.xlsx")$mtime, "%d-%b-%Y"))
      # fileInventoryMostRecent <- c(fileInventoryMostRecent, max(sch$Semester))
      
      assign("annualProductivity", annualProductivity, pos=1)
      cat(blue("Read Annual Productivity Data\n"))
      
      
      #########################################
      #                                       #
      #   Read Research Expenditure data      #
      #                                       #
      #########################################
      
      cat(yellow("[readInputDataServer]", green("before read.researchExpenditures.July2019\n")))
      expenditures <- read.researchExpenditures.July2019(raw.data.path = expenditures.path, 
                                                         rds.data.path=rds.path,
                                                         fileInventoryName,
                                                         fileInventoryDescription,
                                                         fileInventoryLastUpdate,
                                                         fileInventoryMostRecent)
      cat(yellow("[readInputDataServer]", green("after read.researchExpenditures.July2019\n")))
      t.directExpenditures <- read_excel(".//Data//Direct Expenditures_Detail_Org8438_20210914164840.xlsx")
      expendituresRaw <- t.directExpenditures
      t.IDC <- read_excel(".//Data//Indirect Expenditures_Detail_Org8438_20210914165616.xlsx")
      currentAccounts <- read_excel(".//Data//Current_Accounts.xlsx", skip=3)
      processExpendituresNew <- function(inData, inFaculty){
        midFaculty <- inFaculty %>%
          filter(faculty.long != "Minguez, Julie") %>%
          mutate(faculty.long=case_when(faculty.long=="Minguez, Julie" ~ "Loisel, Julie",
                                        TRUE ~ faculty.long)) %>%
          separate(faculty.long, into=c("Last.Name", "First.Name"), sep=",", remove=FALSE) %>%
          mutate(First.Name=str_trim(First.Name)) %>%
          separate(First.Name, into=c("First.Name", "Middle.Name"), sep=" ", remove=TRUE) 
        
        outData <- inData %>%
          mutate(Researcher=case_when(Researcher=="Minguez, Julie" ~ "Loisel, Julie",
                                      TRUE ~ Researcher)) %>%
          separate(Researcher, into=c("Last.Name", "First.Name"), sep=",", remove=FALSE) %>%
          separate(First.Name, into=c("First.Name", "Middle.Name"), sep=", ", remove=TRUE) %>%
          mutate(First.Name=str_trim(First.Name))
        
        outData1 <- outData %>%
          left_join(midFaculty, by=c("Last.Name"="Last.Name", "First.Name"="First.Name")) %>%
          group_by(faculty.short, `Calendar Year`) %>%
          summarize(sum=sum(Amount), .groups="drop") %>%
          arrange(`Calendar Year`) %>%
          pivot_wider(id_cols=faculty.short, names_from=`Calendar Year`, values_from=sum) %>%
          arrange(faculty.short) %>%
          rename(facultyName=faculty.short) %>%
          mutate(facultyName=case_when(is.na(facultyName) ~ "Non-GEOG Faculty",
                                      TRUE ~ facultyName))
        
        
        outData1
      }
      
      expendituresNew2021 <- processExpendituresNew(t.directExpenditures, faculty.list)
      saveRDS(expendituresNew2021, paste0(rds.path, "expendituresNew2021.rds"))
      saveRDS(expendituresRaw, paste0(rds.path, "expendituresRaw.rds"))
      saveRDS(currentAccounts, paste0(rds.path, "currentAccounts.rds"))
      idcNew2021 <- processExpendituresNew(t.IDC, faculty.list)
      
      assign("expendituresData", expenditures, pos=1)
      assign("expendituresNew2021", expendituresNew2021, pos=1)
      assign("expendituresRaw", expendituresRaw, pos=1)
      assign("currentAccounts", currentAccounts, pos=1)
      cat(blue("Read Research Expenditures\n"))
      
      
      idcData <- readIDCData(fileInventoryFilename, 
                             fileInventoryDescription,
                             fileInventoryLastUpdate)
      assign("idcData", idcData, pos=1)
      cat(blue("Read idcData\n"))

      fileInventoryMostRecent <- c(fileInventoryMostRecent, getMaxIDCDate(idcData))
      colorList <- createColorsList(expenditures, idcData)
      
      
      #clgeExpenditures <- read.researchExpenditures.CLGE(paste0(rds.path, "//CLGE_Expenditures//"))  #Need to fix all of the warnings that occur when this is called.
      
      #########################################
      #                                       #
      #   Read semester codes data            #
      #                                       #
      #########################################
      
      semester.codes <- readRDS(paste0(rds.path, "semester.codes.rds"))
      newSemesterCodes <- readRDS(paste0(rds.path, "new.semester.codes.rds"))
      assign("semester.codes", semester.codes, pos=1)
      assign("newSemesterCodes", newSemesterCodes, pos=1)
      cat(blue("Read Semester Codes\n"))
      
      #########################################
      #                                       #
      #   Read student flow data              #
      #                                       #
      #########################################
      
      #cat(red("before studentFlowData\n"))
      studentFlowData <- readRDS(paste0(rds.path, "student.flow.all.rds"))
      studentFlowData <- createStudentFlowDataNew(studentFlowData)  #This checks for new data
      fileInventoryFilename <- c(fileInventoryFilename, "student.flow.all.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Student Flow Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "student.flow.all.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, format(file.info(paste0(rds.path, "student.flow.all.rds"))$mtime, "%d-%b-%Y"))
      assign("studentFlowData", studentFlowData, pos=1)
      cat(blue("Read student flow data\n"))
      
      #############################################
      # Read Enrolled Student Data                #
      #############################################
      if(file.exists(paste0(rds.path, "enrolledStudents.rds"))){
        enrolledStudents <- readRDS(paste0(rds.path, "enrolledStudents.rds"))
      } else {
        enrolledStudents <- NULL
      }
      enrolledStudents <- readEnrolledStudents(inData=enrolledStudents, inDataPath=paste0(local.archive.path, "EnrolledStudentsData//"),
                                               outDataPath=rds.path, force.create=TRUE)
      
      assign("enrolledStudents", enrolledStudents, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "enrolledStudents.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Enrolled Students")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "enrolledStudents.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(enrolledStudents$TERM))
      
      rvFI$fileInventory <- data.frame(filename=fileInventoryFilename, description=fileInventoryDescription, updated=fileInventoryLastUpdate, recent=fileInventoryMostRecent)
      
      cat(blue("Read Enrolled Students Data\n"))
      
      #########################################
      #                                       #
      #   Read Active Student Data            #
      #                                       #
      #########################################
      
      #cat(red("before activeStudentsData\n"))
      activeStudentsData <- readActiveStudentData(force.create=FALSE)  # Only undergraduates.  Problems with double majors
      assign("t.asd", activeStudentsData, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "active.students.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Active Students Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "active.students.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(activeStudentsData$semester))
      cat(blue("Read active student data\n"))
      
      activeMinorsData <- readMinorsData(force.create=FALSE)
      assign("activeMinorsData", activeMinorsData, pos=1)
      assign("t.amd", activeMinorsData, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "all.minors.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Active Minors Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "all.minors.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(activeMinorsData$semester))
      
      cat(blue("Read active minors data\n"))
      
      
      activeGraduateStudentsData <- readActiveGraduateStudentData(force.create=FALSE)
      
      # Combine with enrolledStudents here to remove students who were admitted but did not enroll.
      
      
      assign("activeGraduateStudentsData", activeGraduateStudentsData, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "active.graduate.students.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Active Graduate Students Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "active.students.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(activeGraduateStudentsData$semester.numeric))
      
      enrolledStudents <- enrolledStudents %>%
        mutate(TERM=as.numeric(TERM))
      t.availableTERMS <- unique(enrolledStudents$TERM)
      t.incomparables <- activeGraduateStudentsData %>%
        filter(semester.numeric %out% t.availableTERMS)
      t.comparables <- activeGraduateStudentsData %>%
        filter(semester.numeric %in% t.availableTERMS) %>%
        mutate(semester.numeric = as.numeric(semester.numeric))
      
      
      t.semiTest <- t.comparables %>%
        semi_join(enrolledStudents, by=c("UIN"="TAMU_UIN", "semester.numeric"="TERM"))
      activeGraduateStudentsData <- rbind(t.incomparables, t.semiTest)
      assign("activeGraduateStudentsData", activeGraduateStudentsData, pos=1)
      #saveRDS(activeGraduateStudentsData, paste0)
      cat(blue("Read active graduate students data\n"))
      #########################################
      #                                       #
      #   Read Graduate Committee Data        #
      #                                       #
      #########################################
      
      graduateCommitteeMembershipHistorical <- createGraduateCommitteesHistorical(rds.data.path=rds.path, local.archive.path, 
                                                                                  "GraduateCommitteeData//", force.create=FALSE)
      assign("graduateCommitteeMembershipHistorical", graduateCommitteeMembershipHistorical, pos=1)
      assign("t.gch", graduateCommitteeMembershipHistorical, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "gradcommittees.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Graduate Committee Membership")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "gradcommittees.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(graduateCommitteeMembershipHistorical$Semester))
      
      cat(blue("Read graduate Committee Membership data\n"))
      #########################################
      #                                       #
      #   Read Graduation Data                #
      #                                       #
      #########################################
      
      fullGraduationData <- makeCompleteGraduateListing()
      assign("fullGraduationData", fullGraduationData, pos=1)
      assign("t.gd", fullGraduationData, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "all.graduated.students.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Graduation Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "all.graduated.students.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, format(max(readr::parse_datetime(as.character(fullGraduationData$GRAD_DATE), "%d-%b-%Y")), "%d-%b-%Y"))
      
      cat(blue("Read full graduation data\n"))
      #########################################
      #                                       #
      #   Read Teaching Evaluation Data       #
      #                                       #
      #########################################
      
      teachingEvaluationData <- readTeachingEvaluationData()
      teachingEvaluationData <- teachingEvaluationData %>%
        mutate(Instructor=case_when(
          Instructor=="Daniel Goldberg" ~ "Daniel W. Goldberg",
          Instructor=="Stacey Lyle" ~ "Stacey D. Lyle",
          TRUE ~ Instructor
        ))
      saveRDS(teachingEvaluationData, paste0(rds.path, "ted.rds"))
      assign("t.ted", teachingEvaluationData, pos=1)
      assign("teachingEvaluationData", teachingEvaluationData, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "all.teaching.evaluations.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Teaching Evaluation Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "all.teaching.evaluations.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(teachingEvaluationData$Term))
      
      cat(blue("Read teaching evaluation data\n"))
      #########################################
      #                                       #
      #   Create Faculty Evaluation Data      #
      #                                       #
      #########################################
      
      inYear <- as.numeric(format(today(tzone="UTC"), "%Y"))-1
      
      # available.faculty is read in from an RDS file over in GEOG_Scheduling
      # it contains rank information that is not present in faculty.list 
      # need to read it in here directly and add information about how up-to-date it is
      rankLUT <- data.frame(rankCode=c(1:13), rankTitle=c("Assistant Professor", "Associate Professor", "Professor",
                                                          "Visiting Assistant Professor", "Visiting Associate Professor", "Research Assistant Professor",
                                                          "Instructional Assistant Professor", "Instructional Associate Professor", "Instructional Professor",
                                                          "Visiting Professor", "Research Associate Professor", "Research Professor",
                                                          "Lecturer"))
      t.faculty.list <- readRDS("./Data/unknownFacultyList.rds")
      
      #cat(blue("before reviewFaculty\n"))
      #print(available.faculty)
      reviewFaculty <- available.faculty %>%
        pivot_longer(-Faculty, names_to="semester", values_to="rankCode") %>%
        #filter((semester==paste0(inYear, "A"))|(semester==paste0(inYear, "B"))|(semester==paste0(inYear,"C"))) %>%
        filter(semester==paste0(inYear, "C")) %>%
        filter(!is.na(rankCode)) %>%
        left_join(rankLUT, by="rankCode") %>%
        mutate(templateName=as.vector(unlist(fix.names(.$Faculty)))) %>%
        mutate(templateName=case_when(
          (templateName == "B. Guneralp") ~ "Guneralp, B.",
          (templateName == "I. Guneralp") ~ "Guneralp, I.",
          TRUE ~ templateName
        )) %>%
        mutate(facultyRating="Needs Rating") %>%
        mutate(includeTeaching=case_when(
          (rankCode %in% c(1,2,3,4,6, 7,8,10, 13)) ~ TRUE,
          TRUE ~ FALSE
        )) %>%
        mutate(includeResearch=case_when(
          (rankCode %in% c(1,2,3,4,6, 10)) ~ TRUE,
          TRUE ~ FALSE
        )) %>%
        mutate(includeService=case_when(
          (rankCode %in% c(1,2,3,6, 7,8,10)) ~ TRUE,
          TRUE ~ FALSE
        )) %>%
        mutate(includeGraduateSupervision=case_when(
          (rankCode %in% c(1,2,3, 6)) ~ TRUE,
          TRUE ~ FALSE
        )) %>%
        select("Faculty", "templateName", "rankTitle", "facultyRating", "includeTeaching", "includeResearch", "includeService", "includeGraduateSupervision") %>%
        mutate(fullName=sub("(\\w+),\\s(\\w+)","\\2 \\1", .$Faculty)) %>%
        mutate(fullName=case_when(
          fullName=="John Connors" ~ "John Casellas Connors",
          fullName=="C Brannstrom." ~ "Christian Brannstrom",
          TRUE ~ fullName
        )) %>%
        mutate(faculty.short=sub("(\\w+),\\s(\\S+)","\\2 \\1", .$templateName)) %>%
        mutate(faculty.short=case_when(
          faculty.short=="Connors" ~ "Casellas Connors",
          TRUE ~ faculty.short
        )) %>%
        mutate(fullName=case_when(
          fullName=="O'Kathleen Reilly" ~ "Kathleen O'Reilly",
          TRUE ~ fullName
        )) %>%
        left_join(t.faculty.list, by=c("faculty.short")) %>%
        mutate(faculty.teaching=case_when(
          fullName=="Bonnie Bounds" ~ "Bonnie E. Bounds",
          fullName=="Betsy Breyer" ~ "Elizabeth Breyer",
          fullName=="Lindsay Sansom" ~ "Lindsay C. Sansom",
          fullName=="Tony Filippi" ~ "Anthony Filippi",
          fullName=="Oliver Frauenfeld" ~ "Oliver W. Frauenfeld",
          fullName=="Stacey Lyle" ~ "Stacey D. Lyle",
          fullName=="Charles Lafon" ~ "Charles W. Lafon",
          fullName=="Brendan Roark" ~ "Erin Roark",
          fullName=="George Allen" ~ "George H. Allen",
          fullName=="John Casellas Connors" ~ "John P. Casellas Connors",
          fullName=="Michael Bishop" ~ "Michael P. Bishop",
          fullName=="Dan Goldberg" ~ "Daniel W. Goldberg",
          TRUE ~ fullName
        )) %>%
        mutate(faculty.committee=case_when(
          faculty.committee=="Allen, George" ~ "Allen, George H.",
          TRUE ~ faculty.committee
        )) %>%
        mutate(includeResearch = case_when(
          templateName=="Cai"~ TRUE,
          TRUE ~ includeResearch
        ))
      assign("reviewFaculty", reviewFaculty, pos=1)
      cat(blue("Processed faculty review data\n"))
      
      #########################################
      #                                       #
      #   Read Course Demographics Data and   #
      #   Enrollment Data                     #
      #                                       #
      #########################################
      courseDemographicsData <- readCLGECourseDemographicData()
      courseDemographicsData.V1 <- readRDS(".//Data//revised.enrollment.demographics.rds")
      
      if (showDebugCues==TRUE) cat(blue("here\n"))
      assign("courseDemographicsData", courseDemographicsData, pos=1)
      assign("courseDemographicsData.V1", courseDemographicsData.V1, pos=1)
      if (showDebugCues==TRUE) assign("t.cdd", courseDemographicsData, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "clge.enrollment.demographics.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Course Demographics Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "clge.enrollment.demographics.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(courseDemographicsData$semester))
      
      courseEnrollmentData <- createCourseEnrollments(courseDemographicsData)
      if (showDebugCues==TRUE) cat(blue("here2\n"))
      assign("courseEnrollmentData", courseEnrollmentData, pos=1)
      assign("t.ced", courseEnrollmentData, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "clge.enrollment.demographics.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Course Enrollment Data")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "clge.enrollment.demographics.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(courseEnrollmentData$Semester))
      
      fileInventory <- data.frame(filename=fileInventoryFilename, description=fileInventoryDescription, updated=fileInventoryLastUpdate, recent=fileInventoryMostRecent)
      rvFI$fileInventory <- data.frame(filename=fileInventoryFilename, description=fileInventoryDescription, updated=fileInventoryLastUpdate, recent=fileInventoryMostRecent)
      
      if (showDebugCues==TRUE) cat(blue("before processFileInventory\n"))
      fileInventory <- processFileInventory(fileInventory)
      rvFI$fileInventory <- processFileInventory(isolate(rvFI$fileInventory))
      if (showDebugCues==TRUE) cat(blue("after processFileInventory\n"))
      if(showDebugCues==TRUE) print(fileInventory)
      #assign("t.fileInventory", fileInventory, pos=1)
      #assign("t.ed", courseEnrollmentData, pos=1)
      cat(blue("Read course demographics data\n"))
      
      #########################################
      #                                       #
      #   Read Departmental Committees        #
      #                                       #
      #########################################
      
      committeesFilePath <- "..//GEOG_Committees//Data//CommitteeService.xlsx"
      tamuCommittees <- read_excel(committeesFilePath)
      assign("tamuCommittees", tamuCommittees, pos=1)
      #assign("t.univCommittees", tamuCommittees, pos=1)
      
      cat(blue("Read departmental committees\n"))
      
      #############################################
      # Create the fullRegistrationTracking.rds   #
      # database.                                 #
      #############################################
      createRegistrationTrackingDB <- function(){
        require(stringr)
        local.archive.path <- ".//Data//"
        rds.path <- ".//Data//"
        local.archive.subdirectory <- "202031"
        clge.enrollment.demographics <- readRDS(paste0(rds.path, "clge.enrollment.demographics.rds"))
        
        ################################################
        # update the clge.enrollment.demographics.rds  #
        #                                              #
        #                                              #
        ################################################

        
        fullEnrollmentTracking <- c(NULL)
        for(i in 1:dim(t.out1)[1]){
          #enrollmentData <- processEnrollmentFile2020C("./Data/", "202031", row.names(t.out1)[i], creationDateTime=t.out1$mtime[i], fileSize=t.out1$size[i])
          enrollmentData <- processEnrollmentFile2020C("./Data/", "202031", row.names(t.out1)[i], creationDateTime=t.out1$mtime[i], fileSize=t.out1$size[i])
          
          cat("past enrollmentData\n")
          if(is.null(fullEnrollmentTracking)){
            fullEnrollmentTracking <- enrollmentData
          } else {
            fullEnrollmentTracking <- rbind(fullEnrollmentTracking, enrollmentData)
          }
        }  
        fullEnrollmentTracking
      }  # Works for Fall 2020 Only!

      processBannerFiles <- function(inSemester, datesToProcess, fileStem="PWS_COURSE_BY_MAJ_CNTS_"){
        #cat("In processBannerFiles\n")
        padZero <- function(inData){
          outData <- NULL
          t.1 <- as.numeric(inData)
          for(i in 1:length(t.1)){
            if(t.1[i] <= 9){
              outData <- c(outData, paste0("0", t.1[i]))
            } else {
              outData <- c(outData, t.1[i])
            }
          }
          outData
        }
        ####################################
        # Convert dates to filenames       #
        ####################################
        #cat("fileStem:", fileStem, "\n")
        cat(yellow(year(datesToProcess)), "\n")
        cat(yellow(paste("month:", month(datesToProcess))), "\n")
        cat(yellow(paste("day:", day(datesToProcess))), "\n")
        theFileNames <- paste0(fileStem, year(datesToProcess), padZero(month(datesToProcess)), 
                               padZero(day(datesToProcess)), ".csv")
        #assign("dtp", datesToProcess, pos=1)
        
        #print(padZero(day(datesToProcess)))
        
        #theWD <- getwd()
        theWD <- "."
        #theSemester <- "202111"
        theSemester <- inSemester
        theFiles <- dir(paste0(theWD,"/Data/",theSemester))
        theFiles <- theFileNames
        numFiles <- length(theFiles)
        showDebugCues <- TRUE
        if(showDebugCues==TRUE) cat(green("In processBannerFiles\n"))
        if(showDebugCues==TRUE) cat(green("numFiles:", numFiles, "\n"))
        #numFiles <- 1
        keepSubjects <- c("GEOG", "ATMO", "GEOS", "OCNG", "GEOL", "GEOP")
        fullData <- NULL
        for(i in 1:numFiles){
          fileName <- theFiles[i]
          fileCreationDate <- substr(fileName, nchar(fileName)-11, nchar(fileName)-4)
          theYear <- substr(fileCreationDate,1,4)
          theMonth <- substr(fileCreationDate,5,6)
          theDay <- substr(fileCreationDate, 7,8)
          
          newDateTime <- paste0(fileCreationDate,"010101")
          newFileCreationDate <- paste0(theYear,"-",theMonth, "-", theDay)
          #cat(newDateTime,"\n")
          cat("Processing", fileName, "\n")
          newData <- read.csv(paste0("./Data/", theSemester, "/", fileName), row.names=NULL, encoding='UTF-8')
          #print(dim(newData))
          #if(nrow(newData==0) | is.null(newData)){
          if(dim(newData)[[1]]==0){
            #cat(blue("Empty File:", fileName,"\n"))
          } else {
            tempNames <- names(newData)
            newNames <- tempNames[-1]
            newData <- newData[-ncol(newData)]
            names(newData) <- newNames
            newData <- newData %>%
              mutate(subject=substr(COURSE,1,4)) %>%
              mutate(courseNumber=substr(COURSE,5,7)) %>%
              mutate(course.designation=paste(subject, courseNumber)) %>%
              mutate(course.designation1=paste0(subject,courseNumber,"-",SECT_NUM)) %>%
              mutate(dateCreated=newFileCreationDate) %>%
              mutate(dateCreated.1=ymd_hms(newDateTime)) %>%
              filter(subject %in% keepSubjects) %>%
              select(semester=TERM, subject, courseNumber, courseSection=SECT_NUM, 
                     major=MAJOR, course.designation, course.designation1,dateCreated, 
                     timeStamp=dateCreated.1, STUDENT_COUNT)
            if(dim(newData)[[1]]>0){
            newDataOldFormat <- NULL
            for(j in 1:nrow(newData)){
              for(k in 1:newData[j,"STUDENT_COUNT"]){
                newDataOldFormat <- rbind(newDataOldFormat, newData[j,])
              }
            }
            newDataOldFormat <- newDataOldFormat %>%
              select(semester, subject, courseNumber, courseSection, 
                     major, course.designation, course.designation1,dateCreated, 
                     timeStamp)
            fullData <- rbind(fullData, newDataOldFormat)
            } 
          }
        }
        fullData
      }
      
      processEnrollmentFile2020C <- function(local.archive.path, local.archive.subdirectory, inFileName, inDate){
        read.counts <- function(data){
          # intake one string with format "1-AGEC,3-GEOG," and convert
          # into a vector of majors AGEC, GEOG, GEOG, GEOG
          # that can be used to generate counts and further processed.
          #cat("String =", data, "\n")
          
          # There should be no more than 1 trailing , at the end of the string.
          # remove all but 1 trailing comma when there are more than 1
          
          i <- nchar(data)
          t.substr <- substr(data,i,i)
          while(t.substr == ","){
            i <- i-1
            t.substr <- substr(data, i,i)
          }
          data <- substr(data, 1, i+1)
          data1 <- paste(",",data, sep="")
          expanded.major <- c(NULL)
          length.of.string <- nchar(data1)
          locations.of.commas <- as.data.frame(str_locate_all(pattern =",", data1))
          locations.of.hyphens <- as.data.frame(str_locate_all(pattern="-", data1))
          num.majors <- dim(locations.of.commas)[1]
          major <- c(NULL)
          count <- c(NULL)
          for(i in 1:(num.majors-1)){
            major <- c(major, substr(data1,locations.of.commas[i+1,1]-4,locations.of.commas[i+1,1]-1))
            count <- c(count, substr(data1,locations.of.commas[i,1]+1,locations.of.hyphens[i,1]-1))
          }
          count <- as.numeric(count)
          #cat("major:", major, "count:", count, "\n")
          # change the output to be individual students instead of count
          # one line for each enrolled student.
          for(i in 1:length(count)){
            #cat(i, count[i], "\n")
            if(is.na(count[i])){
              cat("count is NA\n")
              cat(data, "\n")
            }
            if(is.null(count[i])) cat("count is NULL\n")
            expanded.major <- c(expanded.major, rep(major[i], count[i]))
          }
          # t.out <- data.frame(major=major, count=count)
          expanded.major
        }
        
        
        fullPath <- paste0(local.archive.path, local.archive.subdirectory, "//", inFileName)
        #print(file.info(fullPath))
        #creationDateTime <- file.info(fullPath)$mtime
        
        #cat(fullPath, "\n")
        #cat(format(creationDateTime, "%Y-%m-%d"), "\n")
        cat("Reading", fullPath, "\n")
        rawFile <- read.table(fullPath, header=FALSE, sep=";", stringsAsFactors = FALSE)
        cat("Read", fullPath, "\n")
        #columnNames <- strsplit(rawFile[i,], ",")
        columnNames <- strsplit(rawFile[1,], ",")
        rawFile <- rawFile[-1,]
        academicPeriod <- str_extract(rawFile, "[^,]+")
        rawFile <- gsub("[^,]*,(.*)", "\\1",rawFile)
        subject <- str_extract(rawFile, "[^,]+")
        rawFile <- gsub("[^,]*,(.*)", "\\1",rawFile)
        #subject <- str_extract(rawFile, "(?<=,)[^,]*(?=,)")
        courseNumber <- str_extract(rawFile, "[^,]+")
        rawFile <- gsub("[^,]*,(.*)", "\\1",rawFile)
        courseSection <- str_extract(rawFile, "[^,]+")
        enrollmentData <- gsub("[^,]*,(.*)", "\\1",rawFile)
        outputData <- data.frame(academicPeriod=academicPeriod, subject=subject, courseNumber=courseNumber, courseSection=courseSection, enrollment=enrollmentData)
        
        keepSubjects <- c("GEOG", "GEOS", "OCNG", "ATMO", "METR", "GEOL", "GEOP")
        outputData <- outputData %>%
          filter(subject %in% keepSubjects) %>%
          mutate(courseSection=as.numeric(as.character(courseSection))) %>%
          filter(!is.na(courseSection)) %>%
          filter(courseSection > 100)
        
        for(i in 1:dim(outputData)[1]){
          if(i ==1) finalOutputData <- c(NULL)
          semester <- outputData$academicPeriod[i]
          year <- as.numeric(substring(as.character(outputData$academicPeriod[i]),1,4))
          semester.out <- as.numeric(substring(as.character(outputData$academicPeriod[i]),5,5))
          subject <- outputData$subject[i]
          course.number <- outputData$courseNumber[i]
          section <- outputData$courseSection[i]
          major.counts <- as.character(outputData$enrollment[i])
          #cat(as.character(subject), as.character(course.number), section, major.counts, "\n")
          if(!is.na(major.counts)){
            expanded.majors <- read.counts(major.counts)
            temp.data <- data.frame(semester=semester, subject=subject, courseNumber=course.number,
                                    courseSection=section, major=expanded.majors, stringsAsFactors = FALSE)
            finalOutputData <- rbind(finalOutputData, temp.data)
          }
        }
        
        finalOutputData <- finalOutputData %>%
          mutate(course.designation=paste(subject, courseNumber)) %>%
          mutate(course.designation1=paste0(subject,courseNumber,"-",courseSection)) %>%
          #mutate(dateCreated=format(creationDateTime, "%Y-%m-%d")) %>%
          mutate(dateCreated=inDate) %>%
          mutate(timeStamp=paste(inDate, "01:01:01"))
        #mutate(timeStamp = creationDateTime)
        
        finalOutputData
      }
      createCombinedRegistrationTrackingDB <- function(inputDB){
        
        availableFiles <- list.files("./Data")
        fixActivelyEnrollingSemester <- function(inSemester, regTrackingData, demoData, inSemesterCodes){
          # remove the actively updating semester's data from demoData
          tempDemoData <- demoData %>%
            filter(semester != inSemester)
          t1 <- demoData %>%
            filter(semester == inSemester)
          
          # replace the actively updating semester's data in demoData with the data from regTrackingData
          tempRegTrackingData <- regTrackingData %>%
            filter(semester == inSemester) %>%
            filter(dateCreated==max(dateCreated)) %>%
            select("semester", "subject", "courseNumber", "courseSection", "major") %>%
            group_by(semester, subject, courseNumber, courseSection, major) %>%
            summarize(enrolledStudents=n(), .groups="drop") %>%
            left_join(inSemesterCodes, by=c("semester"="current")) 
          
          #harmonize the names of tempRegTrackingData with tempDemoData and bind
          tempRegTrackingData <- tempRegTrackingData %>%
            select("semester", "subject", "course.number"="courseNumber", "section"="courseSection",
                   "major", "Semester.1"="semester.chr", "year"="Year", "enrolledStudents")
          
          outData <- tempDemoData %>%
            rbind(tempRegTrackingData)
          outData
        }
        
        cat(red("fixActivelyEnrollingSemester created\n"))
        theOutput <- inputDB
        if(length(availableFiles)>0){
          
          fileModes <- file.mode(paste0("./Data/", availableFiles))
          #availableSemesters <- availableFiles[as.character(fileModes)=="700"]
          availableSemesters <- availableFiles[as.numeric(as.character(fileModes))>=700]
          warning("availableSemesters", print(fileModes), "\n")
          #cat("availableSemesters:", availableSemesters, "\n")
          
          for(i in 1:length(availableSemesters)){
            if(is.null(availableSemesters)) warning("availableSemesters is NULL\n")
            if(availableSemesters[i]=="202031"){
              #cat("Creating data for 202031\n")
              # special treatment for Fall 2020 because of file formats
              #cat("Fall 2020", availableSemesters[i], " ")
              # determine the latest date in the existing DB for this semester
              
              
              
              
              # determine the latest date in the folder
              rawDataFiles <- list.files(paste0("./Data/202031"))
              rawFileDates <- substr(rawDataFiles, nchar(rawDataFiles)-21, nchar(rawDataFiles)-14)
              maxRawFileDate <- max(rawFileDates)
              
              if(!is.null(inputDB)){
                fileDatesInDB <- inputDB %>%
                  filter(semester==availableSemesters[i]) %>%
                  select(dateCreated) %>%
                  unlist() %>%
                  as.vector()
                maxDateInDB <- max(fileDatesInDB)
                if(is.na(maxDateInDB)){
                  maxDateInDB <- "1900-01-01"
                }
                #cat(availableSemesters[i], maxDateInDB, "\n")
                datesToProcess <- setdiff(as.character(ymd(rawFileDates)), fileDatesInDB)
              } else {
                cat("DB does not exist!\n")
                datesToProcess <- ymd(rawFileDates)
                maxDateInDB <- "1900-01-01"
              }
              
              
              #print(datesToProcess)
              #cat("ymd(maxRawFileDate):", ymd(maxRawFileDate), "maxDateInDB:", maxDateInDB, "\n")
              if(ymd(maxRawFileDate)!=maxDateInDB){
                cat("Need to process the data\n")
                
                for(j in 1:length(datesToProcess)){
                  cat("Processing", datesToProcess[j], "\n")
                  theFileName <- paste0("Student_course_by_major Report_", format(ymd(datesToProcess[j]), "%Y%m%d"),
                                        "_Fall 2020.csv")
                  theOutput <- rbind(theOutput, processEnrollmentFile2020C("./Data/", "202031", theFileName, datesToProcess[j]))
                  
                }
              } else {
                
                if(showDebugCues==TRUE) cat("Fall 2020 Data is Up-to-Date.\n")
                #theOutput <- inputDB
              }
            } else {
              # regular treatment for semesters beginning Spring 2021 using Banner Reports
              #cat("Spring 2021", availableSemesters[i], "\n")
              
              # determine the latest date in the existing DB for this semester
              
              # if(is.na(maxDateInDB)){
              #   cat("There are no dates in the DB for this semester\n")
              # } else {
              #   cat(maxDateInDB, "\n")
              # }
              # determine the latest date in the folder
              rawDataFiles <- list.files(paste0("./Data/", availableSemesters[i]))
              rawFileDates <- ymd(substr(rawDataFiles, nchar(rawDataFiles)-11, nchar(rawDataFiles)-4))
              maxRawFileDate <- max(rawFileDates)
              cat(green(availableSemesters), "\n")
              
              # fileDatesInDB <- inputDB %>%
              #   filter(semester==availableSemesters[i]) %>%
              #   select(dateCreated) %>%
              #   unlist() %>%
              #   as.vector() 
              # maxDateInDB <- max(fileDatesInDB)
              if(!is.null(inputDB)){
                fileDatesInDB <- inputDB %>%
                  filter(semester==availableSemesters[i]) %>%
                  select(dateCreated) %>%
                  unlist() %>%
                  as.vector()
                maxDateInDB <- max(fileDatesInDB)
                minDateInDB <- min(fileDatesInDB)
                showDebugCues <- TRUE
                if(showDebugCues==TRUE) cat(red("The latest date in the DB is", maxDateInDB, "\n"))
                if(showDebugCues==TRUE) cat(red("The earliest date in the DB is", minDateInDB, "\n"))
                showDebugCues <- FALSE
                datesToProcess <- setdiff(as.character(ymd(rawFileDates)), fileDatesInDB)
                if(!is.null(minDateInDB)){
                  datesToProcess <- datesToProcess[datesToProcess > minDateInDB]
                }
                if(is.null(theOutput)){
                  cat("theOutput is NULL!\n")
                }
                #theOutput <- inputDB
              } else {
                cat("DB does not exist!\n")
                datesToProcess <- ymd(rawFileDates)
                maxDateInDB <- NA
                theOutput <- NULL
              }
              cat(green(" before the if\n"))
              if(!is.na(maxDateInDB)){
                #cat("Data for", availableSemesters[i], "are present in the DB.\n")
                datesToProcess <- setdiff(as.character(rawFileDates), fileDatesInDB)
                if(!is.null(minDateInDB)){
                  datesToProcess <- datesToProcess[datesToProcess > minDateInDB]
                }
                cat(green("Before determination of dates too early to process\n"))
                print(datesToProcess)
                # what is the earlies date in current semester that is being processed?
                
                if(length(datesToProcess)>0){
                  theOutput <- rbind(theOutput, processBannerFiles(availableSemesters[i], datesToProcess))
                  #############################
                  # theOutput now has updated registration tracking and demographic data in it.
                  # replace the demographic data in courseDemographicsData.V1 with the latest date 
                  # in datesToProcess list.  
                  #############################
                  courseDemographicsData.V1 <- fixActivelyEnrollingSemester(inSemester=availableSemesters[i], 
                                                          regTrackingData=theOutput,
                                                          demoData=courseDemographicsData.V1, 
                                                          inSemesterCodes=semester.codes)
                  cat(yellow("After courseDemographicsData.V1\n"))
                  assign("courseDemographicsData.V1", courseDemographicsData.V1, pos=1)
                  cat(red("saving courseDemographicsData.V1\n"))
                  saveRDS(courseDemographicsData.V1, ".//Data//revised.enrollment.demographics.rds")
                  
                } else {
                  theOutput <- theOutput
                }
                
                
              } else {
                cat("Data for", availableSemesters[i], "are NOT present in the DB.\n")
                datesToProcess <- as.character(rawFileDates)
                print(rawFileDates)
                theOutput <- rbind(theOutput, processBannerFiles(availableSemesters[i], datesToProcess))
                courseDemographicsData.V1 <- fixActivelyEnrollingSemester(inSemester=availableSemesters[i], 
                                                                          regTrackingData=theOutput,
                                                                          demoData=courseDemographicsData.V1, 
                                                                          inSemesterCodes=semester.codes)
                cat(yellow("After courseDemographicsData.V1\n"))
                assign("courseDemographicsData.V1", courseDemographicsData.V1, pos=1)
                cat(red("saving courseDemographicsData.V1\n"))
                saveRDS(courseDemographicsData.V1, ".//Data//revised.enrollment.demographics.rds")
              }
            }
          }
        }
        theOutput
      } 
      
      preProcessingComparison <- function(inData, inClassesStart, inDept){
        processData <- function(inData, inDept){
          outData <- inData %>%
            group_by(semester, subject, courseNumber, dateCreated) %>%
            summarize(semester, subject, courseNumber, dateCreated, nReg=n(), .groups="drop") %>%
            filter(subject==inDept) %>%
            unique() 
          outData
        }
        addStartDates <- function(inData, inStartDates){
          midData <- inData %>%
            left_join(inStartDates, by="semester") %>%
            mutate(temp1=difftime(dateCreated, startDate, units="days")) %>%
            mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
            mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                  TRUE ~ 1)) %>%
            mutate(daysBefore=temp2*sign)
          midData
        } 
        midData <- processData(inData, inDept)
        midData <- addStartDates(midData, inClassesStart)
        midData
      }
      
      #############################################
      # Check if fullRegistrationTracking.rds     #
      # exists.                                   #
      #############################################
      updateEnrollmentDemographics <- function(currentEnrollmentDemographics, registrationTracking,
                                               keepSubjects=c("ATMO", "GEOG", "GEOL", "GEOP", "GEOS", "OCNG"),
                                               inSemesterCodes=semester.codes){
        outData <- currentEnrollmentDemographics
        # Determine if additional semesters are available
        semestersInDB <- currentEnrollmentDemographics %>%
          select("semester") %>%
          unlist() %>%
          as.vector()
        availableFiles <- list.files("./Data")
        if(length(availableFiles)>0){
          fileModes <- file.mode(paste0("./Data/", availableFiles))
          semestersAvailable <- availableFiles[as.numeric(as.character(fileModes))>=700]
        }
        missingSemesters <- setdiff(semestersAvailable, semestersInDB)
        
        # for each semester that is missing from the enrollment demographics data,
        # pull the data for the latest available date.
        # convert data to the format used in the enrollment data frame
        # 
        print(missingSemesters)
        for(i in 1:length(missingSemesters)){
          cat(missingSemesters[i], "\n")
          singleSemesterDesignation <- missingSemesters[i]
          singleSemesterData <- registrationTracking %>%
            filter(semester==singleSemesterDesignation) %>%
            filter(dateCreated == max(dateCreated))
          cat("possibly adding", nrow(singleSemesterData), "records\n")
          reducedSingleSemesterData <- singleSemesterData %>%
            filter(subject %in% keepSubjects) %>%
            left_join(inSemesterCodes, by=c("semester"="current")) %>%
            select("semester", "subject", "course.number"="courseNumber",
                   "section"="courseSection", "major", "designation"="course.designation",
                   "designation1"="course.designation1", "year"="Year", "Semester.1"="semester.chr",
                   "full.semester"= "semester.display", "dateCreated") %>%
            mutate(Course=str_replace(designation, " ", ""))
          #print(dim(reducedSingleSemesterData))
          cat("adding", nrow(reducedSingleSemesterData), "records\n")
          #For a semester that is still accumulating data, need to remove the demographic
          #data for that semester from outData (currentEnrollmentDemographics) and replace it
          #with the reducedSingleSemesterData .
          #
          outData <- outData %>%
            rbind(reducedSingleSemesterData)
          
        }
        outData
      }
      
      if (file.exists("./Data/fullRegistrationTracking.rds")) {
        fullRegistrationTracking <- readRDS(".//Data//fullRegistrationTracking.rds")
        numRows <- dim(fullRegistrationTracking)[[1]]
        fullRegistrationTracking <- createCombinedRegistrationTrackingDB(fullRegistrationTracking)
        ppFullRegistrationTracking <- preProcessingComparison(fullRegistrationTracking, classesStart, inDept)
        newNumRows <- dim(fullRegistrationTracking)[[1]]
        if(newNumRows > numRows){
          #cat(yellow("Saving .//Data//fullRegistrationTracking.rds\n"))
          saveRDS(fullRegistrationTracking, paste0(".//Data//fullRegistrationTracking.rds"))
        }
      } else {
        fullRegistrationTracking <- NULL
        fullRegistrationTracking <- createCombinedRegistrationTrackingDB(fullRegistrationTracking)
        ppFullRegistrationTracking <- preProcessingComparison(fullRegistrationTracking, classesStart, inDept)
        saveRDS(fullRegistrationTracking, paste0(".//Data//fullRegistrationTracking.rds"))
      }

      #fullRegistrationTracking <- createCombinedRegistrationTrackingDB(fullRegistrationTracking)

      #saveRDS(fullRegistrationTracking, paste0(".//Data//fullRegistrationTracking.rds"))

      semester.codes <- readRDS(".//Data//semester.codes.rds")
      previousSemestersFinalEnrollment <- readRDS(".//Data//clge.enrollment.demographics.rds")
      # previousSemestersFinalEnrollment <- updateEnrollmentDemographics(previousSemestersFinalEnrollment,
      #                                       fullRegistrationTracking, 
      #                                       inSemesterCodes=semester.codes)
      saveRDS(previousSemestersFinalEnrollment, ".//Data//clge.enrollment.demographics.rds")
  
      assign("fullRegistrationTracking", fullRegistrationTracking, pos=1)
      assign("ppFullRegistrationTracking", ppFullRegistrationTracking, pos=1)
      assign("previousSemestersFinalEnrollment", previousSemestersFinalEnrollment, pos=1)

      cat(blue("Read full registration data\n"))
      
      #############################################
      # Read Personnel Data                       #
      #############################################
      inAdministrativeDeferals <- readRDS(".//Data//administrativeAppointments.rds")
      ptrCompletedDates <- readRDS(".//Data//completedPTR.rds")
      facultyNamesTemplate <- read_excel(".//Data//facultyNamesTemplate.xlsx")
      facultyLeaves <- readRDS(".//Data/facultyLeave.rds")
      facultyComposition <- read_excel(".//Data//Faculty Composition.xlsx")
      fileName <- "GEOG_Equity_Increases_and_Promotion_Increases_09182020.xlsx"
      fileName <- "20210802_GEOG_Faculty_Salaries_2000-2021.xlsx"
      salaryDataRawOld <- readxl::read_excel(paste0(".//Data//", fileName), sheet=1)
      col.types <- c("numeric", "text", "text", "text", "text", "text", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric")
      salaryDataRawNew <- readxl::read_excel(paste0(".//Data//", fileName), sheet=2, col_types=col.types)
      assign("salaryDataRawNew", salaryDataRawNew, pos=1)
      
      #############################################
      # Updated salaryDataRaw   8/1/2021          #
      #############################################
      
      t.out1 <- initialFileProcessing(salaryDataRawOld, facultyNamesTemplate, facultyComposition)
      t.out2 <- initialFileProcessing(salaryDataRawNew, facultyNamesTemplate, facultyComposition)
      
      #salaryDataProcessedNew <- salaryDataProcessingNew(t.out1, t.out2, facultyComposition, facultyNamesTemplate)
      
      #############################################################
      # 5/21/2022:                                                #
      # The tryCatch statement below does NOT allow for updating  #
      # the salaryDataProcessedNew object.  This functionality    #
      # needs to be written.  Argument passing into the personnel #
      # module is a mess.  salaryDataProcessedNew is referenced   #
      # within the module, but is not passed in, nor created in   #
      # the module.  It's an external dependency.  This needs to  #
      # be fixed.  It is messy, though.                           #
      #############################################################
      tryCatch(
        { 
          salaryDataProcessedNew <- readRDS("./Data/salary.rds")
        },
        warning = function(cond){
          salaryDataProcessedNew <- salaryDataProcessingNew(t.out1, t.out2, facultyComposition, facultyNamesTemplate)
        },
        finally = {
          #cat("here\n")
        }
      )
      
      #assign("salaryDataProcessed", salaryDataProcessed, pos=1)
      
      #############################################
      # End of update block                       #
      #############################################
      
      assign("inAdministrativeDeferals", inAdministrativeDeferals, pos=1)
      assign("ptrCompletedDates", ptrCompletedDates, pos=1)
      assign("facultyNamesTemplate", facultyNamesTemplate, pos=1)
      assign("facultyLeaves", facultyLeaves, pos=1)
      #assign("salaryDataRaw", salaryDataRaw, pos=1)
      assign("salaryDataProcessedNew", salaryDataProcessedNew, pos=1)
      
      cat(blue("Read Personnel Data\n"))
      
      #############################################
      # Read Graduate Program Data                #
      #############################################
      
      gradFunding <- readRDS(".//Data//gradFunding.rds")
      fundingOffers <- readRDS(".//Data//fundingOffers.rds")
      gradPublications <- readRDS(".//Data//publications.rds")
      gradPresentations <- readRDS(".//Data/presentations.rds")
      fullStudentData <- readRDS(".//Data//fullStudentData.rds")
      
      assign("gradFunding", gradFunding, pos=1)
      assign("fundingOffers", fundingOffers, pos=1)
      assign("gradPublications", gradPublications, pos=1)
      assign("gradPresentations", gradPresentations, pos=1)
     
      gdbGraduation <- fullGraduationData
      assign("gdbGraduation", gdbGraduation, pos=1)
     
      if (showDebugCues==TRUE) cat(red("before updateActiveGraduateStudents\n"))
      
      fullStudentData <- updateActiveGraduateStudents(inData=fullStudentData, inActiveStudents=activeGraduateStudentsData,
                                              inGraduationData = gdbGraduation)
      
      # The code below is to ensure that only enrolled students are considered active.  Otherwise some admitted students
      # who never enroll are considered active.
      #
      
      enrolledStudents <- enrolledStudents %>%
        mutate(TERM=as.numeric(TERM)) %>%
        mutate(TAMU_UIN=as.character(TAMU_UIN))
      t.availableTERMS <- unique(enrolledStudents$TERM)
      t.incomparables <- fullStudentData %>%
        filter(semester.numeric %out% t.availableTERMS)
      t.comparables <- fullStudentData %>%
        filter(semester.numeric %in% t.availableTERMS) %>%
        mutate(semester.numeric = as.numeric(semester.numeric))
      
      
      t.semiTest <- t.comparables %>%
        semi_join(enrolledStudents, by=c("UIN"="TAMU_UIN", "semester.numeric"="TERM"))
      fullStudentData <- rbind(t.incomparables, t.semiTest)
      assign("fullStudentData", fullStudentData, pos=1)
      filename <- paste0(rds.path, "fullStudentData.rds")
      saveRDS(fullStudentData, filename)
      cat(blue("Read graduate program data\n"))
      assign("fullStudentData", fullStudentData, pos=1)
      
      #############################################
      # Integrate Updated Degree Progress Data    #
      #############################################
      
      fullStudentData <- integrateStudentMilestoneData(inData=fullStudentData, inDept=inDept, force.create=FALSE,
                                             inDataPath = paste0(local.archive.path, "DegreePlanData//"),
                                             outDataPath = rds.path)
      assign("fullStudentData", fullStudentData, pos=1)
      if(inDept=="GEOG"){
        filename <- paste0(rds.path, "fullStudentData.rds")
      } else {
        filename <- paste0(rds.path, "fullStudentData", inDept, ".rds")
      }
      saveRDS(fullStudentData, filename)
      cat(blue(paste0("Updated degree progress data for ", inDept, "\n")))
      
      #############################################
      # Read Graduate Application Data            #
      #############################################
      if(file.exists(paste0(rds.path, "applicants.rds"))){
        graduateApplicants <- readRDS(paste0(rds.path, "applicants.rds"))
      } else {
        graduateApplicants <- NULL
      }
      graduateApplicants <- readApplicantInfo(inData=graduateApplicants, inDataPath=paste0(local.archive.path, "ApplicantData//"),
                               outDataPath=rds.path, force.create=TRUE)
      assign("graduateApplicants", graduateApplicants, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "applicants.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Graduate Applications")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "applicants.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(graduateApplicants$TERM))

      rvFI$fileInventory <- data.frame(filename=fileInventoryFilename, description=fileInventoryDescription, updated=fileInventoryLastUpdate, recent=fileInventoryMostRecent)
      
      cat(blue("Read Graduate Applicants Data\n"))
      
      
      #############################################
      # Read Student Supervisory Committee Data   #
      #############################################
      
      if(file.exists(paste0(rds.path, "studentSupervisoryCommittees.rds"))){
        studentSupervisoryCommittees <- readRDS(paste0(rds.path, "studentSupervisoryCommittees.rds"))
      } else {
        studentSupervisoryCommittees <- NULL
      }
      
      studentSupervisoryCommittees <- readStudentSupervisoryCommittees(inData=studentSupervisoryCommittees, inDept="GEOG", inDataPath=paste0(local.archive.path, "StudentSupervisoryCommitteeData//"),
                                                                       outDataPath=rds.path)
      
      assign("studentSupervisoryCommittees", studentSupervisoryCommittees, pos=1)
      fileInventoryFilename <- c(fileInventoryFilename, "studentSupervisoryCommittees.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Student Supervisory Committees")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "studentSupervisoryCommittees.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(enrolledStudents$TERM))
       
      rvFI$fileInventory <- data.frame(filename=fileInventoryFilename, description=fileInventoryDescription, updated=fileInventoryLastUpdate, recent=fileInventoryMostRecent)
       
      cat(blue("Read Student Supervisory Committees Data\n"))
      

    }
  )    
}

##########################################
# when are the following modules used?   #
##########################################
readInputDataNewUI <- function(id, label = "Read Input") {
  ns <- NS(id)
  tagList(
    div(id=ns("test"), class='dashboardPlotCard',
        tagList(
          uiOutput(ns("plotTitle")),
          div(id=ns("container1"), class="plotControlsAndBody",
              div(id=ns("controlsBanner"), class="bannerDivClass",
                  uiOutput(ns("bannerTest"))),
              div(id=ns("fileInfoContainer"), class="fileInfoContainer",
                  dataTableOutput(ns("fileInfoDT"))
              )
          )
        )
    )
  )
}

readInputDataNewServer <- function(id, inDept) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #########################
      # Output Variables      #
      #########################
      local.path <- "//Volumes//GoogleDrive//My Drive//"
      local.archive.path <- paste0(local.path, "ShinyApps//LocalDataArchive//")
      expenditures.path <- paste0(local.archive.path, "GEOGResearchExpenditures//")
      rds.path <- ".//Data//"
      `%out%` <- function(a,b) ! a %in% b
      fileInventoryLastUpdate <- c(NULL)
      fileInventoryFilename <- c(NULL)
      fileInventoryDescription <- c(NULL)
      fileInventoryMostRecent <- c(NULL)
      showDebugCues <- FALSE
      rvFI <- reactiveValues(fileInventory=NULL)
      outputCollection <- NULL
      useDept <- inDept
      
      PreviousSemester <- function(current.semester, semester.codes, numeric=FALSE, digits=4){
        if (numeric==TRUE){
          t.1 <- semester.codes %>%
            filter(current==current.semester) %>%
            select(previous)
        } else {
          t.1 <- semester.codes %>%
            filter(current==current.semester) %>%
            select(grads)
        }
        t.1
      }
      CurrentSemester <- function(numeric=FALSE, digits=4){
        t.year <- year(Sys.Date())
        year.digit <- t.year
        
        two.digit.year <- year.digit %% 1000
        t.c <- paste(t.year,"-12-15", sep="")
        t.b <- paste(t.year,"-8-25", sep="")
        t.a <- paste(t.year,"-5-15", sep="")
        current.date <- as.character(Sys.Date())
        if(digits==4) {paste.year <- year.digit}
        if(digits==2) {paste.year <- two.digit.year}
        if(numeric){
          paste.year <- year.digit
          if(current.date < t.a) {
            t.letter <- "11"
          } else if (current.date < t.b) {
            t.letter <- "21"
          } else t.letter <- "31"
          current.semester1 <- paste(paste.year, t.letter, sep="") 
          current.semester1 <- as.numeric(current.semester1)
        } else{
          if(current.date < t.a) {
            t.letter <- "A"
          } else if (current.date < t.b) {
            t.letter <- "B"
          } else t.letter <- "C"
          current.semester1 <- paste(paste.year, t.letter, sep="")    
        }
        current.semester1
      }
      #########################
      # Input Control UIs     #
      #########################
      output$fileInfoDT <- renderDataTable({
        isolate(rvFI$fileInventory) %>%
          datatable(options=list(dom='t', ordering=F, pageLength=50), rownames=FALSE)
      })
      
      ############################################
      # Graduate Application Functions           #
      ############################################
      
      readApplicantInfo <- function(inData, force.create=FALSE, inDataPath, outDataPath){
        keepDepartments <- c("GEOG", "OCNG", "ATMO", "GEPL")
        cat(yellow("[readInputDataNewServer]"), green("keepDepartments:", keepDepartments, "\n"))
        if(is.null(inData)|force.create==TRUE){
          # read all of the files available
          outData <- inData
          existingFiles <- dir(inDataPath)
          #eliminate locked files from the list
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          fileInfo <- file.info(existingFilesFullPath)
          for(i in 1:nrow(fileInfo)){
            tryCatch(
              { 
                cat("Trying to read:", row.names(fileInfo)[i], "\n")
                partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                  data.frame()
              },
              error = function(cond){
                message("Data are not an excel file.  Reading it as a .csv file.")
                partialData <<- read.csv(row.names(fileInfo)[i], na.strings="") %>%
                  rename("DECISION DATE"="DECISION.DATE", "FEE PAID"="FEE.PAID",
                         "COMPLETE DATE"="COMPLETE.DATE", "SENT DEPT DATE"="SENT.DEPT.DATE",
                         "GRE-Official"="GRE.Official", "GR-V"="GR.V","GR-Q"="GR.Q",
                         "GR-W"="GR.W", "TOEP-Official"="TOEP.Official") %>%
                  data.frame() 
                
              },
              finally = {
                #cat("here\n")
              }
            )
            
           
            partialDataTest <- partialData %>%
              select("TERM", "ADMT_CODE", "DEPARTMENT", "MAJOR", "PROGRAM", "UIN", "LASTNAME",
                     "FIRSTNAME", "MIDDLENAME", "DECISION_CODE",
                     "ETHDesc", "GN", "RS", "DECISION.DATE", "APPL_DATE") %>%
              mutate(ETHDesc=str_trim(ETHDesc)) %>%
              filter(DEPARTMENT %in% keepDepartments) %>%
              #filter(DEPARTMENT == "GEOG") %>%
              #filter(MAJOR != "WMHS") %>%
              mutate(degreeSought=str_extract(PROGRAM, "(?<=)(.*?)(?=\\-)")) %>%
              mutate(DECISION.DATE=as.POSIXct.default(DECISION.DATE, tryFormat=("%m/%d/%Y"))) %>%
              mutate(APPL_DATE=as.POSIXct.default(APPL_DATE, tryFormat="%m/%d/%Y"))
            cat(blue("before outData\n"))
            outData <- rbind(outData, partialDataTest) %>%
              unique()
          }
          saveRDS(outData, paste0(outDataPath, "applicants.rds"))
        } else {
          cat(yellow("[readInputDataNewServer]"),red("File exists (applicants.rds) \n"))
          rdsCreationDate <- file.info(paste0(outDataPath, "applicants.rds"))$mtime
          existingFiles <- dir(inDataPath)
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          assign("t.effp", existingFilesFullPath, pos=1)
          cat(red("created t.effp.  rdsCreationDate:", rdsCreationDate, "\n"))
          fileInfo <- file.info(existingFilesFullPath)
          processFiles <- fileInfo %>%
            filter(mtime > rdsCreationDate)
          outData <- inData
          #print(processFiles)
          cat(red("Need to process", nrow(processFiles), "files.\n"))
          if(nrow(processFiles)>0){
            for(i in 1:dim(processFiles)[1]){
              tryCatch(
                { 
                  cat("Trying to read:", row.names(fileInfo)[i], "\n")
                  partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                    data.frame()
                },
                error = function(cond){
                  message("Data are not an excel file.  Reading it as an .csv file.")
                  partialData <<- read.csv(row.names(fileInfo)[i], na.strings="") %>%
                    rename("DECISION DATE"="DECISION.DATE", "FEE PAID"="FEE.PAID",
                           "COMPLETE DATE"="COMPLETE.DATE", "SENT DEPT DATE"="SENT.DEPT.DATE",
                           "GRE-Official"="GRE.Official", "GR-V"="GR.V","GR-Q"="GR.Q",
                           "GR-W"="GR.W", "TOEP-Official"="TOEP.Official") %>%
                    data.frame() 
                  
                },
                finally = {
                  #cat("here\n")
                }
              )
              
              partialDataTest <- partialData %>%
                select("TERM", "ADMT_CODE", "DEPARTMENT", "MAJOR", "PROGRAM", "UIN", "LASTNAME",
                       "FIRSTNAME", "MIDDLENAME", "DECISION_CODE",
                       "ETHDesc", "GN", "RS", "DECISION.DATE", "APPL_DATE") %>%
                mutate(ETHDesc=str_trim(ETHDesc)) %>%
                filter(DEPARTMENT %in% keepDepartments) %>%
                #filter(DEPARTMENT == "GEOG") %>%
                #filter(MAJOR != "WMHS") %>%
                mutate(degreeSought=str_extract(PROGRAM, "(?<=)(.*?)(?=\\-)")) %>%
                mutate(DECISION.DATE=as.POSIXct.default(DECISION.DATE, tryFormat=("%m/%d/%Y"))) %>%
                mutate(APPL_DATE=as.POSIXct.default(APPL_DATE, tryFormat="%m/%d/%Y"))
              
              outData <- rbind(outData, partialDataTest) %>%
                unique()
              # check for duplicates.
              
              # Here is where a new rds file needs to be written.
              saveRDS(outData, paste0(outDataPath, "applicants.rds"))
            }
          }
        }
        outData
      }
      
      ############################################
      # Student Supervisory Committee Functions  #
      ############################################
      
      readStudentSupervisoryCommittees <- function(inData, inDept, force.create=FALSE, inDataPath, outDataPath){
        outData <- inData
        
        readSingleCommitteeFile <- function(inFileName){
          #################################################
          # Establish a file connection.  Then read each  #
          # line of the file into a separate element of a #
          # vector with number of elements equal to the   #
          # number of lines. Tidy up by unlinking the     #
          # connection.                                   #
          #################################################
          #cat(green(inFileName, "\n"))
          
          fil <- file(inFileName)
          t.out <- readLines(fil, n=-1)
          #cat(green(t.out, "\n"))
          unlink(fil)
          close(fil)
          
          ##################################################
          # Process each line.  The first line will        #
          # establish the separator for the file.          #
          # The current term is in the parameters at the   #
          # end of the file.                               #
          #                                                #
          ##################################################
          
          theSeparator <- "|"
          t.outNames <- unlist(str_split(t.out[2], fixed(theSeparator)))
          #cat(t.outNames, "\n")
          t.outData <- str_split(t.out[3:length(t.out)], fixed(theSeparator))
          
          shortLines <- NULL
          for(i in 3:length(t.outData)){
            if(length(unlist(t.outData[i])) != length(t.outNames)){
              #cat("i:", i, "length:", length(unlist(t.outData[i])), "\n")
              shortLines <- c(shortLines, i)
              
            }
          }
          # remove the shortLines from the data
          allIndices <- c(1:length(t.outData))
          goodIndices <- allIndices %out% shortLines
          t.outDataGood <- t.outData[goodIndices]
          
          # subset out the parameter data
          t.parameterData <- t.outData[shortLines]
          #print(t.parameterData)
          #cat("after t.parameterData\n")
          fixedParameterData <- NULL
          for(i in 1:length(t.parameterData)){
            #cat("in the loop\n")
            if(length(t.parameterData[[i]]) > 1){
              #cat("setting fixedParameterData\n")
              fixedParameterData <- c(fixedParameterData, t.parameterData[i])
            }
          }
          #cat("Before fixedParameterData\n")
          #print(fixedParameterData)
          theTerm <- fixedParameterData[[1]][3]
          #cat("After fixedParameterData\n")
          #cat(red(theTerm, "\n"))
          
          # convert the good data into a matrix
          unlistedData <- unlist(t.outDataGood)
          theMatrix <- matrix(unlist(t.outDataGood), ncol=length(t.outNames), byrow=TRUE)
          # for(i in 3:length(t.out)){
          #   if(i==1){
          #     #Determine the separator
          #     theSeparator="|"
          #   }
          #   if(i==2){
          #     #setup variable names
          #     t.outSplit <- str_split(t.out, fixed(theSeparator))
          #   }
          #   if(i >2) {
          #     #process individual lines of data
          #   
          #   }
          # }
          
          #cat(names(partialData), "\n")
          
          
          #outData <- rbind(outData, partialData)
          theDataFrame <- data.frame(theMatrix)
          names(theDataFrame) <- t.outNames
          theDataFrame %>%
            mutate(TERM=theTerm)
        }
        
        if(is.null(inData)|force.create==TRUE){
          # read all of the files available
          existingFiles <- dir(inDataPath)
          #eliminate locked files from the list
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          fileInfo <- file.info(existingFilesFullPath)
          
          for(i in 1:nrow(fileInfo)){
            #for(i in 1:1){
            tryCatch(
              {
                message(paste("Reading", row.names(fileInfo)[i]))
                partialData <<- readSingleCommitteeFile(row.names(fileInfo)[i]) %>%
                  select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                         "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") 
                
              },
              error = function(cond){
                cat("Trying to read:", row.names(fileInfo)[i], "\n")
                partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                  select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                         "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") %>%
                  data.frame()
              },
              finally = {
                #cat("here\n")
              }
            )
            cat("nrow(partialData):", nrow(partialData), "\n")
            outData <- rbind(outData, partialData) 
          }
          saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
        } else {
          #cat(red("File exists\n"))
          rdsCreationDate <- file.info(paste0(outDataPath, "studentSupervisoryCommittees.rds"))$mtime
          existingFiles <- dir(inDataPath)
          existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
          existingFilesFullPath <- paste0(inDataPath, existingFiles)
          fileInfo <- file.info(existingFilesFullPath)
          processFiles <- fileInfo %>%
            filter(mtime > rdsCreationDate)
          outData <- inData
          if(nrow(processFiles)>0){
            for(i in 1:dim(processFiles)[1]){
              tryCatch(
                { 
                  message(paste("Reading", row.names(fileInfo)[i]))
                  partialData <<- readSingleCommitteeFile(row.names(fileInfo)[i]) %>%
                    select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                           "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") 
                  
                },
                error = function(cond){
                  cat("Trying to read:", row.names(fileInfo)[i], "\n")
                  partialData <- readxl::read_excel(row.names(fileInfo)[i])  %>%
                    select("UIN"="STU_UIN", "STUDENTNAME"="STU_NAME", "DEPT"="PRIM_DEPT", "PROG"="PRIM_PROG", "DEGC"="PRIM_DEGC", "MAJR"="PRIM_MAJR",
                           "FACULTYNAME"="FAC_NAME", "COMM.TYPE"="COMM_TYPE", "ROLE"="COMM_ROLE", "FAC.COLL.DEPT"="FAC_COLL_DEPT", "TERM") %>%
                    data.frame()
                  
                },
                finally = {
                  #cat("here\n")
                }
              )
              
              outData <- rbind(outData, partialDataTest)
              
              # Here is where a new rds file needs to be written.
              #saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
            }
          }
        }
        
        outData
      }
      
      ############################################
      # Full Student Data Functions              #
      ############################################
      
      createFullDataAnyDepartment <- function(inDept="OCNG", 
                                              inEnrolledStudents=NULL, semester.codes=semester.codes,
                                              force.create=FALSE, local.archive.path){
        #########################################
        # Create the fullStudentData equivalent #
        # for any department in the CLGE        #
        #                                       #
        #########################################
        
        #########################################
        # GEOG is special because we have older #
        # data and are not dependent upon       #
        # the enrolled student data as a        #
        # scaffold for the other data.          #
        # inEnrolledStudents however will       #
        # contain any new students and needs to #
        # be integrated with the legacy GEOG    #
        # data.                                 #
        #########################################
        outDataPath <- ".//Data//"
        verboseFlag <- TRUE
        useDept <- inDept
        cat("useDept:", useDept, "\n")
        ##########################################
        # Remove inData as an input.   Read inData
        # from .rds file.  Use force.create to build
        # data file from scratch.  Or if the .rds file 
        # does not exist.
        #
        
        if(inDept=="GEOG"){
          outFileName <- "fullStudentData.rds"
          if(file.exists(paste0(outDataPath, outFileName))) {
            outData <- readRDS(paste0(outDataPath, outFileName))
            inData <- outData
          } else {
            outData <- NULL
            inData <- outData
          }
        } else {
          # all other departments
          outFileName <- paste0("fullStudentData", inDept, ".rds")
          if(file.exists(paste0(outDataPath, outFileName))){
            outData <- readRDS(paste0(outDataPath, outFileName))
            inData <- outData
          } else {
            outData <- NULL
            inData <- outData
          }
        }
        
        # if(inDept=="GEOG"){
        #   if(is.null(inData)){
        #     outData <- readRDS(".//Data//fullStudentData.rds")
        #     #cat(red("Geography data must not have inData set to NULL.\n"))
        #     #outData <- NULL
        #   } else {
        #     outData <- inData
        #   }
        # } else {
        if(force.create | (is.null(outData))){
          outData <- inEnrolledStudents %>%
            filter(DEPARTMENT==inDept) %>%
            mutate(NAMES=case_when(is.na(MIDDLE_NAME) ~ paste0(LAST_NAME, ", ", FIRST_NAME),
                                   TRUE ~ paste0(LAST_NAME, ", ", FIRST_NAME, " ", MIDDLE_NAME))) %>%
            select("NAMES", "UIN"="TAMU_UIN", "CLASSFICATION"="STUDENT_CLASSIFICATION", 
                   "semester.numeric"="TERM",
                   "LastName"="LAST_NAME", "FirstName"="FIRST_NAME", "MiddleName"="MIDDLE_NAME",
                   "termAdmitted"="FIRST_TERM_GR", "degreeSought"="DEGREE", "MAJR"="MAJOR",
                   "FIRST_CONCENTRATION", "COLLEGE") %>%
            mutate(PROGRAM1_1ST_DEG=paste0(COLLEGE,"-",degreeSought)) %>%
            mutate(PROGRAM1_1ST_DEG=case_when(FIRST_CONCENTRATION != "" ~ paste0(PROGRAM1_1ST_DEG, "-", FIRST_CONCENTRATION),
                                              TRUE ~ paste0(PROGRAM1_1ST_DEG))) %>%
            mutate(Email=NA) %>%
            mutate(X=NA) %>%
            mutate(Photo=NA) %>%
            mutate(semester.numeric=as.character(semester.numeric)) %>%
            left_join(semester.codes, by=c("semester.numeric"="current")) %>%
            select("NAMES", "UIN", "CLASSFICATION", "semester.numeric", "LastName", "FirstName", "MiddleName", "termAdmitted",
                   "degreeSought", "MAJR", "FIRST_CONCENTRATION", "COLLEGE", "PROGRAM1_1ST_DEG", "Email", "X", "Photo", "semester.abbrv"="current.code")
          
        }
        outData <- integrateStudentMilestoneData(inData=outData, inDept=inDept, force.create=force.create,
                                                 inDataPath = paste0(local.archive.path, "DegreePlanData//"),
                                                 outDataPath = ".")
        
        outData <- integrateSocialMediaData(inData=outData, inDept=useDept, force.create=force.create,
                                            inDataPath = paste0(local.archive.path, "SocialMediaData//"),
                                            outDataPath = ".")
        
        outData <- integrateGraduationData(inData=outData, inDept=useDept, 
                                           force.create=force.create, inDataPath=local.archive.path)
        
        outData <- outData %>%
          select("NAMES", "UIN", "CLASSFICATION", "PROGRAM1_1ST_DEG", "semester.abbrv",
                 "semester.numeric", "LastName", "FirstName", "MiddleName", "Email",
                 "termAdmitted", "X", "Photo", "LinkedIn", "Facebook",
                 "degreeSought", "GRAD_DATE", "STUDENT", "MAJR", "calendar.year",
                 "fiscal.year", "semester", "DateSubmitted", "DateApprovedByFaculty", "ProposalDefense", "ProposalSubmitDate", 
                 "ResidencyReq", "PreliminaryExamDate", "PreliminaryExamResult", "ThesisDefenseDate",
                 "ThesisSubmitDate", "Twitter", "Reddit", "Instagram", "ResearchGate", "Orcid", "Github") %>%
          unique()
        
        if(!is.null(inData)){
          if(!isTRUE(all_equal(inData, outData))){
            if (verboseFlag) cat(green("Will write new fullStudentData.rds\n"))
            if(inDept=="GEOG"){
              outFileName <- "fullStudentData.rds"
            } else {
              outFileName <- paste0("fullStudentData", inDept, ".rds")
            }
            saveRDS(outData, paste0(outDataPath, outFileName))
          } else {
            if (verboseFlag) cat(blue("No updates to fullStudentData.rds\n"))
          }
        } else {
          if (verboseFlag) cat(green("Created new fullStudentData.  Needs to be written.  Started as NULL.\n"))
          if(inDept=="GEOG"){
            outFileName <- "fullStudentData.rds"
          } else {
            outFileName <- paste0("fullStudentData", inDept, ".rds")
          }
          saveRDS(outData, paste0(outDataPath, outFileName))
        } 
        outData
      }
      
      makeCompleteGraduateListing <- function(force.create=FALSE, inDept="GEOG", local.archive.path){
        
        ########################################
        # No need for separate directories for #
        # different departments because data   #
        # files are entire university.         #
        ########################################
        outDataPath <- ".//Data//"
        needsUpdating <- FALSE
        path <- paste0(local.archive.path, "GraduationData//")
        t.combined <- NULL
        maxSemesterGraduated <- NULL
        maxSemesterGraduatedInRDS <- NULL
        t.dir <- dir(path)
        sem.codes <- substring(t.dir, nchar(t.dir)-6, nchar(t.dir)-4)
        maxSemesterInFiles <- max(sem.codes)
        
        if(inDept=="GEOG"){
          rdsFilename <- "all.graduated.students.rds"
        } else {
          rdsFilename <- paste0("all.graduated.students", inDept, ".rds")
        }
        if(file.exists(paste0(rds.path, rdsFilename))){
          t.combined <- readRDS(paste0(rds.path, rdsFilename))
          assign("t.combined", t.combined, pos=1)
          maxSemesterGraduatedInRDS <- t.combined %>%
            mutate(two.digit=as.character(calendar.year)) %>%
            mutate(two.digit=substring(two.digit, 3,4)) %>%
            mutate(two.digit.code = paste0(two.digit, semester)) %>%
            select("two.digit.code") %>%
            unlist() %>%
            as.vector() %>%
            max()
        }
        #cat("maxSemesterGraduatedInRDS:", maxSemesterGraduatedInRDS, "maxSemesterInFiles:", maxSemesterInFiles, "\n")
        if(is.null(maxSemesterGraduatedInRDS)) {
          needsUpdating <- TRUE
        } else if(maxSemesterInFiles > maxSemesterGraduatedInRDS){
          needsUpdating <- TRUE
        }
        mostRecentSemesterInRDS <- maxSemesterGraduated
        target.semester <- PreviousSemester(CurrentSemester(numeric=TRUE), semester.codes, numeric=FALSE)
        
        sem.codes <- substring(t.dir, nchar(t.dir)-6, nchar(t.dir)-4)
        #force.create=FALSE
        if(inDept=="GEOG"){
          keepMajors <- c("GEOG", "GIST", "SPSG", "USGE", "GEOS")
        }
        if(inDept=="OCNG"){
          keepMajors <- c("OCNG", "OCST")
        }
        if(inDept=="ATMO"){
          keepMajors <- c("METR", "ATMO")
        }
        if(inDept=="GEPL"){
          keepMajors <- c("GEOP", "GEOL")
        }
        if(inDept=="ENVP"){
          keepMajors <- c("ENGS", "ENST")
        }
        
        #cat(green("needsUpdating:", needsUpdating, "force.create:", force.create, "\n"))
        
        if((needsUpdating == TRUE) | (force.create==TRUE)){
          if (showDebugCues==TRUE) cat("Here I am!!!!!!!!!!!!!!!!!!!!!\n")
          t.combined <- NULL
          for(i in 1:length(t.dir)){
            #################################
            # Rewrite the if statement as a #
            # try - catch structure to      #
            # eliminate the i>30 hack       #
            #################################
            if(i>30){
              grads <- read_excel(paste0(path, t.dir[i]))
              grads$GRAD_DATE <- as.character(grads$GRAD_DATE, "%d-%b-%Y")
              
            } else {
              grads <- read.table(paste0(path, t.dir[i]), header=TRUE, sep="|")
            }
            
            if(i < 3){
              grads <- grads %>%
                #filter(COLLEGE=="GE") %>%
                #filter((MAJOR=="GEOG") | (MAJOR=="GIST") | (MAJOR=="SPSG") | (MAJOR=="USGE") | (MAJOR=="GEOS")) %>%
                filter(MAJOR %in% keepMajors) %>%
                select("GRAD_DATE", "NAME", "DEGREE", "MAJOR", "UIN") 
            }
            if(i == 3){
              names(t.combined) <- c("GRAD_DATE", "STUDENT", "DEGREE", "MAJR", "UIN")
              grads <- grads %>%
                #filter(COLLEGE == "GE") %>%
                #filter((MAJR == "GEOG") | (MAJR == "GIST") | (MAJR=="SPSG") | (MAJR=="USGE") | (MAJR=="GEOS")) %>%
                filter(MAJR %in% keepMajors) %>%
                select("GRAD_DATE", "NAME", "DEGREE", "MAJR", "UIN") %>%
                rename(c("STUDENT"="NAME")) 
            }
            if(i > 3){
              if (showDebugCues==TRUE) cat(blue("i>3\n"))
              names(t.combined) <- c("GRAD_DATE", "STUDENT", "DEGC", "MAJR", "UIN")
              grads <- grads %>%
                #filter(COLL == "GE") %>%
                #filter((MAJR == "GEOG") | (MAJR == "GIST") | (MAJR=="SPSG") | (MAJR=="USGE") | (MAJR=="GEOS")) %>%
                filter(MAJR %in% keepMajors) %>%
                select("GRAD_DATE", "STUDENT", "DEGC", "MAJR", "UIN")
            }
            cat("Processed", t.dir[i], "\n")
            if (showDebugCues==TRUE) cat("names(t.combined:", names(t.combined), "\n")
            if (showDebugCues==TRUE) cat("names(grads):", names(grads), "\n")
            if(i>1){
              t.combined <- rbind(t.combined, grads)
            } else {
              t.combined <- grads
            }
            
          }
          # add in the calendar year and fiscal year fields
          t.year <- unlist(lapply(as.character(t.combined$GRAD_DATE), substr, 8,11))
          t.dec <- grep("Dec", as.character(t.combined$GRAD_DATE))
          t.may <- grep("May", as.character(t.combined$GRAD_DATE))
          t.aug <- grep("Aug", as.character(t.combined$GRAD_DATE))
          t.semester <- as.numeric(t.year)
          t.semester[t.dec] <- "C"
          t.semester[t.may] <- "A"
          t.semester[t.aug] <- "B"
          t.year <- as.numeric(t.year)
          t.fiscal <- as.numeric(t.year)
          t.fiscal[t.dec] <- t.fiscal[t.dec]+1
          t.combined <- data.frame(t.combined, calendar.year=t.year, fiscal.year=t.fiscal, semester=t.semester)
          t.combined <- t.combined %>%
            droplevels("DEGC") %>%
            droplevels("MAJR")
          t.combined <- t.combined %>%
            mutate(UIN=as.character(UIN))
          t.combined
        }
        if((needsUpdating==TRUE)|(force.create==TRUE)){
          if(inDept=="GEOG"){
            outFileName <- "all.graduated.students.rds"
          } else {
            outFileName <- paste0("all.graduated.students", inDept, ".rds")
          }
          saveRDS(t.combined, paste0(outDataPath, outFileName))
          #save the RDS file
          #saveRDS(t.combined, paste0(rds.path, "all.graduated.students.rds"))
        }
        t.combined
      }  
      
      integrateStudentMilestoneData <- function(inData, inDept, force.create=FALSE, inDataPath, outDataPath){
        if(is.null(inData)){
          cat(red("Cannot process Milestone Data becasues inData is NULL!\n"))
          outData <- inData
        } else {
          outData <- inData
          inDataPath <- paste0(inDataPath,inDept, "//")
          processDegreeMilestoneFile <- function(inData){
            # Process the degree milestone file to match field names in fullStudentData
            #
            keepCodes <- c("M1DP", "M2PR", "M2RS", "M4FE", "M5TH", "P1DP", "P2PL", "P2PR", "P3AC", "P2PF", "P2RS","P4FE","P5TH" )
            # Handle the legacy data
            tNames <- names(inData)
            
            if ("SHRNCRS.SHRNCRS_SEQ_NO" %in% tNames) {
              ##########################################
              # Convert legacy file forman names into  #
              # names appropriate for the rest of this #
              # procedure.                             #
              ##########################################
              theData <- inData %>%
                select("UIN"="odsmgr.FWS_GET_TAMU_UIN.MST_GENERAL_STUDENT.PERSON_UID.",
                       "degreeSought"="MST_GENERAL_STUDENT.DEGREE",
                       "ADATE"="SHRNCRS.SHRNCRS_NCST_DATE",
                       "DESC"="NCRQ_DESC",
                       "NON_CRS_REQ_CODE"="SHRNCRS.SHRNCRS_NCRQ_CODE") %>% #,
                # "DEGREE_OUTCOME"="OUTCOME") %>%
                filter(NON_CRS_REQ_CODE %in% keepCodes) %>%
                mutate(letter = substr(NON_CRS_REQ_CODE,1,1)) %>%
                mutate(letter2 = substr(degreeSought,1,1)) %>%
                filter(letter==letter2) %>%
                unique() %>%
                mutate(DESC=case_when(DESC=="Doctoral Degree Plan" ~ "DateSubmitted",
                                      DESC=="Master's Degree Plan" ~ "DateSubmitted",
                                      DESC=="Doctoral Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Master's Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Preliminary Examination" ~ "PreliminaryExamDate",
                                      DESC=="Final Examination/Defense" ~ "ThesisDefenseDate",
                                      DESC=="Thesis" ~ "ThesisSubmitDate",
                                      DESC=="Master's Residence Req" ~ "ResidencyReq",
                                      DESC=="Doctoral Residence Req" ~ "ResidencyReq",
                                      TRUE ~ "Other")) %>%
                filter(DESC != "Other") %>%
                select("UIN", "degreeSought", "ADATE", "DESC") %>%
                unique() %>%
                group_by(UIN, degreeSought, DESC) %>%
                mutate(ADATE1=last(ADATE)) %>%
                mutate(ADATE=ADATE1) %>%
                select("UIN", "degreeSought", "ADATE", "DESC") %>%
                unique() %>%
                mutate(ADATE=as.Date(ADATE, "%m/%d/%Y")) %>%
                mutate(ADATE=as.character(ADATE)) %>%
                pivot_wider(names_from=DESC, values_from=ADATE)
              assign("t.legacy", theData, pos=1)
            } else {
              theData <- inData %>%
                select("UIN"="TAMU_UIN", "degreeSought"="DEGREE_CODE", 
                       "ADATE"="SHRNCRS_ACTIVITY_DATE", "DESC"="NON_CRS_REQ_CODE_DESC",
                       "NON_CRS_REQ_CODE", "DEGREE_OUTCOME") %>%
                filter(NON_CRS_REQ_CODE %in% keepCodes) %>%
                mutate(letter = substr(NON_CRS_REQ_CODE,1,1)) %>%
                mutate(letter2 = substr(degreeSought,1,1)) %>%
                filter(letter==letter2) %>%
                mutate(DESC=case_when(DESC=="Doctoral Degree Plan" ~ "DateSubmitted",
                                      DESC=="Master's Degree Plan" ~ "DateSubmitted",
                                      DESC=="Doctoral Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Master's Research Proposal" ~ "ProposalSubmitDate",
                                      DESC=="Preliminary Examination" ~ "PreliminaryExamDate",
                                      DESC=="Final Examination/Defense" ~ "ThesisDefenseDate",
                                      DESC=="Thesis" ~ "ThesisSubmitDate",
                                      DESC=="Master's Residence Req" ~ "ResidencyReq",
                                      DESC=="Doctoral Residence Req" ~ "ResidencyReq",
                                      TRUE ~ "Other")) %>%
                filter(DESC != "Other") %>%
                select("UIN", "degreeSought", "ADATE", "DESC") %>%
                unique() %>%
                mutate(ADATE=as.Date(ADATE, "%m-%d-%Y")) %>%
                mutate(ADATE=as.character(ADATE)) %>%
                pivot_wider(names_from=DESC, values_from=ADATE)
            }
            theData
          }
          readSingleDegreeMilestoneFile <- function(inFileName){
            theData <- read.csv(inFileName)
            theData
          }
          
          if(inDept=="GEOG"){
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", ".rds"))$mtime
          } else {
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", inDept, ".rds"))$mtime  
          }
          
          existingFiles <- dir(inDataPath)
          if(length(existingFiles)==0){
            cat(red("There are no files\n"))
            outData <- inData
          } else {
            existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
            existingFilesFullPath <- paste0(inDataPath, existingFiles)
            fileInfo <- file.info(existingFilesFullPath)
            if(force.create==TRUE){
              processFiles <- fileInfo
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
              #print(processFiles)
            } else {
              processFiles <- fileInfo %>%
                filter(mtime > rdsCreationDate)
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
            }
            #processFiles <- fileInfo
            outData <- inData
            #cat(green("before if\n"))
            if(nrow(processFiles)>0){
              #cat(green("nrow(processFiles)>0\n"))
              keepNames <- names(outData)
              #cat("keepNames:", keepNames, "\n")
              for(i in 1:dim(processFiles)[1]){
                #for(i in 1:1){
                
                message(paste("Reading", row.names(fileInfo)[i]))
                pD <- readSingleDegreeMilestoneFile(row.names(fileInfo)[i])
                pD <- processDegreeMilestoneFile(pD)
                
                
                ##########################################
                # Integrate the data from pD into inData #
                ##########################################
                
                pD <- pD %>%
                  mutate(UIN=as.character(UIN))
                outData <- outData %>%
                  mutate(UIN=as.character(UIN)) %>%
                  left_join(pD, by=c("UIN", "degreeSought")) %>%
                  mutate_if(is.Date, as.character) 
                
                #cat(blue(names(outData)), "\n")
                if("DateSubmitted.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(DateSubmitted=case_when(!is.na(DateSubmitted.y) ~ DateSubmitted.y,
                                                   TRUE ~ DateSubmitted.x)) 
                }
                if("ResidencyReq.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ResidencyReq=case_when(!is.na(ResidencyReq.y) ~ ResidencyReq.y,
                                                  TRUE ~ ResidencyReq.x)) 
                }
                if("PreliminaryExamDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(PreliminaryExamDate=case_when(!is.na(PreliminaryExamDate.y) ~ PreliminaryExamDate.y,
                                                         TRUE ~ PreliminaryExamDate.x)) 
                }
                if("ProposalSubmitDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ProposalSubmitDate=case_when(!is.na(ProposalSubmitDate.y) ~ ProposalSubmitDate.y,
                                                        TRUE ~ ProposalSubmitDate.x)) 
                }
                if("ThesisDefenseDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ThesisDefenseDate=case_when(!is.na(ThesisDefenseDate.y) ~ ThesisDefenseDate.y,
                                                       TRUE ~ ThesisDefenseDate.x))
                }
                ##############################################
                # Add mutate statements for ThesisSubmitDate #
                # in an if statment                          #
                ##############################################
                if("ThesisSubmitDate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ThesisSubmitDate=case_when(!is.na(ThesisSubmitDate.y) ~ ThesisSubmitDate.y,
                                                      TRUE ~ ThesisSubmitDate.x))
                }
                ##############################################
                # Eliminate the new fields with .x and .y    #
                ##############################################
                
                possibleNames <- c(keepNames, "DateSubmitted", "ResidencyReq", "PreliminaryExamDate", "ProposalSubmitDate", 
                                   "ThesisDefenseDate", "ThesisSubmitDate")
                actualNames <- union(keepNames, possibleNames)
                theFinalNames <- intersect(names(outData), actualNames)
                
                outData <- outData %>%
                  select(all_of(theFinalNames))
                #select(all_of(keepNames))
                #cat(green(names(outData), "\n"))
                # Here is where a new rds file needs to be written.
                #saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
              }
            }
          }
          
          #cat(blue("before mutates\n"))
          theDateFormat <- "%Y-%m-%d"
          if(!("DateSubmitted" %in% names(outData))){
            outData <- outData %>%
              mutate(DateSubmitted=NA) %>%
              mutate(DateSubmitted=as.Date(DateSubmitted))
          }
          if(!("DateApprovedByFaculty" %in% names(outData))){
            outData <- outData %>%
              mutate(DateApprovedByFaculty =NA) %>%
              mutate(DateApprovedByFaculty=as.Date(DateApprovedByFaculty))
          }
          if(!("ProposalDefense" %in% names(outData))){
            outData <- outData %>%
              mutate(ProposalDefense = NA) %>%
              mutate(ProposalDefense = as.Date(ProposalDefense))
          }
          if(!("ProposalSubmitDate" %in% names(outData))){
            outData <- outData %>%
              mutate(ProposalSubmitDate = NA) %>%
              mutate(ProposalSubmitDate = as.Date(ProposalSubmitDate))
          }
          if(!("ResidencyReq" %in% names(outData))){
            outData <- outData %>%
              mutate(ResidencyReq = NA) %>%
              mutate(ResidencyReq = as.Date(ProposalSubmitDate))
          }
          if(!("PreliminaryExamDate" %in% names(outData))){
            outData <- outData %>%
              mutate(PreliminaryExamDate = NA) %>%
              mutate(PreliminaryExamDate = as.Date(PreliminaryExamDate))
          }
          if(!("ThesisDefenseDate" %in% names(outData))){
            outData <- outData %>%
              mutate(ThesisDefenseDate = NA) %>%
              mutate(ThesisDefenseDate = as.Date(ThesisDefenseDate))
          }
          if(!("ThesisSubmitDate" %in% names(outData))){
            outData <- outData %>%
              mutate(ThesisSubmitDate = NA) %>%
              mutate(ThesisSubmitDate = as.Date(ThesisSubmitDate))
          }
          if(!("PreliminaryExamResult" %in% names(outData))){
            outData <- outData %>%
              mutate(PreliminaryExamResult = NA)
          }
          
          outData <- outData %>%
            mutate(DateSubmitted=as.Date(DateSubmitted, theDateFormat)) %>%
            mutate(DateApprovedByFaculty=as.Date(DateApprovedByFaculty, theDateFormat)) %>%
            mutate(ProposalDefense= as.Date(ProposalDefense, theDateFormat)) %>%
            mutate(ProposalSubmitDate=as.Date(ProposalSubmitDate, theDateFormat)) %>%
            mutate(ResidencyReq=as.Date(ResidencyReq, theDateFormat)) %>%
            mutate(PreliminaryExamDate=as.Date(PreliminaryExamDate, theDateFormat)) %>%
            mutate(ThesisDefenseDate=as.Date(ThesisDefenseDate, theDateFormat)) %>%
            mutate(ThesisSubmitDate=as.Date(ThesisSubmitDate, theDateFormat)) %>%
            unique()
          
        }
        outData
      }
      
      integrateSocialMediaData <- function(inData, inDept, force.create=FALSE, inDataPath, outDataPath){
        if(is.null(inData)){
          cat(red("Cannot process Social Media Data becasues inData is NULL!\n"))
          outData <- inData
        } else {
          outData <- inData
          inDataPath <- paste0(inDataPath,inDept, "//")
          #cat("test1","***\n")
          
          readSingleSocialMediaFile <- function(inFileName){
            #cat("inFileName:", inFileName, "\n")
            theData <- read.csv(inFileName)
            #Remove everything except UIN and Social Media Fields
            keepNames <- c("UIN", "LinkedIn", "Facebook", "Twitter", "Reddit", "Instagram", "ResearchGate", "Orcid", "Github")
            theNames <- intersect(keepNames, names(theData))
            theData <- theData %>%
              select(all_of(theNames))
            
            theData
          }
          
          if(inDept=="GEOG"){
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", ".rds"))$mtime
          } else {
            rdsCreationDate <- file.info(paste0(outDataPath, "fullStudentData", inDept, ".rds"))$mtime  
          }
          #cat("inDataPath:", inDataPath, "***\n")
          existingFiles1 <- dir("//Volumes//GoogleDrive//My Drive//ShinyApps//LocalDataArchive//SocialMediaData//GEOG//")
          existingFiles <- dir(inDataPath)
          if(length(existingFiles)==0){
            cat(green("There are no social media files\n"))
            outData <- inData
          } else {
            
            existingFiles <- existingFiles[(substr(existingFiles,1,1) != "~")]
            existingFilesFullPath <- paste0(inDataPath, existingFiles)
            fileInfo <- file.info(existingFilesFullPath)
            if(force.create==TRUE){
              processFiles <- fileInfo
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
            } else {
              processFiles <- fileInfo %>%
                filter(mtime > rdsCreationDate)
              #cat("nrow(processFiles):", nrow(processFiles), "\n")
            }
            #processFiles <- fileInfo
            outData <- inData
            #cat(green("before if\n"))
            #cat(green("Number of social media data files to process:", nrow(processFiles),"\n"))
            if(nrow(processFiles)>0){
              #cat(green("nrow(processFiles)>0\n"))
              keepNames <- names(outData)
              for(i in 1:dim(processFiles)[1]){
                
                message(paste("Reading", row.names(fileInfo)[i]))
                newSMData <- readSingleSocialMediaFile(row.names(fileInfo)[i])
                newSMData <- newSMData %>%
                  mutate_if(is.factor, is.character) %>%
                  mutate_if(is.logical, as.character)
                outData <- outData %>%
                  mutate_if(is.factor, as.character) %>%
                  mutate_if(is.logical, as.character)
                
                #################################################
                # Integrate the data from newSMData into inData #
                #################################################
                
                newSMData <- newSMData %>%
                  mutate(UIN=as.character(UIN))
                outData <- outData %>%
                  mutate(UIN=as.character(UIN)) %>%
                  left_join(newSMData, by=c("UIN")) 
                if("Facebook.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Facebook=case_when(!is.na(Facebook.y) ~ Facebook.y,
                                              TRUE ~ Facebook.x)) 
                }
                if("LinkedIn.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(LinkedIn=case_when(!is.na(LinkedIn.y) ~ LinkedIn.y,
                                              TRUE ~ LinkedIn.x)) 
                }
                if("Twitter.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Twitter=case_when(!is.na(Twitter.y) ~ Twitter.y,
                                             TRUE ~ Twitter.x)) 
                }
                if("Reddit.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Reddit=case_when(!is.na(Reddit.y) ~ Reddit.y,
                                            TRUE ~ Reddit.x)) 
                }
                if("Instagram.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Instagram=case_when(!is.na(Instagram.y) ~ Instagram.y,
                                               TRUE ~ Instagram.x))
                }
                if("ResearchGate.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(ResearchGate=case_when(!is.na(ResearchGate.y) ~ ResearchGate.y,
                                                  TRUE ~ ResearchGate.x))
                }
                if("Orcid.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Orcid=case_when(!is.na(Orcid.y) ~ Orcid.y,
                                           TRUE ~ Orcid.x))
                }
                if("Github.y" %in% names(outData)){
                  outData <- outData %>%
                    mutate(Github=case_when(!is.na(Github.y) ~ Github.y,
                                            TRUE ~ Github.x))
                }
                
                ##############################################
                # Eliminate the new fields with .x and .y    #
                ##############################################
                #cat(green("Names to keep:", all_of(keepNames), "\n"))
                outData <- outData %>%
                  select(all_of(keepNames))
                
                # Here is where a new rds file needs to be written.
                #saveRDS(outData, paste0(outDataPath, "studentSupervisoryCommittees.rds"))
              }
            }
          }
          
          if(!("LinkedIn" %in% names(outData))){
            #cat(green("No DP fields in outData\n"))
            outData <- outData %>%
              mutate(LinkedIn=NA) %>%
              mutate(Facebook=NA) %>%
              mutate(Twitter=NA) %>%
              mutate(Reddit=NA) %>%
              mutate(Instagram=NA) %>%
              mutate(Orcid=NA) %>%
              mutate(ResearchGate=NA) %>%
              mutate(Github=NA) 
          } 
        }
        outData
      }
      
      integrateGraduationData <- function(inData, inDept, force.create=FALSE, inDataPath){
        
        if(is.null(inData)){
          cat(red("Cannot process Graduation Data becasues inData is NULL!\n"))
          outData <- inData
        } else {
          t.gradListing <- makeCompleteGraduateListing(inDept=useDept, force.create=force.create, local.archive.path=inDataPath)
          # cat(green("after t.gradListing, is.null(inData):", is.null(inData), "\n"))
          # assign("t.gradListing", t.gradListing, pos=1)
          # assign("t.inData", inData, pos=1)
          # print(names(inData))
          outData <- inData %>%
            mutate(UIN=as.character(UIN)) %>%
            left_join(t.gradListing, by=c("UIN", "degreeSought"="DEGC")) 
          if("GRAD_DATE.y" %in% names(outData)){
            outData <- outData %>%
              mutate(GRAD_DATE=case_when(!is.na(GRAD_DATE.y) ~ GRAD_DATE.y,
                                         TRUE ~ GRAD_DATE.x)) %>%
              mutate(STUDENT=case_when(!is.na(STUDENT.y) ~ STUDENT.y,
                                       TRUE ~ STUDENT.x)) %>%
              mutate(calendar.year=case_when(!is.na(calendar.year.y) ~ calendar.year.y,
                                             TRUE ~ calendar.year.x)) %>%
              mutate(fiscal.year=case_when(!is.na(fiscal.year.y) ~ fiscal.year.y,
                                           TRUE ~ fiscal.year.x)) %>%
              mutate(semester=case_when(!is.na(semester.y) ~ semester.y,
                                        TRUE ~ semester.x)) 
          }
          if("MAJR.y" %in% names(outData)){
            outData <- outData %>%
              mutate(MAJR=case_when(!is.na(MAJR.y) ~ MAJR.y,
                                    TRUE ~ MAJR.x))
          }
          # assign("t.outData", outData, pos=1)
        }
        outData
      }
      
      
      #############################################
      # Read Graduate Application Data            #
      #############################################
      if(file.exists(paste0(rds.path, "applicants.rds"))){
        graduateApplicants <- readRDS(paste0(rds.path, "applicants.rds"))
      } else {
        graduateApplicants <- NULL
      }
      graduateApplicants <- readApplicantInfo(inData=graduateApplicants, inDataPath=paste0(local.archive.path, "ApplicantData//"),
                                              outDataPath=rds.path, force.create=TRUE)
      fileInventoryFilename <- c(fileInventoryFilename, "applicants.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Graduate Applications")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "applicants.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(graduateApplicants$TERM))
      
      rvFI$fileInventory <- data.frame(filename=fileInventoryFilename, description=fileInventoryDescription, updated=fileInventoryLastUpdate, recent=fileInventoryMostRecent)
      
      cat(yellow("Read Graduate Applicants Data\n"))
      
      if(is.null(outputCollection)){
        outputCollection <- list(graduateApplicants=graduateApplicants)
      } else {
        outputCollection[["graduateApplicants"]] <- graduateApplicants
      }
      outputCollection <- list(outputCollection, graduateApplicants=graduateApplicants)
      
      #############################################
      # Read Student Supervisory Committee Data   #
      #############################################
      
      if(file.exists(paste0(rds.path, "studentSupervisoryCommittees.rds"))){
        studentSupervisoryCommittees <- readRDS(paste0(rds.path, "studentSupervisoryCommittees.rds"))
      } else {
        studentSupervisoryCommittees <- NULL
      }
      
      studentSupervisoryCommittees <- readStudentSupervisoryCommittees(inData=studentSupervisoryCommittees, inDept=inDept, inDataPath=paste0(local.archive.path, "StudentSupervisoryCommitteeData//"),
                                                                       outDataPath=rds.path)
      
      fileInventoryFilename <- c(fileInventoryFilename, "studentSupervisoryCommittees.rds")
      fileInventoryDescription <- c(fileInventoryDescription, "Student Supervisory Committees")
      fileInventoryLastUpdate <- c(fileInventoryLastUpdate, format(file.info(paste0(rds.path, "studentSupervisoryCommittees.rds"))$mtime, "%d-%b-%Y"))
      fileInventoryMostRecent <- c(fileInventoryMostRecent, max(enrolledStudents$TERM))
      
      rvFI$fileInventory <- data.frame(filename=fileInventoryFilename, description=fileInventoryDescription, updated=fileInventoryLastUpdate, recent=fileInventoryMostRecent)
      
      cat(yellow("Read Student Supervisory Committees Data\n"))
      
      if(is.null(outputCollection)){
        outputCollection <- list(studentSupervisoryCommittees=studentSupervisoryCommittees)
      } else {
        outputCollection[["studentSupervisoryCommittees"]] <- studentSupervisoryCommittees
      }
      
      #############################################
      # Read Full Graduate Student Data           #
      #############################################
      
      fullStudentData <- createFullDataAnyDepartment(inDept=useDept, inEnrolledStudents=enrolledStudents, 
                                            semester.codes=semester.codes, force.create=FALSE,
                                            local.archive.path=local.archive.path)
      
      cat(yellow("Read Full Graduate Student Data\n"))
      
      if(is.null(outputCollection)){
        outputCollection <- list(fullStudentData=fullStudentData)
      } else {
        outputCollection[["fullStudentData"]] <- fullStudentData
      }
      
      #############################################
      # Return the list that contains all of the  #
      # necessary data for the dashboard          #
      #############################################
      
      return(outputCollection)
    }
  )
}
