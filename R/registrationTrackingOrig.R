#' registrationTrackingOrigUI
#'
#' @param id the ID
#' @param label the Label
#'
#' @return
#' @export
#'
#' @examples
registrationTrackingOrigUI <- function(id, label = "Registration Tracking") {
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=FALSE,
                   solidHeader=TRUE,
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("entireUI")))
                                  )
                   ),
                   id=ns("enrollmentBox"),
                   class="schBoxTestOct"
                 ))

}

#' registrationTrackingOrigServer
#'
#' @param id the id
#' @param deptAbbrv the Department Abbreviation
#' @param previousSemestersFinalEnrollment previous semester's final enrollment
#' @param fullRegistrationTracking full registration tracking data
#' @param useShort use short data format
#' @param semester.codes the semester codes
#' @param classesStart list of dates when classes start by semester
#' @param focalSem focal Semester
#' @param refSem reference Semestr
#' @param ugControl undergraduate control
#' @param syncSwitch syncronize switch
#' @param sameTerm same term
#' @param chosenCourse chosen course
#' @param synonyms for departments (e.g. PSYC=PBSI)
#'
#' @return
#' @export
#'
#' @examples
registrationTrackingOrigServer <- function(id, deptAbbrv, focalSem, refSem, ugControl,
                                           syncSwitch, sameTerm, chosenCourse,
                                           previousSemestersFinalEnrollment, fullRegistrationTracking, useShort=FALSE,
                                           semester.codes,
                                           classesStart,
                                           synonyms) {
  moduleServer(
    id,

    function(input, output, session) {


      #########################
      # Global Variables      #
      # within this module    #
      #########################

      #########################
      # Reactive Variables    #
      #########################
      vals <- reactiveValues()
      observe({vals$Department <- deptAbbrv()})
      observe({vals$courseNum <- chosenCourse()})
      observe({vals$synchronize <- syncSwitch()})
      observe({vals$synchronize <- sameTerm()})
      observe({vals$focalSemester <- rvFocalSemester()})
      rvFocalSemester <- reactiveVal(NA)
      rvReferenceSemester <- reactiveVal(NA)
      rvDepartment <- reactiveVal(NA)
      rvCourse <- reactiveVal(NA)

      #########################
      # Input Control UIs     #
      #########################

      output$choiceUGControl <- renderUI({
        ns <- session$ns
        t.out <- radioButtons(ns("choiceUG"),
                              "Course Level",
                              c("Undergraduate"=1, "Graduate"=2))
        t.out
      })
      output$synchronize <- renderUI({
        ns <- session$ns
        materialSwitch(ns("synchSwitch"), value=TRUE, label="Synchronize")
      })
      output$sameTerm <- renderUI({
        ns <- session$ns
        materialSwitch(ns("sameTermSwitch"), value=TRUE, label="Same Term:")
      })

      #########################
      # Card Elements         #
      #########################
      output$plotTitle <- renderText({
        ns <- session$ns
        req(deptAbbrv())
        req(chosenCourse())

        if(ugControl()==1){
          courseNumber <- input$selectCourseUG
        } else {
          courseNumber <- input$selectCourseGrad
        }
        theTitle <- paste0(deptAbbrv(), " ", chosenCourse(), " Registration Tracking")
        theTitle
      })

      output$entireUI <- renderUI({
        ns <- session$ns
        tabsetPanel(type="tabs",
                    tabPanel("Registered Students",
                             fluidRow(conditionalPanel(condition="2 > 0", c3Output(ns("distPlotC3")))),

                    ),
                    # tabPanel("Major Demographics",
                    #          #conditionalPanel(condition="2 > 0", c3Output(ns("plotMajorsDemographics"))),
                    #          #uiOutput(ns("demographics"))
                    #          h3("Blank text")
                    # ),
                    tabPanel("Sections",
                             conditionalPanel(condition="2 > 0", c3Output(ns("plotC3Sections"))),
                    ),
                    tabPanel("Gains/Losses",
                             fluidRow(
                               column(6, c3Output(ns("plotLossesC3"))),
                               column(6, c3Output(ns("plotGainesC3")))
                             )
                    )
        ) #tabsetPanel
      })
      output$distPlotC3 <- renderC3({
        make.plotC3()
      })

      output$demographics <- renderUI({
        ns <- session$ns
        tagList(
          fluidRow(
            column(6, DTOutput(ns("demographicsDT"))),
            column(6, c3Output(ns("demographicsPie")))
          )
        )
      })
      output$demographicsPie <- renderC3({
        processedData <- makeMajorsPie()

        c3(processedData) %>%
          c3_pie(legendPosition="Left") %>%
          legend(position="right")

      })
      output$demographicsDT <- renderDT({
        processedData <- makeMajorsDT() %>%
          arrange(-n)
        datatable(processedData, escape=FALSE,
                  #container=sketch,
                  rownames=FALSE,
                  options=list(
                    dom='t',
                    ordering=FALSE,
                    pageLength=100,
                    scrollY="200px"
                  ))
      })


      output$plotC3Sections <- renderC3({
        make.plotSectionsC3()
      })
      output$plotLossesC3 <- renderC3({
        determineLosses()
      })
      output$plotGainesC3 <- renderC3({
        determineGains()
      })


      #########################
      # Observer Functions    #
      #########################

      #########################
      # Data processing and   #
      # preparation functions #
      #########################

      createFullDataForFigure <- function(inData, oldReferenceData, useShort){
        singleCourseSummaryWithoutSections <- function(inData, courseID, theSemester, useShort){
          options(dplyr.summarise.inform=F)

          # determine if theSemester is in the data set
          if(theSemester %in% unique(inData$semester)){
            if (useShort) {
              outData <- inData %>%
                filter(course.designation == courseID) %>%
                filter(semester == theSemester) %>%
                group_by(dateCreated) %>%
                summarize(n=sum(numEnrolled))
            } else {
              outData <- inData %>%
                filter(course.designation == courseID) %>%
                filter(semester == theSemester) %>%
                group_by(dateCreated) %>%
                summarize(n=n())
            }
            options(dplyr.summarise.inform=F)
          } else {
            outData <- NULL
          }
          outData
        }

        combineFocalAndReferenceData <- function(inFocalData, inReferenceData){
          req(inFocalData)
          req(inReferenceData)

          # fill in missing dates
          if(nrow(inFocalData)>0){
            inFocalData <- inFocalData %>%
              mutate(dateCreated=as.POSIXct(dateCreated)) %>%
              complete(dateCreated=seq.POSIXt(min(dateCreated), max(dateCreated), by="day")) %>%
              mutate(dateCreated=as.character(dateCreated))
          }
          if(nrow(inReferenceData)>0){
            inReferenceData <- inReferenceData %>%
              mutate(dateCreated=as.POSIXct(dateCreated)) %>%
              complete(dateCreated=seq.POSIXt(min(dateCreated), max(dateCreated), by="day")) %>%
              mutate(dateCreated=as.character(dateCreated))
          }

          referenceSemester <- refSem()
          focalSemester <- focalSem()
          if(referenceSemester %in% classesStart$semester){
            refStartDate <- classesStart %>%
              filter(semester==referenceSemester) %>%
              select("startDate") %>%
              format(., "%Y-%m-%d") %>%
              as.character()
          } else {
            refStartDate <- NULL
          }


          focalStartDate <- classesStart %>%
            filter(semester==focalSemester) %>%
            select("startDate") %>%
            format(., "%Y-%m-%d") %>%
            as.character()

          if(focalStartDate=="character(0)") {
            message("classesStart is missing startDate for ", focalSemester, "\n")
          }

          #if no refStartDate then t.rd1 should be static over time.
          if(is.null(refStartDate)){
            t.fd1 <- inFocalData %>%
              mutate(temp1=difftime(dateCreated, focalStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) %>%
              select("dateCreated", "n", "daysBefore") %>%
              mutate(nRef=max(inReferenceData$n, na.rm=TRUE))
          } else {
            t.rd1 <- inReferenceData %>%
              mutate(temp1=difftime(dateCreated, refStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) #

            t.rd1 <- t.rd1 %>%
              select("nRef"="n", "daysBefore")

            t.fd1 <- inFocalData %>%
              mutate(temp1=difftime(dateCreated, focalStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) #


            t.fd1 <- t.fd1 %>%
              select("dateCreated", "n", "daysBefore") %>%
              full_join(t.rd1, by=c("daysBefore"))

          }


          if(nrow(t.fd1)>0){
            for(i in 1:dim(t.fd1)[[1]]){
              if(is.na(t.fd1[i,"dateCreated"])){
                t.fd1[i,"dateCreated"] <- as.character(as.Date(as.character(t.fd1[(i-1),"dateCreated"]))+1)
              }
            }
          }

          t.fd1 %>%
            select("dateCreated", "n", "nRef")
        }


        dept <- deptAbbrv()

        dept.name <- dept
        # if(input$choiceUG == "1") {
        #   useSelectCourse <- input$selectCourseUG
        # } else {
        #   useSelectCourse <- input$selectCourseGrad
        # }

        browser()
        targetCourse <- paste(dept.name, useSelectCourse)

        focalData <- singleCourseSummaryWithoutSections(inData, targetCourse, input$focalSemester, useShort=useShort)
        referenceData <- singleCourseSummaryWithoutSections(inData, targetCourse, input$referenceSemester, useShort=useShort)

        if(is.null(referenceData)){
          cat("Reference data don't exist as a time series.\n")
          referenceData <- focalData
          referenceData$n <- oldReferenceData

        }


        combinedData <- combineFocalAndReferenceData(focalData, referenceData)

        combinedData
      }

      ############################################
      # re-configure this code.                  #
      # make.plotC3 should only include plotting #
      #   functions.  NOT data prep              #
      # Write a new reactive({}) to do the prep  #
      ############################################

      demoDataPreparation <- reactive({
        req(focalSem())
        req(deptAbbrv())
        req(chosenCourse())

        theDemographicData <- fullRegistrationTracking %>%
          filter(course.designation==paste(deptAbbrv(), chosenCourse())) %>%
          filter(semester==focalSem()) %>%
          group_by(dateCreated, major) %>%
          summarize(n=sum(numEnrolled))

        theLatestDate <- max(theDemographicData$dateCreated)

        theDemographicData <- theDemographicData %>%
          filter(dateCreated==theLatestDate)

      })

      dataPreparation <- reactive({

        ############################
        # Required elements        #
        ############################
        req(focalSem())
        req(refSem())
        req(deptAbbrv())
        req(chosenCourse())

        dailyTrackingSemesters <- unique(fullRegistrationTracking$semester)
        ############################
        # Function definitions     #
        ############################

        singleCourseSummaryWithoutSections <- function(inData, courseID, theSemester){

          options(dplyr.summarise.inform=F)

          processHistoricalData <- function(inData){

            #browser()
            courseSubject <- str_extract(courseID, "[A-Z]+")
            courseNumber <- str_extract(courseID, "\\d+")
            possibleSubjects <- synonyms %>%
              filter(subject==courseSubject) %>%
              unlist() %>%
              as.vector() %>%
              unique()

            inData %>%
              #mutate(course.designation = paste(subject, course.number)) %>%
              filter(subject %in% possibleSubjects) %>%
              filter(course.number==courseNumber) %>%
              #filter(course.designation == courseID) %>%
              filter(semester == theSemester) %>%
              summarize(n=sum(enrolledStudents))

          }
          processDailyData <- function(inData){

            courseSubject <- str_extract(courseID, "[A-Z]+")
            theCourseNumber <- str_extract(courseID, "\\d+")
            possibleSubjects <- synonyms %>%
              filter(subject==courseSubject) %>%
              unlist() %>%
              as.vector() %>%
              unique()
            midData <- inData %>%
              filter(subject %in% possibleSubjects) %>%
              filter(courseNumber == theCourseNumber)

            midData %>%
              filter(semester == theSemester) %>%
              group_by(dateCreated) %>%
              summarize(n=sum(numEnrolled))

          }
          if("course.designation" %in% names(inData)){
            outData <- processDailyData(inData)
          } else {
            outData <- processHistoricalData(inData)
          }

          options(dplyr.summarise.inform=T)
          outData
        }
        getReferenceData <- function(inData, courseID, referenceSemester){
          options(dplyr.summarise.inform=F)
#browser()
          referenceData <- inData %>%
            filter(semester==referenceSemester) %>%
            filter(designation==courseID) %>%
            group_by(designation) %>%
            summarise(n=n(), .groups="drop")
          options(dplyr.summarise.inform=T)
          referenceData$n
        }
        getReferenceDataShort <- function(inData, courseID, referenceSemester){
          #browser()
          options(dplyr.summarise.inform=F)
          referenceData <- inData %>%
            filter(semester==referenceSemester) %>%
            mutate(designation=paste(subject, course.number)) %>%
            filter(designation==courseID)
          options(dplyr.summarise.inform=T)

          referenceData$enrolledStudents
        }
        combineFocalAndReferenceData <- function(inFocalData, inReferenceData){

          req(inFocalData)
          req(inReferenceData)

          if(nrow(inReferenceData)==1){
            midReferenceData <- inFocalData %>%
              mutate(n=inReferenceData$n)
            inReferenceData <- midReferenceData
          }
          # fill in missing dates
          if(nrow(inFocalData)>0){
            inFocalData <- inFocalData %>%
              mutate(dateCreated=as.POSIXct(dateCreated)) %>%
              complete(dateCreated=seq.POSIXt(min(dateCreated), max(dateCreated), by="day")) %>%
              mutate(dateCreated=as.character(dateCreated))
          }
          if(nrow(inReferenceData)>0){
            inReferenceData <- inReferenceData %>%
              mutate(dateCreated=as.POSIXct(dateCreated)) %>%
              complete(dateCreated=seq.POSIXt(min(dateCreated), max(dateCreated), by="day")) %>%
              mutate(dateCreated=as.character(dateCreated))
          }

          referenceSemester <- refSem()
          focalSemester <- focalSem()
          if(referenceSemester %in% classesStart$semester){
            refStartDate <- classesStart %>%
              filter(semester==referenceSemester) %>%
              select("startDate") %>%
              format(., "%Y-%m-%d") %>%
              as.character()
          } else {
            refStartDate <- NULL
          }


          focalStartDate <- classesStart %>%
            filter(semester==focalSemester) %>%
            select("startDate") %>%
            format(., "%Y-%m-%d") %>%
            as.character()

          if(focalStartDate=="character(0)") {
            message("classesStart is missing startDate for ", focalSemester, "\n")
          }

          #if no refStartDate then t.rd1 should be static over time.
          if(is.null(refStartDate)){
            t.fd1 <- inFocalData %>%
              mutate(temp1=difftime(dateCreated, focalStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) %>%
              select("dateCreated", "n", "daysBefore") %>%
              mutate(nRef=max(inReferenceData$n, na.rm=TRUE))
          } else {
            t.rd1 <- inReferenceData %>%
              mutate(temp1=difftime(dateCreated, refStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) #

            t.rd1 <- t.rd1 %>%
              select("nRef"="n", "daysBefore")

            t.fd1 <- inFocalData %>%
              mutate(temp1=difftime(dateCreated, focalStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) #


            t.fd1 <- t.fd1 %>%
              select("dateCreated", "n", "daysBefore") %>%
              full_join(t.rd1, by=c("daysBefore")) %>%
              filter(!is.na(dateCreated))
          }

          # add dates
          if(nrow(t.fd1)>0){
            for(i in 1:dim(t.fd1)[[1]]){
              if(is.na(t.fd1[i,"dateCreated"])){
                t.fd1[i,"dateCreated"] <- as.character(as.Date(as.character(t.fd1[(i-1),"dateCreated"]))+1)
              }
            }
          }

          t.fd1 %>%
            select("dateCreated", "n", "nRef")
        }

        ############################
        # Processing logic         #
        ############################

        theFocalData <- singleCourseSummaryWithoutSections(fullRegistrationTracking, paste(deptAbbrv(), chosenCourse()), focalSem())

        if(refSem() %in% dailyTrackingSemesters){
          theReferenceData <- singleCourseSummaryWithoutSections(fullRegistrationTracking, paste(deptAbbrv(), chosenCourse()), refSem())
        } else {
          theReferenceData <- singleCourseSummaryWithoutSections(previousSemestersFinalEnrollment, paste(deptAbbrv(), chosenCourse()), refSem())
        }
        combinedData <- combineFocalAndReferenceData(theFocalData, theReferenceData)
        #browser()
        combinedData
      })

      make.plotC3 <- reactive({

        t.data <- dataPreparation()

        ##########################################
        # convert input$referenceSemester to a   #
        # format suitable as a column name       #
        ##########################################
        refSemester <- semester.codes %>%
          filter(current==refSem()) %>%
          select(semester4) %>%
          unlist() %>%
          as.vector() %>%
          as.character()
        focSemester <- semester.codes %>%
          filter(current==focalSem()) %>%
          select(semester4) %>%
          unlist() %>%
          as.vector() %>%
          as.character()

        theData <- t.data %>%
          mutate(dateCreated=as.Date(dateCreated)) %>%
          mutate(!!refSemester := nRef) %>%
          rename(!!focSemester := 'n') %>%
          select("dateCreated", refSemester, focSemester) %>%
          c3(x='dateCreated') %>%
          c3_viridis()
        theData
      })

      makeMajorsDT <- reactive({
        demoData <- demoDataPreparation()

        demoData

      })
      makeMajorsPie <- reactive({

        demoData <- demoDataPreparation() %>%
          ungroup() %>%
          slice_max(n, n=5) %>%
          pivot_wider(id_cols=dateCreated, names_from=major, values_from=n)

        demoData
      })

      make.plotSectionsC3 <- reactive({

        ############################
        # Required elements        #
        ############################
        req(focalSem())
        req(refSem())
        req(deptAbbrv())
        req(chosenCourse())

        singleCourseSummaryWithSections <- function(inData, courseID, theSemester){
          options(dplyr.summarise.inform=F)

          outData <- inData %>%
            filter(course.designation == courseID) %>%
            filter(semester==theSemester) %>%
            group_by(dateCreated, courseSection) %>%
            summarize(n=sum(numEnrolled))
          options(dplyr.summarise.inform=T)
          outData
        }

        theData <- singleCourseSummaryWithSections(fullRegistrationTracking, paste(deptAbbrv(), chosenCourse()), focalSem())

        theData <- theData %>%
          arrange(courseSection) %>%
          pivot_wider(id_cols=dateCreated, names_from=courseSection, values_from=n) %>%
          rowwise() %>%
          mutate(Total=sum(cur_data(), na.rm=TRUE))

        theData %>%
          mutate(dateCreated=as.Date(dateCreated)) %>%
          c3(x='dateCreated') %>%
          c3_viridis()


      })
      make.plotMajorsC3 <- reactive({

        cat(blue("[make.plotJajorsC3] useShort:", useShort, "\n"))

        singleCourseSummaryWithoutSections <- function(inData, courseID, theSemester){
          #print(names(inData))
          #browser()
          options(dplyr.summarise.inform=F)
          if(!useShort){
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, major) %>%
              summarize(n=n())
          }
          else{
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, major) %>%
              summarize(n=sum(numEnrolled), .groups="drop")
          }
          options(dplyr.summarise.inform=T)
          outData
        }

        dept <- input$selectDept
        if (as.numeric(dept) == 1) dept.name <- "ATMO"
        if (as.numeric(dept) == 2) dept.name <- "GEOG"
        if (as.numeric(dept) == 3) dept.name <- "GEOL"
        if (as.numeric(dept) == 4) dept.name <- "GEOS"
        if (as.numeric(dept) == 5) dept.name <- "OCNG"
        if (as.numeric(dept) == 6) dept.name <- "GEOP"
        if(input$choiceUG == "1") {
          useSelectCourse <- input$selectCourseUG
        } else {
          useSelectCourse <- input$selectCourseGrad
        }

        theData <- singleCourseSummaryWithoutSections(fullRegistrationTracking, paste(dept.name, useSelectCourse), input$focalSemester)

        ##########################################
        # convert input$referenceSemester to a   #
        # format suitable as a column name       #
        ##########################################
        #assign("t.theData", theData, pos=1)
        theData <- theData %>%
          pivot_wider(id_cols=dateCreated, names_from=major, values_from=n) %>%
          group_by(dateCreated) %>%
          rowwise() %>%
          mutate(Total=sum(cur_data(), na.rm=TRUE))


        t.names <- names(theData)
        t.include <- t.names %in% c("dateCreated", "Total")
        t.names <- t.names[!t.include]

        theData %>%
          mutate(dateCreated=as.Date(dateCreated)) %>%
          c3(x='dateCreated') %>%
          c3_mixedGeom(#type='bar',
            #stacked=t.names,
            types=list(Total='line')
          )
      })
      determineLosses <- reactive({

        lossesOutput <- NULL

        req(focalSem())
        req(refSem())
        req(deptAbbrv())
        req(chosenCourse())

          dept.name <- deptAbbrv()
          inputReferenceSemester <- refSem()
          courseNumber <- chosenCourse()

          possibleSubjects <- synonyms %>%
            filter(subject==deptAbbrv()) %>%
            unlist() %>%
            as.vector() %>%
            unique()
          options(dplyr.summarise.inform=F)

          topLosses <- previousSemestersFinalEnrollment %>%
            #filter(subject==deptAbbrv()) %>%
            filter(subject %in% possibleSubjects) %>%
            filter(course.number==chosenCourse()) %>%
            filter(semester %in% c(focalSem(),refSem())) %>%
            group_by(semester, major) %>%
            summarize(nStudents=sum(enrolledStudents)) %>%
            ungroup() %>%
            pivot_wider(id_cols=semester, names_from=major, values_from=nStudents, values_fill=0) %>%
            pivot_longer(-semester) %>%
            pivot_wider(names_from=semester, values_from=value) %>%
            mutate(diff= (!!rlang::sym(focalSem()) - (!!rlang::sym(refSem())))) %>%
            arrange(diff) %>%
            filter(diff <= 0) %>%
            slice_min(diff, n=10)

          options(dplyr.summarise.inform=T)

          lossesOutput <- topLosses %>%
            select(c("major"="name"), c("Losses" = "diff")) %>%
            c3(x="major", y="Losses", colors=list('Losses'="#3C0000")) %>%
            c3_bar()
      })
      determineGains <- reactive({
        gainsOutput <- NULL
        req(focalSem())
        req(refSem())
        req(deptAbbrv())
        req(chosenCourse())

        dept.name <- deptAbbrv()
        inputReferenceSemester <- refSem()
        courseNumber <- chosenCourse()
        possibleSubjects <- synonyms %>%
          filter(subject==deptAbbrv()) %>%
          unlist() %>%
          as.vector() %>%
          unique()
        options(dplyr.summarise.inform=F)

        topGains <- previousSemestersFinalEnrollment %>%
          #filter(subject==deptAbbrv()) %>%
          filter(subject %in% possibleSubjects) %>%
          filter(course.number==chosenCourse()) %>%
          filter(semester %in% c(focalSem(),refSem())) %>%
          group_by(semester, major) %>%
          summarize(nStudents=sum(enrolledStudents)) %>%
          ungroup() %>%
          pivot_wider(id_cols=semester, names_from=major, values_from=nStudents, values_fill=0) %>%
          pivot_longer(-semester) %>%
          pivot_wider(names_from=semester, values_from=value) %>%
          mutate(diff= (!!rlang::sym(focalSem()) - (!!rlang::sym(refSem())))) %>%
          arrange(diff) %>%
          filter(diff >= 0) %>%
          slice_max(diff, n=10)

        options(dplyr.summarise.inform=T)

        gainsOutput <- topGains %>%
          select(c("major"="name"), c("Gains" = "diff")) %>%
          c3(x="major", y="Gains", colors=list('Gains'="#3C0000")) %>%
          c3_bar()
        # #require(lubridate)
        # #require(input$referenceSemester)
        # #require(input$input$selectDept)
        # #cat("In determineGains, inputReferenceSemester:", is.null(input$referenceSemester), ".\n")
        #
        # singleCourseSummaryWithoutSections <- function(inData, courseID, theSemester){
        #   options(dplyr.summarise.inform=F)
        #   #browser()
        #   if(!useShort){
        #     outData <- inData %>%
        #       filter(course.designation == courseID) %>%
        #       filter(semester==theSemester) %>%
        #       group_by(dateCreated, major) %>%
        #       summarize(n=n())
        #   } else {
        #     outData <- inData %>%
        #       filter(course.designation == courseID) %>%
        #       filter(semester==theSemester) %>%
        #       group_by(dateCreated, major) %>%
        #       summarize(n=sum(numEnrolled))
        #   }
        #   options(dplyr.summarise.inform=T)
        #   outData
        # }
        #
        # if(!is.null(input$referenceSemester)){
        #
        #
        #   dept <- input$selectDept
        #
        #   if (as.numeric(dept) == 1) dept.name <- "ATMO"
        #   if (as.numeric(dept) == 2) dept.name <- "GEOG"
        #   if (as.numeric(dept) == 3) dept.name <- "GEOL"
        #   if (as.numeric(dept) == 4) dept.name <- "GEOS"
        #   if (as.numeric(dept) == 5) dept.name <- "OCNG"
        #   if (as.numeric(dept) == 6) dept.name <- "GEOP"
        #   if(input$choiceUG == "1") {
        #     useSelectCourse <- input$selectCourseUG
        #   } else {
        #     useSelectCourse <- input$selectCourseGrad
        #   }
        #
        #   courseNumber <- useSelectCourse
        #   inputReferenceSemester <- input$referenceSemester
        #
        #   maxDate <- as.character(max(fullRegistrationTracking$dateCreated))
        #   currentSemester <- fullRegistrationTracking %>%
        #     left_join(semester.codes, by=c("semester"="current")) %>%
        #     select("sem4") %>%
        #     unique() %>%
        #     as.character()
        #   referenceSemester <- semester.codes %>%
        #     filter(current==inputReferenceSemester) %>%
        #     select("sem4") %>%
        #     unique() %>%
        #     as.character()
        #
        #   theData <- singleCourseSummaryWithoutSections(fullRegistrationTracking, paste(dept.name, courseNumber), input$focalSemester)
        #
        #   theData1 <- theData %>%
        #     filter(dateCreated==maxDate) %>%
        #     mutate(dateCreated=currentSemester)
        #   options(dplyr.summarise.inform=F)
        #
        #   if(!useShort){
        #     theData2 <- previousSemestersFinalEnrollment %>%
        #       filter(semester== inputReferenceSemester) %>%
        #       mutate(dateCreated= referenceSemester) %>%
        #       filter(designation == paste(dept.name, courseNumber)) %>%
        #       group_by(dateCreated, major) %>%
        #       summarize(n=n()) %>%
        #       full_join(theData1, by=c("major"="major")) %>%
        #       mutate(!!currentSemester := n.y) %>%
        #       mutate(!!referenceSemester :=n.x) %>%
        #       select("major", currentSemester, referenceSemester) %>%
        #       mutate(!!currentSemester := case_when(is.na(!!rlang::sym(currentSemester)) ~ as.integer(0),
        #                                             TRUE ~ (!!rlang::sym(currentSemester)))) %>%
        #       mutate(!!referenceSemester := case_when(is.na(!!rlang::sym(referenceSemester)) ~ as.integer(0),
        #                                               TRUE ~ (!!rlang::sym(referenceSemester)))) %>%
        #       mutate(diff= (!!rlang::sym(currentSemester)) - (!!rlang::sym(referenceSemester))) %>%
        #       arrange(diff)
        #   } else {
        #     theData2 <- previousSemestersFinalEnrollment %>%
        #       filter(semester== inputReferenceSemester) %>%
        #       mutate(dateCreated= referenceSemester) %>%
        #       mutate(designation= paste(subject, course.number)) %>%
        #       filter(designation == paste(dept.name, courseNumber)) %>%
        #       group_by(dateCreated, major) %>%
        #       summarize(n=sum(enrolledStudents)) %>%
        #       full_join(theData1, by=c("major"="major")) %>%
        #       mutate(!!currentSemester := n.y) %>%
        #       mutate(!!referenceSemester :=n.x) %>%
        #       select("major", currentSemester, referenceSemester) %>%
        #       mutate(!!currentSemester := case_when(is.na(!!rlang::sym(currentSemester)) ~ as.integer(0),
        #                                             TRUE ~ (!!rlang::sym(currentSemester)))) %>%
        #       mutate(!!referenceSemester := case_when(is.na(!!rlang::sym(referenceSemester)) ~ as.integer(0),
        #                                               TRUE ~ (!!rlang::sym(referenceSemester)))) %>%
        #       mutate(diff= (!!rlang::sym(currentSemester)) - (!!rlang::sym(referenceSemester))) %>%
        #       arrange(diff)
        #   }
        #
        #   options(dplyr.summarise.inform=T)
        #
        #   topGains <- theData2[c((nrow(theData2)-10): nrow(theData2)),]
        #
        #
        #   gainsOutput <- topGains %>%
        #     select("major", c("Gains" = "diff")) %>%
        #     c3(x="major", y="Gains", colors=list('Gains'="#3C0000")) %>%
        #     c3_bar()
        # } else {
        #   gainsOutput <- NULL
        # }
        gainsOutput
      })

      return(vals)
    }
  )
}
