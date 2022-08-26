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
                   collapsible=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("bannerTest")),
                     width=40,
                     background="#998542",
                     id=ns("sidebarControls"),
                     class="testSidebar"
                   ),
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
#'
#' @return
#' @export
#'
#' @examples
registrationTrackingOrigServer <- function(id, deptAbbrv, previousSemestersFinalEnrollment, fullRegistrationTracking, useShort=FALSE,
                                           semester.codes) {
  moduleServer(
    id,
    ## Below is the module function

    function(input, output, session) {
      #########################
      # Global Variables      #
      # within this module    #
      #########################
      classesStart <- data.frame(semester=c("202031", "202111", "202121", "202131", "202211", "202221", "202231", "202311", "202321"),
                                 startDate=as.Date(c("2020-08-19", "2021-01-19", "2021-06-01", "2021-08-30", "2022-01-18", "2022-05-31", "2022-08-24", "2023-01-17", "2023-05-30")))

      #########################
      # Reactive Variables    #
      #########################
      vals <- reactiveValues()

      observe({vals$focalSemester <- input$focalSemester})
      observe({vals$referenceSemester <- input$referenceSemester})


      observe({vals$Department <- input$selectDept})
      observe({vals$courseNum <- input$selectCourseUG})
      observe({vals$synchronize <- input$synchSwitch})
      observe({vals$syncTerms <- input$sameTermSwitch})
      rvFocalSemester <- reactiveVal(NA)
      rvReferenceSemester <- reactiveVal(NA)
      rvDepartment <- reactiveVal(NA)
      rvCourse <- reactiveVal(NA)


      #########################
      # Input Control UIs     #
      #########################
      output$selectDeptControl <- renderUI({
        ns <- session$ns
        if(deptAbbrv == "ATMO") {
          matchSelected=1
        }
        if(deptAbbrv == "GEOG") {
          matchSelected=2
        }
        if(deptAbbrv == "GEOL") {
          matchSelected=3
        }
        if(deptAbbrv == "GEOS") {
          matchSelected=4
        }
        if(deptAbbrv == "GEOP") {
          matchSelected=6
        }
        if(deptAbbrv == "OCNG") {
          matchSelected=5
        }
        t.out <- selectInput(ns("selectDept"),
                             label="Course Prefix",
                             choices=list("ATMO"=1, "GEOG"=2, "GEOL"=3, "GEOP"=6, "GEOS"=4, "OCNG"=5),
                             selected=matchSelected)


        t.out
      })
      output$choiceUGControl <- renderUI({
        ns <- session$ns
        t.out <- radioButtons(ns("choiceUG"),
                              "Course Level",
                              c("Undergraduate"=1, "Graduate"=2))


        t.out
      })
      output$ugcourses <- renderUI({
        ns <- session$ns
        dept.code <- input$selectDept
        radioButtons(ns("selectCourseUG"),
                     label=h4("Courses"),
                     choices=create.ug.course.list(dept.code))
      })
      output$referenceSemester <- renderUI({
        ns <- session$ns
        req(vals$syncTerms)
        #cat(green("[referenceSemester] vals$syncTerms:", vals$syncTerms, "\n"))

        previousSemestersFinalEnrollment <- previousSemestersFinalEnrollment %>%
          mutate(semester=as.character(semester)) %>%
          left_join(semester.codes, by=c("semester"="current")) %>%
          select("semester", "longSemester", "semester4") %>%
          unique() %>%
          mutate(semester=as.numeric(semester))
        currentSemester <- as.character(unique(fullRegistrationTracking$semester))
        currentSemester <- max(as.numeric(currentSemester))
        if(vals$syncTerms==TRUE) {
          initialChoice <- currentSemester - 100
          theOutput <- selectInput(ns("referenceSemester"),
                                   label="Reference Semester",
                                   choices=previousSemestersFinalEnrollment$semester,
                                   selected=as.character(initialChoice))
        } else {
          initialChoice <- currentSemester - 100

          theOutput <- selectInput(ns("referenceSemester"),
                                   label="Reference Semester",
                                   choices=previousSemestersFinalEnrollment$semester,
                                   selected=as.character(initialChoice))
        }
        theOutput
      })
      output$focalSemester <- renderUI({
        ns <- session$ns

        currentSemester <- as.character(unique(fullRegistrationTracking$semester))
        currentSemester <- sort(as.numeric(currentSemester))
        selectInput(ns("focalSemester"),
                    label="Focal Semester",
                    choices=currentSemester,
                    selected=max(currentSemester))

      })
      output$synchronize <- renderUI({
        ns <- session$ns
        materialSwitch(ns("synchSwitch"), value=TRUE, label="Synchronize")
      })
      output$gradcourses <- renderUI({
        ns <- session$ns
        dept.code <- input$selectDept
        radioButtons(ns("selectCourseGrad"),
                     label=h4("Courses"),
                     choices=create.grad.course.list(dept.code))
      })
      output$sameTerm <- renderUI({
        ns <- session$ns
        materialSwitch(ns("sameTermSwitch"), value=TRUE, label="Same Term:")
      })
      output$courseList <- renderUI({
        ns <- session$ns
        req(input$choiceUG)
        if(input$choiceUG == 1){
          t.out <- uiOutput(ns("ugcourses"))
        } else {
          t.out <- uiOutput(ns("gradcourses"))
        }
        t.out
      })

      #########################
      # Card Elements         #
      #########################
      output$plotTitle <- renderText({

        ns <- session$ns
        req(input$selectDept)

        if(input$selectDept == 1) {
          selectedDept <- "ATMO"
        }
        if(input$selectDept == 2) {
          selectedDept <- "GEOG"
        }
        if(input$selectDept == 3) {
          selectedDept <- "GEOL"
        }
        if(input$selectDept == 4) {
          selectedDept <- "GEOS"
        }
        if(input$selectDept == 6) {
          selectedDept <- "GEOP"
        }
        if(input$selectDept == 5) {
          selectedDept <- "OCNG"
        }
        if(input$choiceUG==1) {
          courseNumber <- input$selectCourseUG
        } else {
          courseNumber <- input$selectCourseGrad
        }
        theTitle <- paste0(selectedDept, " ", courseNumber, " Registration Tracking")
        theTitle
      })
      output$plotSubTitle <- renderUI({
        ns <- session$ns
        req(input$selectDept)


        if(input$selectDept == 1) {
          selectedDept <- "ATMO"
        }
        if(input$selectDept == 2) {
          selectedDept <- "GEOG"
        }
        if(input$selectDept == 3) {
          selectedDept <- "GEOL"
        }
        if(input$selectDept == 4) {
          selectedDept <- "GEOS"
        }
        if(input$selectDept == 6) {
          selectedDept <- "GEOP"
        }
        if(input$selectDept == 5) {
          selectedDept <- "OCNG"
        }

        if(input$choiceUG == 1){
          t.out <- h5(paste(selectedDept, input$selectCourseUG))
        } else {
          t.out <-  h5(paste(selectedDept, input$selectCourseGrad))
        }
        t.out
      })
      output$bannerTest <- renderUI({
        ns <- session$ns
        tagList(
          fluidRow(tagList(
            column(4, div(id="bannerComponent1", class="bannerComponent",
                          uiOutput(ns("selectDeptControl")))),
            column(4, div(id="bannerComponent3", class="bannerComponent",
                          uiOutput(ns("focalSemester")))),
            column(4, div(id="bannerComponent4", class="bannerComponent",
                          uiOutput(ns("referenceSemester"))))
          )
          ),
          fluidRow(tagList(
            div(id="bannerComponent2", class="bannerComponent",
                uiOutput(ns("choiceUGControl"))),
            div(id="bannerComponent6", class="bannerComponent",
                uiOutput(ns("synchronize"))),
            div(id="bannerComponent7", class="bannerComponent",
                uiOutput(ns("sameTerm")))
          )),
          fluidRow(
            column(12, tagList(
              div(id="bannerComponent5", class="multiColumnBannerComponent",
                  uiOutput(ns("courseList")))
            ))
          )
        )
      })

      output$entireUI <- renderUI({
        ns <- session$ns
        tabsetPanel(type="tabs",
                    tabPanel("Registered Students",
                             fluidRow(conditionalPanel(condition="2 > 0", c3Output(ns("distPlotC3")))),

                    ),
                    tabPanel("Major Demographics",
                             conditionalPanel(condition="2 > 0", c3Output(ns("plotC3Majors"))),
                    ),
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
        #cat(green("in distPlotC3\n"))
        make.plotC3()
      })
      output$plotC3Majors <- renderC3({
        cat(green("in plotC3Majors\n"))
        make.plotMajorsC3()
      })
      output$plotC3Sections <- renderC3({
        cat(green("[plotC3Sections]: useShort:", useShort, "\n"))
        make.plotSectionsC3()
      })
      output$plotLossesC3 <- renderC3({
        cat(green("in plotLossesC3\n"))
        determineLosses()
      })
      output$plotGainesC3 <- renderC3({
        cat(green("in plotGainsC3\n"))
        determineGains()
      })


      #########################
      # Observer Functions    #
      #########################
      observeEvent(input$modalActivationButton, {
        #toggleCssClass does not work if plotConrolsDiv is wrapped
        #in an ns()
        toggleCssClass(id="controlsBanner", class="visible")
      })
      observeEvent(input$focalSemester, {
        #cat(blue("input$focalSemester has been changed\n"))
        rvFocalSemester(input$focalSemester)
      })


      #########################
      # Data processing and   #
      # preparation functions #
      #########################
      create.ug.course.list <- function(dept){
        # print("In create.ug.course.list")
        if (as.numeric(dept) == 1) dept.name <- "ATMO"
        if (as.numeric(dept) == 2) dept.name <- "GEOG"
        if (as.numeric(dept) == 3) dept.name <- "GEOL"
        if (as.numeric(dept) == 4) dept.name <- "GEOS"
        if (as.numeric(dept) == 5) dept.name <- "OCNG"
        if (as.numeric(dept) == 6) dept.name <- "GEOP"
        #t.data <- clge.enrollment.data

        t.data <- fullRegistrationTracking %>%
          mutate(courseNumber=as.character(courseNumber)) %>%
          mutate(courseNumber=as.numeric(courseNumber)) %>%
          filter(subject==dept.name) %>%
          filter(courseNumber < 500) %>%
          select("courseNumber") %>%
          unlist() %>%
          as.vector() %>%
          unique() %>%
          sort()

        t.data
      }

      create.grad.course.list <- function(dept){
        # print("In create.grad.course.list")
        if (as.numeric(dept) == 1) dept.name <- "ATMO"
        if (as.numeric(dept) == 2) dept.name <- "GEOG"
        if (as.numeric(dept) == 3) dept.name <- "GEOL"
        if (as.numeric(dept) == 4) dept.name <- "GEOS"
        if (as.numeric(dept) == 5) dept.name <- "OCNG"
        if (as.numeric(dept) == 6) dept.name <- "GEOP"
        t.data <- fullRegistrationTracking %>%
          mutate(courseNumber=as.character(courseNumber)) %>%
          mutate(courseNumber=as.numeric(courseNumber)) %>%
          filter(subject==dept.name) %>%
          filter(courseNumber >= 500) %>%
          select("courseNumber") %>%
          unlist() %>%
          as.vector() %>%
          unique() %>%
          sort()

        t.data
      }

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
        req(input$referenceSemester)
        req(input$focalSemester)
        combineFocalAndReferenceData <- function(inFocalData, inReferenceData){
          req(inFocalData)
          req(inReferenceData)
          #browser()
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
          #browser()
          #cat(yellow("In combineFocalandReferenceData\n"))
          referenceSemester <- input$referenceSemester
          focalSemester <- input$focalSemester
          if(referenceSemester %in% classesStart$semester){
            refStartDate <- classesStart %>%
              filter(semester==referenceSemester) %>%
              select("startDate") %>%
              format(., "%Y-%m-%d") %>%
              as.character()
          } else {
            refStartDate <- NULL
          }

          #assign("refStartDate", refStartDate, pos=1)
          #assign("inReferenceData", inReferenceData, pos=1)
          #cat("refStartDate:", refStartDate, "\n")
          #assign("classesStart", classesStart, pos=1)
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

            #cat("There is no referenceStartDate\n")
          } else {
            #cat("There is a referenceStartDate\n")
            t.rd1 <- inReferenceData %>%
              mutate(temp1=difftime(dateCreated, refStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) #

            #cat(yellow("before assign(t.rd1a)\n"))
            #assign("t.rd1a", t.rd1, pos=1)
            t.rd1 <- t.rd1 %>%
              select("nRef"="n", "daysBefore")
            #assign("t.fd1b", inFocalData, pos=1)
            t.fd1 <- inFocalData %>%
              mutate(temp1=difftime(dateCreated, focalStartDate, units="days")) %>%
              mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
              mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                    TRUE ~ 1)) %>%
              mutate(daysBefore=temp2*sign) #

            #cat(yellow("before assign(t.fd1a)\n"))
            #assign("t.fd1a", t.fd1, pos=1)
            t.fd1 <- t.fd1 %>%
              select("dateCreated", "n", "daysBefore") %>%
              full_join(t.rd1, by=c("daysBefore"))
            #cat("created t.rd1 and t.fd1\n")
          }

          #print(t.fd1)
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

        focalData <- singleCourseSummaryWithoutSections(inData, paste(dept.name, useSelectCourse), input$focalSemester, useShort=useShort)
        referenceData <- singleCourseSummaryWithoutSections(inData, paste(dept.name, useSelectCourse), input$referenceSemester, useShort=useShort)

        if(is.null(referenceData)){
          cat("Reference data don't exist as a time series.\n")
          referenceData <- focalData
          referenceData$n <- oldReferenceData

        }

        #assign("t.fd", focalData, pos=1)
        #assign("t.rd", referenceData, pos=1)

        combinedData <- combineFocalAndReferenceData(focalData, referenceData)

        #assign("t.cd", combinedData, pos=1)
        combinedData
      }

      make.plotC3 <- reactive({
        req(input$referenceSemester)
        req(input$focalSemester)

        singleCourseSummaryWithoutSections <- function(inData, courseID, theSemester){
          options(dplyr.summarise.inform=F)
          outData <- inData %>%
            filter(course.designation == courseID) %>%
            filter(semester == theSemester) %>%
            group_by(dateCreated) %>%
            summarize(n=n())
          options(dplyr.summarise.inform=F)
          outData
        }
        getReferenceData <- function(inData, courseID, referenceSemester){
          #cat(green("in getReferenceData\n"))
          options(dplyr.summarise.inform=F)
          ###### This function converts the long form (single entry for each student)
          ###### data to a count format (using the n() function.)
          referenceData <- inData %>%
            filter(semester==referenceSemester) %>%
            filter(designation==courseID) %>%
            group_by(designation) %>%
            summarise(n=n(), .groups="drop")
          options(dplyr.summarise.inform=T)

          referenceData$n
        }
        getReferenceDataShort <- function(inData, courseID, referenceSemester){
          options(dplyr.summarise.inform=F)
          ###### This function ingests the short form of the data (counts, not single line for each student)
          ###### Need to ensure that previousSemestersFinalEnrollment (inDat) is in the correct format
          ###### if pass the giantDataBall$outCourseEnrollmentRcourseEnrollmentData it will be fine.
          referenceData <- inData %>%
            filter(semester==referenceSemester) %>%
            mutate(designation=paste(subject, course.number)) %>%
            filter(designation==courseID)

          options(dplyr.summarise.inform=T)
          referenceData$enrolledStudents
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

        if(!useShort)
          theReferenceData <- getReferenceData(previousSemestersFinalEnrollment, paste(dept.name, useSelectCourse), input$referenceSemester)
        else theReferenceData <- getReferenceDataShort(previousSemestersFinalEnrollment, paste(dept.name, useSelectCourse), input$referenceSemester)


        t.data <- createFullDataForFigure(fullRegistrationTracking, theReferenceData, useShort)
#browser()
        ##########################################
        # convert input$referenceSemester to a   #
        # format suitable as a column name       #
        ##########################################
        refSemester <- semester.codes %>%
          filter(current==input$referenceSemester) %>%
          select(semester4) %>%
          unlist() %>%
          as.vector() %>%
          as.character()
        focSemester <- semester.codes %>%
          filter(current==input$focalSemester) %>%
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

      make.plotSectionsC3 <- reactive({
        singleCourseSummaryWithSections <- function(inData, courseID, theSemester){
          options(dplyr.summarise.inform=F)
          if(!useShort){
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, courseSection) %>%
              summarize(n=n())
          } else {
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, courseSection) %>%
              summarize(n=sum(numEnrolled))
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

        theData <- singleCourseSummaryWithSections(fullRegistrationTracking, paste(dept.name, useSelectCourse), input$focalSemester)
        theData <- theData %>%
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
        #require(lubridate)
        #req(input$referenceSemester)
        #req(input$input$selectDept)
        #cat(green("[determineLosses] useShort:", useShort, "\n"))

        singleCourseSummaryWithoutSections <- function(inData, courseID, theSemester){
          options(dplyr.summarise.inform=F)
          if(!useShort){
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, major) %>%
              summarize(n=n())
          } else {
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, major) %>%
              summarize(n=sum(numEnrolled))
          }
          options(dplyr.summarise.inform=T)
          outData
        }

        if(!is.null(input$referenceSemester)){


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

          courseNumber <- useSelectCourse
          inputReferenceSemester <- input$referenceSemester

          maxDate <- as.character(max(fullRegistrationTracking$dateCreated))
          currentSemester <- fullRegistrationTracking %>%
            left_join(semester.codes, by=c("semester"="current")) %>%
            select("sem4") %>%
            unique() %>%
            as.character()
          referenceSemester <- semester.codes %>%
            filter(current==inputReferenceSemester) %>%
            select("sem4") %>%
            unique() %>%
            as.character()

          #print(names(fullRegistrationTracking))
          theData <- singleCourseSummaryWithoutSections(fullRegistrationTracking, paste(dept.name, courseNumber), input$focalSemester)
          #cat(green("[determineLosses] past singleCourseSummaryWithoutSections\n"))
          theData1 <- theData %>%
            filter(dateCreated==maxDate) %>%
            mutate(dateCreated=currentSemester)
          options(dplyr.summarise.inform=F)
          #cat("before print, useShort:", useShort, "\n")
          #print(names(previousSemestersFinalEnrollment))
          if(!useShort){
            theData2 <- previousSemestersFinalEnrollment %>%
              filter(semester== inputReferenceSemester) %>%
              mutate(dateCreated= referenceSemester) %>%
              filter(designation == paste(dept.name, courseNumber)) %>%
              group_by(dateCreated, major) %>%
              summarize(n=n()) %>%
              full_join(theData1, by=c("major"="major")) %>%
              mutate(!!currentSemester := n.y) %>%
              mutate(!!referenceSemester :=n.x) %>%
              select("major", currentSemester, referenceSemester) %>%
              mutate(!!currentSemester := case_when(is.na(!!rlang::sym(currentSemester)) ~ as.integer(0),
                                                    TRUE ~ (!!rlang::sym(currentSemester)))) %>%
              mutate(!!referenceSemester := case_when(is.na(!!rlang::sym(referenceSemester)) ~ as.integer(0),
                                                      TRUE ~ (!!rlang::sym(referenceSemester)))) %>%
              mutate(diff= (!!rlang::sym(currentSemester)) - (!!rlang::sym(referenceSemester))) %>%
              arrange(diff)
          } else {
            theData2 <- previousSemestersFinalEnrollment %>%
              filter(semester== inputReferenceSemester) %>%
              mutate(dateCreated= referenceSemester) %>%
              mutate(designation= paste(subject, course.number)) %>%
              filter(designation == paste(dept.name, courseNumber)) %>%
              group_by(dateCreated, major) %>%
              summarize(n=sum(enrolledStudents)) %>%
              full_join(theData1, by=c("major"="major")) %>%
              mutate(!!currentSemester := n.y) %>%
              mutate(!!referenceSemester :=n.x) %>%
              select("major", currentSemester, referenceSemester) %>%
              mutate(!!currentSemester := case_when(is.na(!!rlang::sym(currentSemester)) ~ as.integer(0),
                                                    TRUE ~ (!!rlang::sym(currentSemester)))) %>%
              mutate(!!referenceSemester := case_when(is.na(!!rlang::sym(referenceSemester)) ~ as.integer(0),
                                                      TRUE ~ (!!rlang::sym(referenceSemester)))) %>%
              mutate(diff= (!!rlang::sym(currentSemester)) - (!!rlang::sym(referenceSemester))) %>%
              arrange(diff)
          }
          options(dplyr.summarise.inform=T)
          topLosses <- theData2[1:10,c(1,4)]

          lossesOutput <- topLosses %>%
            select("major", c("Losses" = "diff")) %>%
            c3(x="major", y="Losses", colors=list('Losses'="#3C0000")) %>%
            c3_bar()
        } else {
          lossesOutput <- NULL
        }


      })
      determineGains <- function(){
        #require(lubridate)
        #require(input$referenceSemester)
        #require(input$input$selectDept)
        #cat("In determineGains, inputReferenceSemester:", is.null(input$referenceSemester), ".\n")

        singleCourseSummaryWithoutSections <- function(inData, courseID, theSemester){
          options(dplyr.summarise.inform=F)
          if(!useShort){
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, major) %>%
              summarize(n=n())
          } else {
            outData <- inData %>%
              filter(course.designation == courseID) %>%
              filter(semester==theSemester) %>%
              group_by(dateCreated, major) %>%
              summarize(n=sum(numEnrolled))
          }
          options(dplyr.summarise.inform=T)
          outData
        }

        if(!is.null(input$referenceSemester)){


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

          courseNumber <- useSelectCourse
          inputReferenceSemester <- input$referenceSemester

          maxDate <- as.character(max(fullRegistrationTracking$dateCreated))
          currentSemester <- fullRegistrationTracking %>%
            left_join(semester.codes, by=c("semester"="current")) %>%
            select("sem4") %>%
            unique() %>%
            as.character()
          referenceSemester <- semester.codes %>%
            filter(current==inputReferenceSemester) %>%
            select("sem4") %>%
            unique() %>%
            as.character()

          theData <- singleCourseSummaryWithoutSections(fullRegistrationTracking, paste(dept.name, courseNumber), input$focalSemester)

          theData1 <- theData %>%
            filter(dateCreated==maxDate) %>%
            mutate(dateCreated=currentSemester)
          options(dplyr.summarise.inform=F)

          if(!useShort){
            theData2 <- previousSemestersFinalEnrollment %>%
              filter(semester== inputReferenceSemester) %>%
              mutate(dateCreated= referenceSemester) %>%
              filter(designation == paste(dept.name, courseNumber)) %>%
              group_by(dateCreated, major) %>%
              summarize(n=n()) %>%
              full_join(theData1, by=c("major"="major")) %>%
              mutate(!!currentSemester := n.y) %>%
              mutate(!!referenceSemester :=n.x) %>%
              select("major", currentSemester, referenceSemester) %>%
              mutate(!!currentSemester := case_when(is.na(!!rlang::sym(currentSemester)) ~ as.integer(0),
                                                    TRUE ~ (!!rlang::sym(currentSemester)))) %>%
              mutate(!!referenceSemester := case_when(is.na(!!rlang::sym(referenceSemester)) ~ as.integer(0),
                                                      TRUE ~ (!!rlang::sym(referenceSemester)))) %>%
              mutate(diff= (!!rlang::sym(currentSemester)) - (!!rlang::sym(referenceSemester))) %>%
              arrange(diff)
          } else {
            theData2 <- previousSemestersFinalEnrollment %>%
              filter(semester== inputReferenceSemester) %>%
              mutate(dateCreated= referenceSemester) %>%
              mutate(designation= paste(subject, course.number)) %>%
              filter(designation == paste(dept.name, courseNumber)) %>%
              group_by(dateCreated, major) %>%
              summarize(n=sum(enrolledStudents)) %>%
              full_join(theData1, by=c("major"="major")) %>%
              mutate(!!currentSemester := n.y) %>%
              mutate(!!referenceSemester :=n.x) %>%
              select("major", currentSemester, referenceSemester) %>%
              mutate(!!currentSemester := case_when(is.na(!!rlang::sym(currentSemester)) ~ as.integer(0),
                                                    TRUE ~ (!!rlang::sym(currentSemester)))) %>%
              mutate(!!referenceSemester := case_when(is.na(!!rlang::sym(referenceSemester)) ~ as.integer(0),
                                                      TRUE ~ (!!rlang::sym(referenceSemester)))) %>%
              mutate(diff= (!!rlang::sym(currentSemester)) - (!!rlang::sym(referenceSemester))) %>%
              arrange(diff)
          }

          options(dplyr.summarise.inform=T)

          topGains <- theData2[c((nrow(theData2)-10): nrow(theData2)),]


          gainsOutput <- topGains %>%
            select("major", c("Gains" = "diff")) %>%
            c3(x="major", y="Gains", colors=list('Gains'="#3C0000")) %>%
            c3_bar()
        } else {
          gainsOutput <- NULL
        }
        gainsOutput
      }

      return(vals)
    }
  )
}
