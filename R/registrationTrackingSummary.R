#' registrationTrackingSummaryUI
#'
#' @param id the id
#' @param label  the label
#'
#' @return
#' @export
#'
#' @examples
registrationTrackingSummaryUI <- function(id, label = "Registration Tracking") {
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=FALSE,
                   solidHeader=TRUE,
                   htmltools::div(class="aBoxBodyC3Background",
                                  tagList(
                                    #textOutput(ns("someText")),
                                    #verbatimTextOutput(ns("someTextVerbatim")),
                                    #c3Output(ns("distPlotC3"))
                                    c3Output(ns("courseEnrollmentChange"))
                                  )
                   ),
                   id=ns("comparisonBox"),
                   class="schBoxTestOct"
                 ))

}

#' registrationTrackingSummaryServer
#'
#' @param id the id
#' @param inTrackingData the tracking data
#' @param ppData the ppData
#' @param semester.codes the semester codes
#' @param focalSem focalSem
#' @param refSem refSem
#' @param deptAbbrv deptAbbrv
#' @param classesStart classesStart
#' @param synonyms synonyms
#'
#' @return
#' @export
#'
#' @examples
registrationTrackingSummaryServer <- function(id, inTrackingData, ppData, semester.codes,
                                              focalSem,
                                              refSem,
                                              deptAbbrv,
                                              classesStart,
                                              synonyms) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      #########################
      # Global Variables      #
      # within this module    #
      #########################

      rv1 <- reactiveVal("rv1")

      #########################
      # Input Control UIs     #
      #########################


      #########################
      # Card Elements         #
      #########################

      output$someText <- renderText({
        theOutput <- paste("focalSemester:", rvFocalSemester())
      })
      output$someTextVerbatim <- renderText({
        theOutput <- paste0("The focalSemester:", inTrackingData$focalSemester)

      })
      output$distPlotC3 <- renderC3({
        data <- data.frame(a = abs(rnorm(20) * 10),
                           b = abs(rnorm(20) * 10),
                           date = seq(as.Date("2014-01-01"), by = "month", length.out = 20))

        c3(data)
      })
      output$plotTitle <- renderText({
        ns <- session$ns

        req(focalSem())
        req(refSem())
        fSem <- semester.codes %>%
          filter(current==focalSem()) %>%
          select("semester.display") %>%
          unlist() %>%
          as.vector()

        rSem <- semester.codes %>%
          filter(current==refSem()) %>%
          select("semester.display") %>%
          unlist() %>%
          as.vector()
        theTitle <- paste0("Percent Change in Course Enrollments between ", fSem, " and ", rSem)
        theTitle
      })
      output$courseEnrollmentChange <- renderC3({
        make.comparison.figure()
      })

      #########################
      # Observer Functions    #
      #########################



      #########################
      # Data processing and   #
      # preparation functions #
      #########################


      createSemesterComparisonData <- function(inData, oldReferenceData=NULL,
                                               inClassesStart=classesStart,
                                               inDept, focalSem=202131,
                                               refSem=202031){
        #browser()

        processData <- function(inData, inDept){
          #browser()
          outData <- inData %>%
            group_by(semester, subject, courseNumber, dateCreated) %>%
            summarize(semester, subject, courseNumber, dateCreated, nReg=n(), .groups="drop") %>%
            filter(subject==inDept) %>%
            unique()

          outData
        }

        combineSemesters <- function(inData, focalSem, refSem, inStartDates){

          ################################################
          # inStartDates is not used at all.  Delete?    #
          # or join with inData to create fill startDate #
          # field?                                       #
          ################################################
#browser()
          possibleSubjects <- synonyms %>%
            filter(subject==deptAbbrv()) %>%
            unlist() %>%
            as.vector() %>%
            unique()

          inData <- inData %>%
            #filter(subject==deptAbbrv()) %>%
            filter(subject %in% possibleSubjects) %>%
            group_by(semester, subject, dateCreated, courseNumber) %>%
            summarise(nReg=sum(numEnrolled), .groups="drop") %>%
            mutate(subject=case_when(subject=="PSYC" ~ "PBSI",
                                     TRUE ~ subject))

          inData <- inData %>%
            left_join(inStartDates, by=c("semester")) %>%
            select("semester", "subject", "courseNumber", "dateCreated", "nReg", "startDate")

          midData <- inData %>%
            mutate(temp1=difftime(dateCreated, startDate, units="days")) %>%
            mutate(temp2=as.numeric(str_extract(temp1, "(-*\\d+(?>\\.\\d+)*)"))) %>%
            mutate(daysBefore=floor(temp2))

          focalData <- midData %>%
            filter(semester==focalSem)
          refData <- midData %>%
            filter(semester==refSem)

          outData <- focalData %>%
            full_join(refData, by=c("subject", "courseNumber", "daysBefore")) %>%
            select("subject", "courseNumber", "focalReg"="nReg.x", "daysBefore", "refReg"="nReg.y",
                   "focalSem"="semester.x", "focalDateCreated"="dateCreated.x",
                   "refSem"="semester.y", "refDateCreated"="dateCreated.y") %>%
            filter(!is.na(focalReg)) %>%
            filter(!is.na(refReg)) %>%
            mutate(diffReg=focalReg-refReg) %>%
            mutate(pctChange=diffReg/refReg*100) %>%
            mutate(courseChr=paste(subject, courseNumber))
          outData
        }
        midData <- combineSemesters(inData, focalSem, refSem, inClassesStart)

        midData
      }

      make.comparison.figure <- reactive({
        req(focalSem())
        req(refSem())

        semesterComparisonData <- createSemesterComparisonData(ppData, focalSem=focalSem(),
                                                               refSem=refSem(),
                                                               inDept=deptAbbrv())


        if(nrow(semesterComparisonData)>0) {
          scd3 <- semesterComparisonData %>%
            filter(focalDateCreated==max(focalDateCreated)) %>%
            arrange(-pctChange) %>%
            select(-"daysBefore") %>%
            select("courseChr", "Percent Change"="pctChange") %>%
            mutate(displayColor=case_when('Percent Change' >= 0 ~ "#500000",
                                          TRUE ~ "#5B6236"))

          theOutput <- scd3 %>%
            c3(x="courseChr", y='Percent Change', colors=list('Percent Change'="#003C71")) %>%
            c3_bar(rotated=TRUE) %>%
            legend(hide=TRUE) %>%
            tickAxis('x',count=2, outer=FALSE) %>%
            c3_chart_size(width=500, height=600)
        } else {
          theOutput <- NULL
        }
        theOutput

      })

    }
  )
}

