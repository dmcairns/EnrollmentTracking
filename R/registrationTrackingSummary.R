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
#'
#' @return
#' @export
#'
#' @examples
registrationTrackingSummaryServer <- function(id, inTrackingData, ppData, semester.codes) {
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
        req(inTrackingData$focalSemester)
        req(inTrackingData$referenceSemester)
        fSem <- semester.codes %>%
          filter(current==inTrackingData$focalSemester) %>%
          select("semester.display") %>%
          unlist() %>%
          as.vector()

        rSem <- semester.codes %>%
          filter(current==inTrackingData$referenceSemester) %>%
          select("semester.display") %>%
          unlist() %>%
          as.vector()
        theTitle <- paste0("Percent Change in Course Enrollments between ", fSem, " and ", rSem)
        theTitle
      })
      output$courseEnrollmentChange <- renderC3({
        #cat("In courseEnrollmentChange1\n")
        req(inTrackingData$focalSemester)
        req(inTrackingData$referenceSemester)
        semesterComparisonData <- createSemesterComparisonData(ppData, focalSem=inTrackingData$focalSemester,
                                                               refSem=inTrackingData$referenceSemester)
        if(nrow(semesterComparisonData)>0) {
          scd3 <- semesterComparisonData %>%
            filter(focalDateCreated==max(focalDateCreated)) %>%
            #column_to_rownames("courseChr") %>%
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
          #assign("scd3.data", scd3, pos=1)
          #assign("scd3.plot", theOutput, pos=1)
        } else {
          theOutput <- NULL
        }
        theOutput

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
                                               inDept="GEOG", focalSem=202131,
                                               refSem=202031){

        processData <- function(inData, inDept){
          outData <- inData %>%
            group_by(semester, subject, courseNumber, dateCreated) %>%
            summarize(semester, subject, courseNumber, dateCreated, nReg=n(), .groups="drop") %>%
            filter(subject==inDept) %>%
            unique()
          outData
        }

        combineSemesters <- function(inData, focalSem, refSem, inStartDates){
          #assign("inData1", inData, pos=1)
          #assign("t.focalSem", focalSem, pos=1)
          #assign("t.refSem", refSem, pos=1)
          #cat(yellow("before midData\n"))

          # what are the characteristics of inData that lead to an error?
          #cat(green("a nrow(inData):", nrow(inData), "\n"))
          #cat(green("focalSem:", focalSem, "\n"))
          #cat(green("something\n"))
          midData <- inData %>%
            mutate(temp1=difftime(dateCreated, startDate, units="days")) %>%
            mutate(temp2=as.numeric(str_extract(temp1, "\\w+"))) %>%
            mutate(sign=case_when(substr(temp1,1,1)=="-" ~ -1,
                                  TRUE ~ 1)) %>%
            mutate(daysBefore=temp2*sign)

          focalData <- midData %>%
            filter(semester==focalSem)
          refData <- midData %>%
            filter(semester==refSem)
          #cat(blue("after filters\n"))
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

        #cat(blue("1before combineSemesters\n"))
        midData <- combineSemesters(inData, focalSem, refSem, inClassesStart)
        #cat(blue("1after combineSemesters\n"))
        midData
      }


    }
  )
}

