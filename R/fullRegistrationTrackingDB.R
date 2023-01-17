#' fullRegistrationTrackingExternalDBBoxModuleUI
#'
#' @param id the ID
#'
#' @return
#' @export
#'
#' @examples
fullRegistrationTrackingExternalDBBoxModuleUI <- function(id){
  ns <- NS(id)
  shinydashboardPlus::box(
    id = "enrollmentBox",
    title = "Enrollment",
    closable = TRUE,
    width = 12,
    height = "500px",
    solidHeader = FALSE,
    collapsible = FALSE,
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = system.file("extdata", "DashboardModularized.css", package = "EnrollmentTracking"))
      ),
      fluidRow(
        column(8, tagList(

          # fluidRow(
          #   registrationTrackingOrigUI(ns("registrationTrackingGEOG"))
          # ),
          fluidRow(
            historicEnrollmentUI(ns("historicEnrollment"))
          ))),
        column(4, tagList(

          # fluidRow(
          #   tagList(
          #     registrationTrackingSummaryUI(ns("t1"))
          #   )
          # )
        )))
    ),
    sidebar = boxSidebar(
      id = "enrollmentSidebar",                #make sure that this is unique
      width = 25,                              #width can be no smaller than 25%
      uiOutput(ns("enrollmentSidebar"))
    )
  )
}

#' fullRegistrationTrackingExternalDBBoxModuleServer
#'
#' @param id the ID
#' @param input the input
#' @param output the output
#' @param session the session
#' @param dbConn the connection object for the external database
#' @param classesStart dates that classes start by semester
#' @param registrationDataBundle used to access full registration tracking until can be fixed
#'
#' @return
#' @export
#'
#' @examples
fullRegistrationTrackingExternalDBBoxModuleServer <- function(id, input, output, session, dbConn, classesStart, registrationDataBundle){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      ###############################################
      # Retrieve data from external DB              #
      #     semester.codes                          #
      #     courseEnrollmentData                    #
      #                                             #
      ###############################################
      library(RPostgres)
      library(DBI)
      library(dbplyr)

      Sys.setenv(PGGSSENCMODE="disable")

      fullRegistrationTracking <- reactiveValues(theData=NULL)

      # con <- dbConnect(RPostgres::Postgres(),
      #                  dbname = 'mcairns/EnrollmentTracking', # database name
      #                  host = 'db.bit.io',
      #                  port = 5432,
      #                  user = 'mcairns',
      #                  password = "v2_3v5SD_cRWEpZn8CMgLfU4hNCbaXaH")

      #query <- "SELECT * FROM t202221 WHERE subject = 'GEOG'"
      query <- "SELECT * FROM semestercodes"
      semesterCodes <- DBI::dbGetQuery(dbConn, sql(query))
      print(dim(semesterCodes))

      query <- "SELECT * FROM courseenrollmentdata"
      courseEnrollmentData <- DBI::dbGetQuery(dbConn, sql(query))
      print(dim(courseEnrollmentData))


      #dbDisconnect(con)
      ###############################################
      # Add change of department code for PSYC      #
      # to PBSI                                     #
      ###############################################


      # registrationDataBundle has been removed from the call to Server
      # data must be imported from an external DB (connection passed in as dbConn)

      departmentSynonyms <- courseEnrollmentData %>%
        mutate(synonymCode=subject) %>%
        select("subject", "synonymCode") %>%
        unique() %>%
        mutate(synonymCode=case_when(subject=="PBSI" ~ "PSYC",
                                     subject=="PSYC" ~ "PBSI",
                                     TRUE ~ subject))


      semester.codes <- semesterCodes
      # possibleSubjects <- sort(unique(registrationDataBundle$outRegistrationTracking$fullRegistrationTracking$subject))
      possibleSubjects <- sort(unique(courseEnrollmentData$subject))

      possibleSemesters <- sort(unique(registrationDataBundle$outRegistrationTracking$fullRegistrationTracking$semester))
      possibleSemesters <- as.numeric(possibleSemesters)

      possibleSemestersHistoric <- sort(unique(courseEnrollmentData$semester))
      possibleSemestersHistoric <- as.numeric(possibleSemestersHistoric)

      output$referenceSemester <- renderUI({
        ns <- session$ns
        currentSemester <- max(possibleSemesters)
        initialChoice <- currentSemester - 100
        theOutput <- selectInput(ns("referenceSemester"),
                                 label="Reference Semester",
                                 choices=possibleSemestersHistoric,
                                 selected=as.character(initialChoice))
        theOutput
      })
      output$focalSemester <- renderUI({
        ns <- session$ns
        # selectInput(ns("focalSemester"),
        #             label="Focal Semester",
        #             choices=possibleSemesters,
        #             selected=max(possibleSemesters))
        selectInput(ns("focalSemester"),
                    label="Focal Semester",
                    choices=possibleSemesters,
                    selected="202221")

      })
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
      output$ugcourses <- renderUI({
        ns <- session$ns
        radioButtons(ns("selectCourseUG"),
                     label=h4("Courses"),
                     choices=create.ug.course.list(input$testInput3))
      })
      output$gradcourses <- renderUI({
        ns <- session$ns
        radioButtons(ns("selectCourseGrad"),
                     label=h4("Courses"),
                     choices=create.grad.course.list(input$testInput3))
      })


      output$courseList <- renderUI({
        ns <- session$ns
        req(input$choiceUG)
        if(input$choiceUG==1){
          t.out <- radioButtons(ns("selectCourse"),
                                label=h4("Courses"),
                                choices=create.ug.course.list(input$testInput3))
        } else {
          t.out <- radioButtons(ns("selectCourse"),
                                label=h4("Courses"),
                                choices=create.grad.course.list(input$testInput3))
        }
        t.out
      })

      output$enrollmentSidebar <- renderUI({
        randomIndex <- sample(1:length(possibleSubjects), 1)
        initialSubject <- possibleSubjects[randomIndex]
        tagList(
          div(id="sidebarContainer", class="sidebarContainerMain",
              tagList(
                selectInput(ns("testInput3"), "Subject", choices=possibleSubjects, selected=initialSubject),
                fluidRow(column(6, div(id="bannerComponent3", class="bannerComponent",
                                       uiOutput(ns("focalSemester")))),
                         column(6, div(id="bannerComponent4", class="bannerComponent",
                                       uiOutput(ns("referenceSemester"))))
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
                    div(id="bannerComponent5a", class="multiColumnBannerComponent",
                        uiOutput(ns("courseList")))
                  ))
                )
              )
          )
        )
      })

      observeEvent(input$focalSemester,{
        # check to see if the fullRegistrationTracking object is null
        # if not, check to see if input$focalSemester is not null
        cat("Entering observeEvent for focalSemester\n")
        req(input$testInput3)
        cat("Paste req(input$testInput3\n")
        if(!is.null(input$focalSemester)){
          if(is.null(fullRegistrationTracking$theData)){
            query <- paste0("SELECT * FROM t", input$focalSemester," WHERE subject = '", input$testInput3,"'")
            fullRegistrationTracking$theData <<- DBI::dbGetQuery(dbConn, sql(query))
            #browser()
          } else {
            # check to see if the semester of interest and dept of interest
            # are in the dataset.  if they already are, then no action necessary
            # else pull the data from the DB.
            availableSubjects <- unique(fullRegistrationTracking$theData$subject)
            availableSemesters <- unique(fullRegistrationTracking$theData$semester)
            if(!(input$focalSemester %in% availableSemesters) &
               !(input$testInput3 %in% availableSubjects)){
              query <- paste0("SELECT * FROM t", input$focalSemester," WHERE subject = '", input$testInput3,"'")
              midData <- DBI::dbGetQuery(dbConn, sql(query))
              fullRegistrationTracking$theData <<- rbind(fullRegistrationTracking$theData, midData)
              #browser()
            }
          }
        }
      })
      observeEvent(input$referenceSemester, {

        req(input$testInput3)
        #browser()
        if(!is.null(input$referenceSemester)){
          if(is.null(fullRegistrationTracking$theData)){
            query <- paste0("SELECT * FROM t", input$referenceSemester," WHERE subject = '", input$testInput3,"'")
            fullRegistrationTracking$theData <<- DBI::dbGetQuery(dbConn, sql(query))
          } else {
            # check to see if the semester of interest and dept of interest
            # are in the dataset.  if they already are, then no action necessary
            # else pull the data from the DB.
            availableSubjects <- unique(fullRegistrationTracking$theData$subject)
            availableSemesters <- unique(fullRegistrationTracking$theData$semester)
            if(!(input$referenceSemester %in% availableSemesters) &
               !(input$testInput3 %in% availableSubjects)){
              query <- paste0("SELECT * FROM t", input$referenceSemester," WHERE subject = '", input$testInput3,"'")
              midData <- DBI::dbGetQuery(dbConn, sql(query))
              fullRegistrationTracking$theData <<- rbind(fullRegistrationTracking$theData, midData)
            }
          }
        }
      })
      observeEvent(input$testInput3, {

        req(input$referenceSemester)
        req(input$focalSemester)
        browser()
        if(!is.null(input$testInput3)){
          if(is.null(fullRegistrationTracking$theData)){
            query1 <- paste0("SELECT * FROM t", input$referenceSemester," WHERE subject = '", input$testInput3,"'")
            query2 <- paste0("SELECT * FROM t", input$focalSemester," WHERE subject = '", input$testInput3,"'")

            midData1 <- DBI::dbGetQuery(dbConn, sql(query1))
            midData2 <- DBI::dbGetQuery(dbConn, sql(query2))
            fullRegistrationTracking$theData <- rbind(midData1, midData2)

          } else {
            # check to see if the semester of interest and dept of interest
            # are in the dataset.  if they already are, then no action necessary
            # else pull the data from the DB.
            tempData <- fullRegistrationTracking$theData %>%
              mutate(combo=paste(subject, semester))

            availableCombos <- unique(tempData$combo)

            if(!(paste(input$testInput3, input$referenceSemester) %in% availableCombos)){
              query <- paste0("SELECT * FROM t", input$referenceSemester," WHERE subject = '", input$testInput3,"'")
              midData <- DBI::dbGetQuery(dbConn, sql(query))
              fullRegistrationTracking$theData <<- rbind(fullRegistrationTracking$theData, midData)
            }
            if(!(paste(input$testInput3, input$focalSemester) %in% availableCombos)){
              query <- paste0("SELECT * FROM t", input$focalSemester," WHERE subject = '", input$testInput3,"'")
              midData <- DBI::dbGetQuery(dbConn, sql(query))
              fullRegistrationTracking$theData <<- rbind(fullRegistrationTracking$theData, midData)
            }
          }
        }
      })

      create.ug.course.list.orig <- function(dept){
        t.data <- fullRegistrationTracking$theData %>%
          mutate(courseNumber=as.character(courseNumber)) %>%
          mutate(courseNumber=as.numeric(courseNumber)) %>%
          filter(subject==dept) %>%
          filter(courseNumber < 500) %>%
          select("courseNumber") %>%
          unlist() %>%
          as.vector() %>%
          unique() %>%
          sort()

        t.data
      }

      create.ug.course.list <- function(dept){
        t.data <- courseEnrollmentData %>%
          mutate(courseNumber=as.character(course.number)) %>%
          mutate(courseNumber=as.numeric(courseNumber)) %>%
          filter(subject==dept) %>%
          filter(courseNumber < 500) %>%
          select("courseNumber") %>%
          unlist() %>%
          as.vector() %>%
          unique() %>%
          sort()

        t.data
      }
      create.grad.course.list <- function(dept){

        t.data <- fullRegistrationTracking %>%
          mutate(courseNumber=as.character(courseNumber)) %>%
          mutate(courseNumber=as.numeric(courseNumber)) %>%
          filter(subject==dept) %>%
          filter(courseNumber >= 500) %>%
          select("courseNumber") %>%
          unlist() %>%
          as.vector() %>%
          unique() %>%
          sort()

        t.data
      }

      theDeptAbbrv <- reactive({input$sidebarSelectDepartment})

      if(!is.null(reactive({fullRegistrationTracking$theData}))){
        someData <- reactive({input$fullRegistrationTracking$theData})
        trackingProxy <- registrationTrackingOrigServer("registrationTrackingGEOG",
                                                        deptAbbrv = reactive({input$testInput3}),
                                                        focalSem = reactive({input$focalSemester}),
                                                        refSem = reactive({input$referenceSemester}),
                                                        ugControl=reactive({input$choiceUG}),
                                                        syncSwitch=reactive({input$synchSwitch}), # probably not needed
                                                        sameTerm=reactive({input$sameTermSwitch}),
                                                        chosenCourse=reactive({input$selectCourse}),
                                                        previousSemestersFinalEnrollment=courseEnrollmentData,
                                                        fullRegistrationTracking=reactive({fullRegistrationTracking$theData}),
                                                        useShort=TRUE,
                                                        semester.codes=semester.codes,
                                                        classesStart=classesStart,
                                                        synonyms=departmentSynonyms)
        #browser()
        historicEnrollmentServer("historicEnrollment", inData=courseEnrollmentData, trackingProxy,
                                 deptAbbrv = reactive({input$testInput3}),
                                 ugControl=reactive({input$choiceUG}),
                                 chosenCourse=reactive({input$selectCourse}),
                                 semester.codes=semester.codes,
                                 synonyms=departmentSynonyms)

        # registrationTrackingSummaryServer("t1", trackingProxy, ppData=someData,
        #                                   semester.codes=semester.codes,
        #                                   focalSem = reactive({input$focalSemester}),
        #                                   refSem = reactive({input$referenceSemester}),
        #                                   deptAbbrv = reactive({input$testInput3}),
        #                                   classesStart=classesStart,
        #                                   synonyms=departmentSynonyms)
      }

    }
  )
}

