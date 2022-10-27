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
          #div(class="thingHolder",
          fluidRow(
            registrationTrackingOrigUI(ns("registrationTrackingGEOG"))
          ),
          fluidRow(
            historicEnrollmentUI(ns("historicEnrollment"))
          ))),
        column(4, tagList(
          #div(class="thingHolder",
          fluidRow(
            tagList(
              registrationTrackingSummaryUI(ns("t1"))
            )
          )
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

      con <- dbConnect(RPostgres::Postgres(),
                       dbname = 'mcairns/EnrollmentTracking', # database name
                       host = 'db.bit.io',
                       port = 5432,
                       user = 'mcairns',
                       password = "v2_3v5SD_cRWEpZn8CMgLfU4hNCbaXaH")

      #query <- "SELECT * FROM t202221 WHERE subject = 'GEOG'"
      query <- "SELECT * FROM semesterCodes"
      semesterCodes <- DBI::dbGetQuery(con, sql(query))

      query <- "SELECT * FROM courseEnrollmentData"
      courseEnrollmentData <- DBI::dbGetQuery(con, sql(query))

      dbDisconnect(con)
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
        selectInput(ns("focalSemester"),
                    label="Focal Semester",
                    choices=possibleSemesters,
                    selected=max(possibleSemesters))

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

      create.ug.course.list <- function(dept){
        t.data <- registrationDataBundle$outRegistrationTracking$fullRegistrationTracking %>%
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

      create.grad.course.list <- function(dept){

        t.data <- registrationDataBundle$outRegistrationTracking$fullRegistrationTracking %>%
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

      trackingProxy <- registrationTrackingOrigServer("registrationTrackingGEOG",
                                                      deptAbbrv = reactive({input$testInput3}),
                                                      focalSem = reactive({input$focalSemester}),
                                                      refSem = reactive({input$referenceSemester}),
                                                      ugControl=reactive({input$choiceUG}),
                                                      syncSwitch=reactive({input$synchSwitch}), # probably not needed
                                                      sameTerm=reactive({input$sameTermSwitch}),
                                                      chosenCourse=reactive({input$selectCourse}),
                                                      previousSemestersFinalEnrollment=courseEnrollmentData,
                                                      fullRegistrationTracking=registrationDataBundle$outRegistrationTracking$fullRegistrationTracking,
                                                      useShort=TRUE,
                                                      semester.codes=semester.codes,
                                                      classesStart=classesStart,
                                                      synonyms=departmentSynonyms)

      historicEnrollmentServer("historicEnrollment", inData=courseEnrollmentData, trackingProxy,
                               deptAbbrv = reactive({input$testInput3}),
                               ugControl=reactive({input$choiceUG}),
                               chosenCourse=reactive({input$selectCourse}),
                               semester.codes=semester.codes,
                               synonyms=departmentSynonyms)

      registrationTrackingSummaryServer("t1", trackingProxy, ppData=registrationDataBundle$outRegistrationTracking$fullRegistrationTracking,
                                        semester.codes=semester.codes,
                                        focalSem = reactive({input$focalSemester}),
                                        refSem = reactive({input$referenceSemester}),
                                        deptAbbrv = reactive({input$testInput3}),
                                        classesStart=classesStart,
                                        synonyms=departmentSynonyms)

    }
  )
}

