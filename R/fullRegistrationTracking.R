#' fullRegistrationTrackingBoxModuleUI
#'
#' @param id the ID
#'
#' @return
#' @export
#'
#' @examples
fullRegistrationTrackingBoxModuleUI <- function(id){
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

#' fullRegistrationTrackingBoxModuleServer
#'
#' @param id the ID
#' @param input the input
#' @param output the output
#' @param session the session
#' @param registrationDataBundle  the input data
#' @param classesStart dates that classes start by semester
#'
#' @return
#' @export
#'
#' @examples
fullRegistrationTrackingBoxModuleServer <- function(id, input, output, session, registrationDataBundle, classesStart){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      ###############################################
      # Add change of department code for PSYC      #
      # to PBSI                                     #
      ###############################################


      departmentSynonyms <- registrationDataBundle$outCourseEnrollment$courseEnrollmentData %>%
        mutate(synonymCode=subject) %>%
        select("subject", "synonymCode") %>%
        unique() %>%
        mutate(synonymCode=case_when(subject=="PBSI" ~ "PSYC",
                                     subject=="PSYC" ~ "PBSI",
                                     TRUE ~ subject))


      semester.codes <- registrationDataBundle$outSemesterCodes$semester.codes
      possibleSubjects <- sort(unique(registrationDataBundle$outRegistrationTracking$fullRegistrationTracking$subject))
      possibleSemesters <- sort(unique(registrationDataBundle$outRegistrationTracking$fullRegistrationTracking$semester))
      possibleSemesters <- as.numeric(possibleSemesters)

      possibleSemestersHistoric <- sort(unique(registrationDataBundle$outCourseEnrollment$courseEnrollmentData$semester))
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
          selectInput(ns("testInput3"), "Subject", choices=possibleSubjects, selected="GEOG"),
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
              div(id="bannerComponent5", class="multiColumnBannerComponent",
                  uiOutput(ns("courseList")))
            ))
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
                                                      previousSemestersFinalEnrollment=registrationDataBundle$outCourseEnrollment$courseEnrollmentData,
                                                      fullRegistrationTracking=registrationDataBundle$outRegistrationTracking$fullRegistrationTracking,
                                                      useShort=TRUE,
                                                      semester.codes=semester.codes,
                                                      classesStart=classesStart,
                                                      synonyms=departmentSynonyms)

      historicEnrollmentServer("historicEnrollment", inData=registrationDataBundle$outCourseEnrollment$courseEnrollmentData, trackingProxy,
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

