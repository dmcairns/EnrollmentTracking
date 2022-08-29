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
            registrationTrackingSummaryUI(ns("t1"))
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
#'
#' @return
#' @export
#'
#' @examples
fullRegistrationTrackingBoxModuleServer <- function(id, input, output, session, registrationDataBundle){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      semester.codes <- registrationDataBundle$outSemesterCodes$semester.codes

      output$enrollmentSidebar <- renderUI({
        p("Some text for enrollmentSidebar")
      })

      trackingProxy <- registrationTrackingOrigServer("registrationTrackingGEOG",
                                                      deptAbbrv = "GEOG",
                                                      previousSemestersFinalEnrollment=registrationDataBundle$outCourseEnrollment$courseEnrollmentData,
                                                      fullRegistrationTracking=registrationDataBundle$outRegistrationTracking$fullRegistrationTracking,
                                                      useShort=TRUE,
                                                      semester.codes=semester.codes)

      historicEnrollmentServer("historicEnrollment", inData=registrationDataBundle$outCourseEnrollment$courseEnrollmentData, trackingProxy,
                               semester.codes=semester.codes)

      registrationTrackingSummaryServer("t1", trackingProxy, ppData=registrationDataBundle$outRegistrationTracking$ppFullRegistrationTracking,
                                        semester.codes=semester.codes)

    }
  )
}

