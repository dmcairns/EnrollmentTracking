
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyjs)
library(readxl)
library(dplyr)
library(tidyr)
library(crayon)
library(DT)
library(lubridate)
library(stringr)
library(c3)
library(ggplot2)
library(htmltools)
library(shinyBS)
library(shinyWidgets)
library(tibble)
library(naniar)
#library(EnrollmentTracking)


# Source code files
# source("themes.R")
#source("readInputModule.R")
# source("readSchedulingModule.R")
#
# source("SchedulingModuleNew.R")
# source("registrationTrackingModule.R")
# source("courseEnrollmentModule.R")
# source("productivityOverviewModule.R")
# source("DepartmentMetricsModules.R")
# source("facultyCommitteesModule.R")
# source("personnelModule.R")

# deployMe <- FALSE
# if (deployMe==TRUE){
#   whatToRead <- c(list(scheduling               =  TRUE,
#                        SCH                      =  FALSE,
#                        faculty.list             =  TRUE,
#                        semesterCodes            =  TRUE,
#                        annualProductivity       =  FALSE,
#                        researchExpenditures     =  FALSE,
#                        idcData                  =  FALSE,
#                        courseDemographicsData   =  TRUE,
#                        courseEnrollmentData     =  TRUE,
#                        registrationTracking     =  TRUE,
#                        graduationData           =  FALSE,
#                        graduateSupervisionData  =  FALSE,
#                        schFull                  =  TRUE,
#                        teachingEvaluations      =  FALSE,
#                        personnel                =  FALSE
#   ))
# } else {
#   whatToRead <- c(list(scheduling               =  TRUE,
#                        SCH                      =  FALSE,
#                        faculty.list             =  TRUE,
#                        semesterCodes            =  TRUE,
#                        annualProductivity       =  TRUE,
#                        researchExpenditures     =  TRUE,
#                        idcData                  =  TRUE,
#                        courseDemographicsData   =  TRUE,
#                        courseEnrollmentData     =  TRUE,
#                        registrationTracking     =  TRUE,
#                        graduationData           =  TRUE,
#                        graduateSupervisionData  =  TRUE,
#                        schFull                  =  TRUE,
#                        teachingEvaluations      =  TRUE,
#                        personnel                =  TRUE
#   ))
# }

# local.path <- "//Volumes//GoogleDrive//My Drive//"
# local.archive.path <- paste0(local.path, "ShinyApps//LocalDataArchive//")
# expenditures.path <- paste0(local.archive.path, "GEOGResearchExpenditures//")
# rds.path <- ".//Data//"
# semester.codes <- readRDS(".//Data//new.semester.codes.rds")
# assign("semester.codes", semester.codes, pos=1)

ui <- dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title=tags$img(src="GeographyBannerSM.png", alt="Geography Banner"),
    leftUi = tagList(
      useShinyjs(),
      #remove CSS below for development
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "DashboardModularized.css?version=17")
      ),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "scheduling.css?version=6")
      )
    )
  ),
  sidebar = dashboardSidebar(


    minified = FALSE,
    collapsed = FALSE,   #Start with the sidebar collapsed
    id = "dashboardSideboardPageControls"
  ),
  body = dashboardBody(
    tamuDashboardThemeGEOG,
    shinydashboardPlus::box(
      id = "readInputBox",
      title="Read Input",
      closable = TRUE,
      width = 12,
      height = "500px",
      solidHeader = FALSE,
      collapsible = FALSE,
      uiOutput("theReadUI"),
      sidebar = boxSidebar(
        id = "readInputSidebar",               #make sure that this is unique
        width = 25,                            #width can be no smaller than 25%
        p("readInput Sidebar Content")
      )
    ),
    fullRegistrationTrackingBoxModuleUI("regTrackingStuff"),


    sidebar = boxSidebar(
      id = "theSidebar",                #make sure that this is unique
      width = 25,                              #width can be no smaller than 25%
      p("Sidebar Content")
    )
  )
)

server <- function(input, output, session) {
  boxStatus <- reactiveValues(mybox=FALSE, programMetricsBox=FALSE,
                              researchMetricsBox=FALSE, gradStudentInfoBox=FALSE,
                              personnelBox=FALSE, gradProgramReportsBox=FALSE,
                              schedulingBox=FALSE, enrollmentBox=TRUE,
                              teachingBox=FALSE, evaluationBox=FALSE,
                              studentCommitteesBox=FALSE, readInputBox=FALSE,
                              overviewBox=FALSE, fullSCHBox=FALSE,
                              fullPersonnelBox=FALSE)
  # observeEvent(input$schedulingBox$closable, {
  #   # This is used to remove a box after it has been created at startup.
  #   #cat(yellow("boxStatus$schedulingBox:", boxStatus$schedulingBox,"\n"))
  #   if(!boxStatus$schedulingBox) updateBox("schedulingBox", "remove")
  #   boxStatus$schedulingBox <- TRUE
  # })
  # observeEvent(input$fullSCHBox$closable, {
  #   if(!boxStatus$fullSCHBox) updateBox("fullSCHBox", "remove")
  #   boxStatus$fullSCHBox <- TRUE
  # })
  # observeEvent(input$readInputBox$closable, {
  #   # This is used to remove a box after it has been created at startup.
  #   if(!boxStatus$readInputBox) updateBox("readInputBox", "remove")
  #   boxStatus$readInputBox <- TRUE
  # })
  # observeEvent(input$overviewBox$closable, {
  #   if(!boxStatus$overviewBox) updateBox("overviewBox", "remove")
  #   boxStatus$overviewBox <- TRUE
  # })
  # observeEvent(input$fullPersonnelBox$closable, {
  #   if(!boxStatus$fullPersonnelBox) updateBox("fullPersonnelBox", "remove")
  #   boxStatus$fullPersonnelBox <- TRUE
  # })
  # observeEvent(input$toggleReadInput, {
  #   # Toggle only collapses/uncollapses
  #   # collapsible must be TRUE for toggle to work.
  #
  #   if(input$schedulingBox$visible) updateBox("schedulingBox", "remove")
  #   if(input$enrollmentBox$visible) updateBox("enrollmentBox", "remove")
  #   if(input$overviewBox$visible) updateBox("overviewBox", "remove")
  #   if(input$fullSCHBox$visible) updateBox("fullSCHBox", "remove")
  #   if(!input$fullPersonnelBox$visible) updateBox("fullPersonnelBox", "remove")
  #   if(!input$readInputBox$visible){
  #     updateBox("readInputBox", action="restore")
  #   }
  # })
  # observeEvent(input$toggleScheduling, {
  #
  #   if(input$readInputBox$visible) updateBox("readInputBox", "remove")
  #   if(input$enrollmentBox$visible) updateBox("enrollmentBox", "remove")
  #   if(input$overviewBox$visible) updateBox("overviewBox", "remove")
  #   if(input$fullSCHBox$visible) updateBox("fullSCHBox", "remove")
  #   if(!input$fullPersonnelBox$visible) updateBox("fullPersonnelBox", "remove")
  #   if(!input$schedulingBox$visible){
  #     updateBox("schedulingBox", "restore")
  #   }
  # })
  # observeEvent(input$toggleEnrollment, {
  #
  #   if(input$readInputBox$visible) updateBox("readInputBox", "remove")
  #   if(input$schedulingBox$visible) updateBox("schedulingBox", "remove")
  #   if(input$overviewBox$visible) updateBox("overviewBox", "remove")
  #   if(input$fullSCHBox$visible) updateBox("fullSCHBox", "remove")
  #   if(!input$fullPersonnelBox$visible) updateBox("fullPersonnelBox", "remove")
  #   if(!input$enrollmentBox$visible){
  #     updateBox("enrollmentBox", "restore")
  #   }
  # })
  # observeEvent(input$toggleOverview, {
  #   if(input$readInputBox$visible) updateBox("readInputBox", "remove")
  #   if(input$schedulingBox$visible) updateBox("scedulingBox", "remove")
  #   if(input$enrollmentBox$visible) updateBox("enrollmentBox", "remove")
  #   if(input$fullSCHBox$visible) updateBox("fullSCHBox", "remove")
  #   if(!input$fullPersonnelBox$visible) updateBox("fullPersonnelBox", "remove")
  #   if(!input$overviewBox$visible) {
  #     updateBox("overviewBox", "restore")
  #   }
  # })
  # observeEvent(input$toggleSCH, {
  #   if(input$readInputBox$visible) updateBox("readInputBox", "remove")
  #   if(input$schedulingBox$visible) updateBox("scedulingBox", "remove")
  #   if(input$enrollmentBox$visible) updateBox("enrollmentBox", "remove")
  #   if(input$overviewBox$visible) updateBox("overviewBox", "remove")
  #   if(!input$fullPersonnelBox$visible) updateBox("fullPersonnelBox", "remove")
  #   if(!input$fullSCHBox$visible) {
  #     updateBox("fullSCHBox", "restore")
  #   }
  # })
  # observeEvent(input$togglePersonnel, {
  #   if(input$readInputBox$visible) updateBox("readInputBox", "remove")
  #   if(input$schedulingBox$visible) updateBox("scedulingBox", "remove")
  #   if(input$enrollmentBox$visible) updateBox("enrollmentBox", "remove")
  #   if(input$overviewBox$visible) updateBox("overviewBox", "remove")
  #   if(input$overviewBox$visible) updateBox("fullSCHBox", "remove")
  #   if(!input$fullPersonnelBox$visible) {
  #     updateBox("fullPersonnelBox", "restore")
  #   }
  #
  #
  # })
  # output$theReadUI <- renderUI({
  #   tagList(
  #     fluidRow(
  #       column(8,readInputDataUI("dataInput")),
  #       column(4, readProgressModuleUI("testProgress", choices=whatToRead))
  #     )
  #   )
  # })

  # giantDataBall <- readProgressModuleServer("testProgress", choices=whatToRead, rds.path=rds.path,
  #                                           local.archive.path=local.archive.path, local.path=local.path,
  #                                           useShort=TRUE)
  # assign("giantDataBall", giantDataBall, pos=1)
  # t.faculty <- giantDataBall$outAnnualProductivity$annualProductivity$`2021`$faculty
  # faculty.list <- t.faculty   # check that this is correct for researchPie2021Server

  fullRegistrationTrackingBoxModuleServer("regTrackingStuff", registrationDataBundle=giantDataBall)

  #Scheduling::fullSchedulingBoxModuleServer("schedulingStuff", schedulingDataBundle=giantDataBall$outScheduling)
  # fullSchedulingBoxModuleServer("schedulingStuff", schedulingDataBundle=giantDataBall$outScheduling)
  #
  # fullSCHBoxModuleServer("fullSCH", schedulingDataBundle=giantDataBall$outScheduling, schDataBundle=giantDataBall)
  #fullPersonnelModuleServer("personnelStuff", personnelDataBundle=giantDataBall)
  #fullOverviewBoxModuleServer("overviewStuff", overviewDataBundle=giantDataBall)



}


# Run the application
shinyApp(ui = ui, server = server)
