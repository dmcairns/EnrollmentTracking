#' historicEnrollmentUI
#'
#' @param id the id
#' @param label the label
#'
#' @return
#' @export
#'
#' @examples
historicEnrollmentUI <- function(id, label = "Historic Enrollment") {
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=FALSE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("courseEnrollmentControls")),
                     width=40,
                     background="#998542",
                     id=ns("sidebarControls"),
                     class="testSidebar"
                   ),
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("historicEnrollmentPanel")))
                                  )
                   ),
                   id=ns("courseSchBox"),
                   class="schBoxTestOct"
                 ))

}

#' historicEnrollmentServer
#'
#' @param id the id
#' @param inData the input data
#' @param inTrackingData the tracking data
#' @param semester.codes the semester.codes
#' @param deptAbbrv the department abbreviation
#' @param ugControl the undergraduate control
#' @param chosenCourse the chosen course
#' @param synonyms table of synonyms for department codes (e.g. PSYC = PBSI)
#'
#' @return
#' @export
#'
#' @examples
historicEnrollmentServer <- function(id, inData, inTrackingData,
                                     deptAbbrv,
                                     ugControl,
                                     chosenCourse,
                                     semester.codes,
                                     synonyms) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      courseEnrollmentData <- inData
      #########################
      # Input Control UIs     #
      #########################

      output$yearTypeControl <- renderUI({
        ns <- session$ns

        selectInput(ns("yearType"), "Year Type:",
                    list("Calendar", "Fiscal"),
                    selected = "Calendar"
        )

      })
      output$startYearControl  <- renderUI({
        ns <- session$ns
        theYears <- inData %>%
          select("year") %>%
          unique() %>%
          unlist() %>%
          as.vector()

        min.year <- min(theYears)
        max.year <- max(theYears)

        semester.list <- c(min.year:max.year)

        t.out <- selectInput(ns("startYear"), "Start Year:",
                             semester.list, selected = min.year)

        t.out

      })
      output$endYearControl <- renderUI({
        ns <- session$ns

        req(input$startYear)
        theYears <- inData %>%
          filter(year >= input$startYear) %>%
          select("year") %>%
          unique() %>%
          unlist() %>%
          as.vector()

        min.year <- min(theYears)
        max.year <- max(theYears)

        semester.list <- c(min.year:max.year)

        t.out <- selectInput(ns("endYear"), "End Year:",
                             semester.list, selected = max(semester.list))

        t.out
      })
      output$degreeEarnedControl <- renderUI({
        ns <- session$ns
        selectInput(ns("degreeEarned"), "Degree:",
                    list("MS", "MGS", "PHD"),
                    selected = "MS"
        )

      })
      output$demoSemesterControl <- renderUI({
        ns <- session$ns
        theSemesters <- inData %>%
          select("semester") %>%
          unique() %>%
          unlist() %>%
          as.vector() %>%
          sort()
        t.out <- selectInput(ns("demoSemester"), "Demographics Semester:",
                             theSemesters, selected=max(theSemesters))
        t.out
      })
      output$courseEnrollmentControls <- renderUI({
        ns <- session$ns
        theOutput <- tagList(
          fluidRow(
            column(4, uiOutput(ns("startYearControl"))),
            column(4, uiOutput(ns("endYearControl"))),
            column(4, uiOutput(ns("demoSemesterControl")))
          ),


          selectInput(ns("choiceSemesters"), label="Semesters",
                      choices=c("Spring", "Summer", "Fall"),
                      selected=c("Spring", "Fall"),
                      multiple=TRUE)
        )

        theOutput
      })


      output$ugcoursesSidebar <- renderUI({
        ns <- session$ns

        req(deptAbbrv())

        dept.code <- deptAbbrv()


        radioButtons(ns("selectCourseUG"),
                     label="Courses",
                     choices=create.ug.course.list(courseEnrollmentData, deptAbbrv()))
      })
      output$ugcourses <- renderUI({
        ns <- session$ns
        # req(input$selectDept)
        # dept.code <- input$selectDept
        req(deptAbbrv())
        print(dept.code)
        #print(length(create.ug.course.list(dept.code)))
        radioButtons(ns("selectCourseUG"),
                     label=h4("Courses"),
                     choices=create.ug.course.list(courseEnrollmentData, deptAbbrv()))
      })
      output$gradcourses <- renderUI({
        ns <- session$ns
        # dept.code <- input$selectDept
        # print(dept.code)

        # print(length(create.grad.course.list(dept.code)))
        radioButtons(ns("selectCourseGrad"),
                     label=h4("Courses"),
                     choices=create.grad.course.list(courseEnrollmentData, deptAbbrv()))
      })
      output$isData <- reactive({
        dim(generate.small.df())[[1]]
      })
      output$noSections <- reactive({
        dim(generate.section.df())[[1]]
      })
      output$NoDataText <- ({
        renderText("This course was not taught in this semester.")
      })
      output$txt <- ({renderText("This course was not taught in this semester.")})



      #########################
      # Card Elements         #
      #########################
      output$plotTitle <- renderText({
        ns <- session$ns

        req(inTrackingData$Department)
        req(inTrackingData$synchronize)
        if(inTrackingData$synchronize==TRUE){
          theTitle <- paste0("Enrollment History for ", inTrackingData$Department, " ", inTrackingData$courseNum)
        } else {
          theTitle <- paste0("Enrollment History for ", deptAbbrv(), " ", chosenCourse())
        }

        theTitle
      })
      output$bannerTest <- renderUI({
        ns <- session$ns
        tagList(
          div(id="bannerComponent1",
              uiOutput(ns("courseEnrollmentControls")))#,
        )
      })
      output$historicEnrollmentPanel <- renderUI({
        ns <- session$ns
        tabsetPanel(type="tabs",
                    tabPanel("Enrollments",
                             conditionalPanel(condition="2 > 0", c3Output(ns("enrollmentPlotC3")))
                    ),
                    tabPanel("Major Demographics",
                             tagList(
                               uiOutput(ns("demographicsPanel"))
                             )

                    )
        )
      })


      output$conditionalEnrollmentPanel <- renderUI({
        ns <- session$ns
        x <- 0
        if(x==0){
          c3Output(ns("enrollmentPlotC3"))
          #plotOutput(ns("enrollmentPlot"))
        } else {
          verbatimTextOutput(ns("txt"))
        }
      })
      output$conditionalDemographicsPanel <- renderUI({
        ns <- session$ns
        noSections <- dim(generate.section.df())[[1]]
        cat("noSections:", noSections, "\n")
        if(noSections==0){
          verbatimTextOutput(ns("NoDataText"), placeholder=T)
        } else {
          if(input$graphicsSwitch){
            DT::dataTableOutput(ns("courseTable"))
          } else {
            plotOutput(ns("demographicsPlot"))
          }
        }
      })
      output$demographicsPanel <- renderUI({
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
      makeMajorsDT <- reactive({
        demoData <- demoDataPreparation()

        demoData

      })
      makeMajorsPie <- reactive({
        demoData <- demoDataPreparation() %>%
          ungroup() %>%
          slice_max(n, n=5) %>%
          pivot_wider(names_from=major, values_from=n)

        demoData
      })
      demoDataPreparation <- reactive({

        req(input$demoSemester)
        req(deptAbbrv())
        req(chosenCourse())

        # Needs to reference a local semester control to replace focalSem()

        theDemographicData <- inData %>%
          mutate(course.designation=paste(subject, course.number)) %>%
          filter(course.designation==paste(deptAbbrv(), chosenCourse())) %>%
          filter(semester==input$demoSemester) %>%
          group_by(major) %>%
          summarize(n=sum(enrolledStudents), .groups="drop")

        theDemographicData

      })
      output$enrollmentPlot <- renderPlot({
        makeEnrollmentPlot()
      })
      output$enrollmentPlotC3 <- renderC3({
        req(input$choiceSemesters)
        req(input$startYear)
        req(input$endYear)
        req(inTrackingData$Department)
        req(inTrackingData$courseNum)

        possibleSubjects <- synonyms %>%
          filter(subject %in% inTrackingData$Department) %>%
          unlist() %>%
          as.vector() %>%
          unique()

        if(inTrackingData$synchronize==TRUE){
          useEnrollmentData <- inData %>%
            filter(subject %in% possibleSubjects) %>%
            filter(course.number==inTrackingData$courseNum) %>%
            filter(Semester.1 %in% input$choiceSemesters) %>%
            filter(year >= input$startYear) %>%
            filter(year <= input$endYear) %>%
            group_by(year, semester) %>%
            summarize(numStudents=sum(enrolledStudents), .groups="drop") %>%
            select("semester", "Students Enrolled" = "numStudents")
        } else {
          useEnrollmentData <- inData %>%
            filter(subject %in% possibleSubjects) %>%
            filter(course.number==input$selectCourse) %>%
            filter(Semester.1 %in% input$choiceSemesters) %>%
            filter(year >= input$startYear) %>%
            filter(year <= input$endYear) %>%
            group_by(year, semester) %>%
            summarize(numStudents=sum(enrolledStudents), .groups="drop") %>%
            select("semester", "Students Enrolled" = "numStudents")
        }


        #browser()
        #Add zeroes for semesters when course is not offered.

        outputChart <- useEnrollmentData %>%
          c3(x="semester", colors=list('Students Enrolled'="#003C71")) %>%
          c3_bar(stacked=TRUE) %>%
          yAxis(label= list(text="Enrolled Students",
                            position="outer-middle")) %>%
          grid('y') %>%
          tickAxis('y', format=JS('function (value, ratio, id, index) {
                       var format = id === `data` ? d3.format(`$`) : d3.format(`~s`) ;
                       return format(value);}')) %>%
          legend(hide=TRUE) #%>%
        # tooltip(format = list(
        #   value = JS('function (value, ratio, id, index) {
        #                    var format = id === `data` ? d3.format(`$`) : d3.format(`$,.3~s`) ;
        #                    return format(value);}')
        # ))


        outputChart

      })
      output$demographicsPlot <- renderPlot({
        cat("***************** In demographicsPlot ***************\n")
        make.demographics.plot()
      })

      output$courseTable <- DT::renderDataTable({
        t.1 <- generate.demographics.df()
        t.data.summary <- t.1 %>%
          group_by(full.semester, major) %>%
          dplyr::summarise(n())
        names(t.data.summary) <- c("Semester", "Major", "Student")
        t.data.summary
      })

      #########################
      # Observer Functions    #
      #########################
      # observeEvent(input$modalActivationButton, {
      #   #toggleCssClass does not work if plotConrolsDiv is wrapped
      #   #in an ns()
      #   toggleCssClass(id="controlsBanner", class="visible")
      # })


      #########################
      # Data processing and   #
      # preparation functions #
      #########################

      createCourseList <- function(inData, dept, isUG){
        theList <- inData %>%
          filter(suject==dept)
        if(isUG){
          theList <- theList %>%
            filter(course.number < 500)
        } else {
          theList <- theList %>%
            filter(course.number >= 500)
        }
        theList <- theList %>%
          select("course.number") %>%
          unique() %>%
          unlist() %>%
          as.vector()

        theOutput <- renderUI({
          radioButtons(ns("selectCourse"),
                       label="Courses",
                       choices=theList)
        })
        theOutput
      }
      makeDateList <- function(){
        t.list <- as.list(c(3000, (unique(inData$Year))))
        names(t.list) <- c("All", as.character(unique(inData$Year)))
        t.list
      }
      create.ug.course.list <- function(inData, dept){
        clge.enrollment.data <- inData
        # print("In create.ug.course.list")
        if (as.numeric(dept) == 1) dept.name <- "ATMO"
        if (as.numeric(dept) == 2) dept.name <- "GEOG"
        if (as.numeric(dept) == 3) dept.name <- "GEOL"
        if (as.numeric(dept) == 4) dept.name <- "GEOS"
        if (as.numeric(dept) == 5) dept.name <- "OCNG"
        if (as.numeric(dept) == 6) dept.name <- "GEOP"
        t.data <- clge.enrollment.data
        department <- substr(t.data$Course, 1,4)
        t.data <- data.frame(t.data, Department=department)
        t.data <- filter(t.data, Department==dept.name)
        unique.courses <- sort(unique(as.character(t.data$Course)))
        course.num <- substr(unique.courses, 5,7)
        t.ug <- (as.numeric(course.num)<500)
        unique.courses <- unique.courses[t.ug]
        t.list <- as.list(unique.courses)
        names(t.list) <- course.num[t.ug]
        t.list
      }

      create.grad.course.list <- function(inData, dept){
        clge.enrollment.data <- inData
        # print("In create.grad.course.list")
        if (as.numeric(dept) == 1) dept.name <- "ATMO"
        if (as.numeric(dept) == 2) dept.name <- "GEOG"
        if (as.numeric(dept) == 3) dept.name <- "GEOL"
        if (as.numeric(dept) == 4) dept.name <- "GEOS"
        if (as.numeric(dept) == 5) dept.name <- "OCNG"
        if (as.numeric(dept) == 6) dept.name <- "GEOP"
        t.data <- clge.enrollment.data
        department <- substr(t.data$Course, 1,4)
        t.data <- data.frame(t.data, Department=department)
        t.data <- filter(t.data, Department==dept.name)
        unique.courses <- sort(unique(as.character(t.data$Course)))
        course.num <- substr(unique.courses, 5,7)
        t.ug <- (as.numeric(course.num)>=500)
        unique.courses <- unique.courses[t.ug]
        t.list <- as.list(unique.courses)
        names(t.list) <- course.num[t.ug]
        t.list
      }
      generate.small.df <- reactive({
        req(input$selectCourseUG)
        req(input$choiceUG)

        t.data <- c(NULL)
        if(input$choiceUG == "1") {
          useSelectCourse <- input$selectCourseUG
        } else {
          useSelectCourse <- input$selectCourseGrad
        }

        if(!is.null(useSelectCourse)){


          t.data <- courseEnrollmentData
          #    t.data <- filter(t.data, )
          cat(green("before input$choiceSemesters != all\n"))
          cat(green("choiceSemesters:", input$choiceSemesters, "\n"))
          #if(input$choiceSemesters != "all") t.data <- filter(t.data, Semester.1==input$choiceSemesters)
          if(input$selectYear != "3000") t.data <- filter(t.data, Year==input$selectYear)
          # if(input$choiceUG == "ug")
          #     useSelectCourse <- input$selectCourseUG
          #if(input$choiceGRAD == "grad")
          #    useSelectCourse <- input$selectCourseGrad
          #print("here")
          # print(useSelectCourse)
          t.data <- t.data %>%
            filter(Course==useSelectCourse) %>%
            group_by(Year, Semester.1) %>%
            dplyr::summarise(total_Enrollment = sum(Enrollment),
                             min_Enrollment = min(Enrollment),
                             max_Enrollment = max(Enrollment),
                             n())
          full.semester <- paste(t.data$Semester.1, t.data$Year)

          # full.semester <- factor(full.semester, levels=c("Spring 2014", "Summer 2014", "Fall 2014", "Spring 2015", "Summer 2015", "Fall 2015", "Spring 2016", "Summer 2016", "Fall 2016", "Spring 2017", "Summer 2017",
          #                                                 "Fall 2017", "Spring 2018", "Summer 2018", "Fall 2018", "Spring 2019"))
          full.semester <- factor(full.semester, levels=unique(full.semester))
          t.data <- data.frame(t.data, full.semester=full.semester)
        }
        t.data
      })
      historicEnrollmentBarChartC3 <- function(inEnrollmentData, inSubject, inCourse,
                                               inSemesters, startYear, endYear) {
        #cat(red("In historicEnrollmentBarChartC3.......................................................\n"))
        #print(names(inEnrollmentData))
        useEnrollmentData <- inEnrollmentData %>%
          filter(subject==inSubject) %>%
          filter(course.number==inCourse) %>%
          filter(Semester.1 %in% inSemesters) %>%
          filter(year >= startYear) %>%
          filter(year <= endYear) %>%
          group_by(year, semester) %>%
          summarize(numStudents=n(), .groups="drop") %>%
          select("semester", "Students Enrolled" = "numStudents")


        outputChart <- useEnrollmentData %>%
          c3(x="semester", colors=list('Students Enrolled'="#003C71")) %>%
          c3_bar(stacked=TRUE) %>%
          yAxis(label= list(text="Enrolled Students",
                            position="outer-middle")) %>%
          grid('y') %>%
          tickAxis('y', format=JS('function (value, ratio, id, index) {
                       var format = id === `data` ? d3.format(`$`) : d3.format(`~s`) ;
                       return format(value);}')) %>%
          legend(hide=TRUE) #%>%
        # tooltip(format = list(
        #   value = JS('function (value, ratio, id, index) {
        #                    var format = id === `data` ? d3.format(`$`) : d3.format(`$,.3~s`) ;
        #                    return format(value);}')
        # ))

        #useEnrollmentData
        outputChart
      }

      generate.section.df <- reactive({

        req(courseEnrollmentData)
        req(input$choiceUG)

        t.data <- courseEnrollmentData

        if("section" %in% names(t.data)) {
          t.data <- t.data %>%
            mutate(Section=section)
        }

        t.data$Section <- factor(t.data$Section)

        if(input$choiceUG == "1") {
          useSelectCourse <- input$selectCourseUG
        } else {
          useSelectCourse <- input$selectCourseGrad
        }

        if(!is.null(useSelectCourse)){
          t.data <- t.data %>%
            filter(Course==useSelectCourse)
        } else t.data <- c(NULL)

        t.data
      })

      generate.demographics.df <- reactive({
        t.data <- courseDemographicsData

        if(input$choiceUG == "1") {
          useSelectCourse <- input$selectCourseUG
        } else {
          useSelectCourse <- input$selectCourseGrad
        }

        if(input$selectYear != 3000){
          t.data <- t.data %>%
            filter(year==input$selectYear)
        }
        t.data <- t.data %>%
          filter(Course==useSelectCourse)
        t.data
      })

      makeEnrollmentPlot <- reactive({
        #cat("Entered makeEnrollmentPlot.  choiceSections:", input$choiceSections, "\n")
        local.df <- generate.small.df()
        print(local.df)
        if(!is.null(local.df)){
          cat("local.df was not NULL\n")
          print(dim(local.df))
          if(dim(local.df)[1]>0){
            if(input$choiceSections==TRUE){
              ggplot(generate.section.df(), aes(Section, Enrollment)) +
                geom_col() +
                theme(
                  legend.title = element_blank(),
                  axis.title.x = element_blank()) +
                geom_text(aes(label=Enrollment, vjust=-0.5)) +
                labs(y="Enrollment") +
                facet_wrap(~ Year, nrow=2)
            } else {
              ggplot(local.df, aes(full.semester, total_Enrollment)) +
                geom_col() +
                theme(axis.text.x = element_text(angle=90),
                      legend.title = element_blank(),
                      axis.title.x = element_blank()) +
                geom_text(aes(label=paste(total_Enrollment, " (", n..,")", sep=""), vjust=-0.5)) +
                labs(y="Enrollment")
            }
          }
        }
      })
      makeEnrollmentPlotC3 <- function(){
        local.df <- generate.small.df()
      }

      make.demographics.plot <- reactive({
        #cat("In make.demographics.plot\n")
        local.df <- generate.demographics.df()
        if(dim(local.df)[1]>0){
          ggplot(data = generate.demographics.df(), aes(x = "",  fill = factor(major) )) +
            geom_bar(position = position_fill()) +
            coord_polar(theta = "y") +
            facet_wrap(~ full.semester)  +
            theme(
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
        }
      })

      outputOptions(output, 'isData', suspendWhenHidden = FALSE)
      outputOptions(output, 'noSections', suspendWhenHidden = FALSE)
      outputOptions(output, 'NoDataText', suspendWhenHidden = FALSE)

    }
  )
}
