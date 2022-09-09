############################################
# Read the enrollment demographics data    #
#                                          #
# Data are from two sources:               #
#   2014-2020 : Sp/Su/F are custom queries #
#               Data saved as xls and csv  #
#               file formats.              #
#   2021-     : Howdy enrollment reports   #
#                                          #
#   2014-2020 are final course enrollments #
#   2021-     are incremental daily data   #
#                                          #
############################################

local.path <- "//Volumes//GoogleDrive//My Drive//"
local.archive.path <- paste0(local.path, "ShinyApps//LocalDataArchive//")
inFile <- paste0(local.archive.path, "EnrollmentDemographics//201921.xls")
howdy.file.path <- paste0(local.path, "ShinyApps//EnrollmentAndScheduling//Data")
custom.file.path <- paste0(local.archive.path, "EnrollmentDemographics")
inFileHowdy <- paste0(local.path, "ShinyApps//EnrollmentAndScheduling//Data//202231//PWS_COURSE_BY_MAJ_CNTS_20220823.csv")
inFileHowdy <- paste0(local.path, "ShinyApps//EnrollmentAndScheduling//Data//202031//Student_course_by_major Report_20200914_Fall 2020.csv")

keepSubjects <- c("GEOG", "PSYC", "PBSI", "CHEM", "AALO", "ANTH", "AFST", "ARAB",
                  "ARTS", "ASIA", "ASTR", "ATMO", "BIOL", "BOTN", "CHIN", "CLAS",
                  "COMM", "ECMT", "ECON", "ENGL", "ENST", "EURO", "FILM", "FREN",
                  "GEOL", "GEOP", "GEOS", "GERM", "HBRW", "HISP", "HIST", "HUMA",
                  "INST", "ITAL", "JAPN", "JOUR", "LBAR", "LING", "LMAS", "MATH",
                  "METR", "MICR", "MODL", "MUSC", "MUST", "NAUT", "NRSC", "OCNG",
                  "PERF", "PHIL", "PHYS", "PORT", "RELS", "RUSS", "SCEN", "SOCI",
                  "SPAN", "STAT", "UGST", "WGSC", "ZOOL")

#' readCustomDemographicsFile
#'
#' @param inFile input file in custom format
#' @param keepSubjects subjects to keep in the output
#'
#' @return
#' @export
#'
#' @examples
#'
readCustomDemographicsFile <- function(inFile, keepSubjects){
  rawData <- tryCatch(
    {
      #cat("2Trying to read:", inFile, "\n")
      readxl::read_excel(inFile)  %>%
        data.frame()
    },
    error = function(cond){
      #message("Data are not an excel file.  Reading it as a .csv file.")
      read.csv(inFile, na.strings="")
    },
    finally = {
      #cat("here\n")
    }
  )

  #browser()
  processedData <- rawData %>%
    filter(!stringr::str_detect(COURSE_SECTION_NUMBER, '^[a-zA-Z]')) %>%
    mutate(COURSE_SECTION_NUMBER=as.numeric(COURSE_SECTION_NUMBER)) %>%
    filter(COURSE_SECTION_NUMBER >= 100) %>%
    filter(SUBJECT %in% keepSubjects) %>%
    unite("tData", c("STUDENT_COUNT", starts_with("X")), na.rm=TRUE, sep=",") %>%
    mutate(STUDENT_COUNT=tData) %>%
    select(-"tData")

  #if(!is.numeric(processedData$COURSE_SECTION_NUMBER)) browser()
  #cat("is.numeric(COURSE_SECTION_NUMBER):", is.numeric(processedData$COURSE_SECTION_NUMBER), "\n")
  nmax <- max(stringr::str_count(processedData$STUDENT_COUNT, ","), na.rm=TRUE) + 1

  processedData <- processedData %>%
    separate(col=STUDENT_COUNT, into=paste0("col", seq_len(nmax)), sep=",", fill="right") %>%
    pivot_longer(cols=starts_with("col"),
                 names_to="columnName",
                 names_prefix="col",
                 values_to="enrollment",
                 values_drop_na = TRUE) %>%
    select(-"columnName") %>%
    separate(col=enrollment, into=c("nStudents", "major"), sep="-", fill="right") %>%
    filter(!is.na(major)) %>%
    mutate(year=as.numeric(ACADEMIC_PERIOD) %/% 100) %>%
    mutate(semesterCode = (as.numeric(ACADEMIC_PERIOD)-year*100) %/% 10) %>%
    mutate(semester = case_when(semesterCode == 1 ~ "Spring",
                                semesterCode == 2 ~ "Summer",
                                semesterCode == 3 ~ "Fall")) %>%
    select("semester"="ACADEMIC_PERIOD", "subject"="SUBJECT", "course.number"="COURSE_NUMBER",
           "section"="COURSE_SECTION_NUMBER", "major", "Semester.1"="semester", "year",
           "enrolledStudents"="nStudents")



  processedData
}

#' readHowdyDemographicsFile
#'
#' @param inFile input file
#' @param keepSubjects subjects to keep
#'
#' @return
#' @export
#'
#' @examples
readHowdyDemographicsFile <- function(inFile, keepSubjects){
  rawData <- tryCatch(
    {
      #cat("1Trying to read:", inFile, "\n")
      readxl::read_excel(inFile)  %>%
        data.frame()
    },
    error = function(cond){
      #message("Data are not an excel file.  Reading it as a .csv file.")
      read.csv(inFile, na.strings="", row.names=NULL)
    },
    finally = {
      #cat("here\n")
    }
  )
  #browser()
  processedData <- tryCatch(
    {
      if(length(names(rawData))>5){
        rawData <- rawData %>%
          select("TERM"="row.names", "COURSE"="TERM", "SECT_NUM"="COURSE", "MAJOR"="SECT_NUM", "STUDENT_COUNT"="MAJOR")
      }
      processedData <- rawData %>%
        mutate(subject=substr(COURSE,1,4)) %>%
        filter(subject %in% keepSubjects) %>%
        mutate(course.number=as.numeric(substr(COURSE,5,7))) %>%
        mutate(year=as.numeric(TERM) %/% 100) %>%
        mutate(semesterCode = (as.numeric(TERM)-year*100) %/% 10) %>%
        mutate(Semester.1 = case_when(semesterCode == 1 ~ "Spring",
                                      semesterCode == 2 ~ "Summer",
                                      semesterCode == 3 ~ "Fall")) %>%
        select("semester"="TERM", "subject", "course.number", "section"="SECT_NUM",
               "major"="MAJOR", "Semester.1", "year", "enrolledStudents"="STUDENT_COUNT")
      processedData
    },
    error = function(cond){
      readCustomDemographicsFile(inFile, keepSubjects)
    }
  )

  if(nrow(processedData)==0){
    theOutput <- NULL
  } else {
    theOutput <- processedData
  }
  processedData
}

#' combineCustomDemographicsFiles
#'
#' @param inLocationOfCustomFiles path to the custom demographics files
#' @param keepSubjects subjects to keep
#'
#' @return
#' @export
#'
#' @examples
combineCustomDemographicsFiles <- function(inLocationOfCustomFiles, keepSubjects){
  theOutput <- NULL
  theFiles <- list.files(inLocationOfCustomFiles, pattern=".xls|.csv",
                         full.names=TRUE)
  for(i in 1:length(theFiles)){
    theOutput <- rbind(theOutput, readCustomDemographicsFile(theFiles[i], keepSubjects))
  }
  theOutput
}

#' combineHowdyDemographicsFiles
#'
#' @param inLocationOfFiles path to the demographics files downloaded from Howdy
#' @param keepSubjects subjects to keep
#'
#' @return
#' @export
#'
#' @examples

combineHowdyDemographicsFiles <- function(inLocationOfFiles, keepSubjects){
  theOutput <- NULL
  theSemesters <- list.dirs(inLocationOfFiles)
  # remove the first one (parent directory)
  theSemesters <- theSemesters[2:length(theSemesters)]
  # extract last 6 characters of paths
  last6 <- substr(theSemesters, nchar(theSemesters)-5, nchar(theSemesters))
  # check the format of last6
  numericLast6 <- tryCatch(
    {
      as.numeric(last6)
    },
    error = function(cond){
      message("One of the directories is not able to be converted to numeric.")
    },
    finally = {
      #cat("here\n")
    }
  )

  for(i in 1:length(numericLast6)){
    cat(crayon::red(numericLast6[i],"\n"))
    semesterPath <- paste0(inLocationOfFiles, "//", as.character(numericLast6[i]))
    theFiles <- list.files(semesterPath)
    theFileDates <- tryCatch(
      {
        as.numeric(substr(theFiles, nchar(theFiles)-21,nchar(theFiles)-14))
      },
      warning = function(cond){
        as.numeric(substr(theFiles, nchar(theFiles)-11, nchar(theFiles)-4))
      }
    )
    maxFileDate <- max(theFileDates)
    #read the newest file for each directory
    t.1 <- stringr::str_detect(theFiles, as.character(maxFileDate))
    file2Read <- paste0(semesterPath, "//", theFiles[t.1])
    theOutput <- rbind(theOutput, readHowdyDemographicsFile(file2Read, keepSubjects))
  }

  theOutput
}

#' combineHowdyEnrollmentTrackingFiles
#'
#' @param inLocationOfFiles location of input files
#' @param keepSubjects subjects to keep
#'
#' @return
#' @export
#'
#' @examples
combineHowdyEnrollmentTrackingFiles <- function(inLocationOfFiles, keepSubjects){
  ########################################################
  # Similar to combineHowdyDemographics Files, but       #
  # contains individual date information and not just    #
  # the most recent data.                                #
  ########################################################read
  theOutput <- NULL
  theSemesters <- list.dirs(inLocationOfFiles)
  # remove the first one (parent directory)
  theSemesters <- theSemesters[2:length(theSemesters)]
  # extract last 6 characters of paths
  last6 <- substr(theSemesters, nchar(theSemesters)-5, nchar(theSemesters))
  # check the format of last6
  numericLast6 <- tryCatch(
    {
      as.numeric(last6)
    },
    error = function(cond){
      message("One of the directories is not able to be converted to numeric.")
    },
    finally = {
      #cat("here\n")
    }
  )

  numSemesters2Read <- length(numericLast6)

  for(i in 1:numSemesters2Read){
    semesterPath <- paste0(inLocationOfFiles, "//", as.character(numericLast6[i]))
    theFiles <- list.files(semesterPath)
    theFileDates <- tryCatch(
      {
        as.numeric(substr(theFiles, nchar(theFiles)-21,nchar(theFiles)-14))
      },
      warning = function(cond){
        as.numeric(substr(theFiles, nchar(theFiles)-11, nchar(theFiles)-4))
      }
    )
    maxFileDate <- max(theFileDates)  #The most recent data available for this semester

    files2Read <- theFiles            #Change this later to allow only reading new files

    numFiles <- length(files2Read)

    for(j in 1:numFiles){
      cat(files2Read[j], "\n")
      theFileCreationDate <- tryCatch(
        {
          as.numeric(substr(files2Read[j], nchar(files2Read[j])-21,nchar(files2Read[j])-14))
        },
        warning = function(cond){
          as.numeric(substr(files2Read[j], nchar(files2Read[j])-11, nchar(files2Read[j])-4))
        }
      )
      theYear <- substr(theFileCreationDate,1,4)
      theMonth <- substr(theFileCreationDate,5,6)
      theDay <- substr(theFileCreationDate, 7,8)

      file2Read <- paste0(semesterPath, "//", files2Read[j])
      rawFile <- readHowdyDemographicsFile(file2Read, keepSubjects)

      if(nrow(rawFile)==0){
        processedFile <- NULL
      } else {
        processedFile <- rawFile %>%
          mutate(course.designation = paste(subject, course.number)) %>%
          mutate(course.designation1 = paste0(subject, course.number, "-", section)) %>%
          mutate(dateCreated=paste0(theYear, "-", theMonth, "-", theDay)) %>%
          mutate(timeStamp=ymd_hms(paste0(dateCreated, "010101"))) %>%
          select("semester", "subject", "courseNumber"="course.number", "courseSection"="section", "major",
                 "course.designation", "course.designation1", "dateCreated", "timeStamp", "numEnrolled"="enrolledStudents") %>%
          mutate(numEnrolled=as.numeric(numEnrolled))
      }
      theOutput <- rbind(theOutput, processedFile)

    }
  }

  theOutput
}

#' combineBothFileTypes
#'
#' @param inLocationOfCustomFiles path to the custom demographics files
#' @param inLocationOfFiles path to the demographics files downloaded from Howdy
#' @param keepSubjects subjects to keep
#'
#' @return
#' @export
#'
#' @examples
combineBothFileTypes <- function(inLocationOfCustomFiles, inLocationOfFiles, keepSubjects){
  `%out%` <- function(a,b) ! a %in% b
  combinedHowdyFiles <- combineHowdyDemographicsFiles(inLocationOfFiles, keepSubjects)
  combinedCustomFiles <- combineCustomDemographicsFiles(inLocationOfCustomFiles, keepSubjects)

  allFiles <- combinedCustomFiles %>%
    filter(semester %out% unique(combinedHowdyFiles$semester)) %>%
    rbind(combinedHowdyFiles) %>%
    mutate(Semester.1 = as.factor(Semester.1)) %>%
    mutate(enrolledStudents = as.numeric(enrolledStudents))

  allFiles
}

#t.data <- readCustomDemographicsFile(inFile, keepSubjects)
#t.data <- readHowdyDemographicsFile(inFileHowdy, keepSubjects)
#t.data1 <- combineHowdyDemographicsFiles(howdy.file.path, keepSubjects)
#t.data2 <- combineCustomDemographicsFiles(custom.file.path, keepSubjects)

#t.data4 <- combineBothFileTypes(custom.file.path, howdy.file.path, keepSubjects)


#t.data1 <- combineHowdyEnrollmentTrackingFiles(howdy.file.path, keepSubjects)
