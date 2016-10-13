#' Get the grades your students progress summary
#'
#' @inheritParams get_all_grades
#' @param student_list A data.table (data.frame) of students with the structure LastName, FirstName,
#'    EndDate (in header) with the EndDate in the mm/dd/yyyy format
#' @param outfile A filename to write results to.
#' @param ... Arguments to `merge` when combining the student list and the progress results
#' @return A data frame
#' @import data.table
#' @export
get_student_progress <- function(h = NULL, user = NULL, passwd = NULL, student_list
                                 , all_students = TRUE, active = FALSE, outfile = NULL, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # if no handle is provided, require that the user and password be passed to
  # initialize an on-the-fly login
  if (is.null(h)) {
    login <- netmathtools::login(user, passwd)
    h <- login$handle
    name <- login$name
  }

  writeLines("Extracting Grades...")
  # extract all the grades for all the courses for which the logged in mentor
  # has access to, return as a data.frame
  ags <- data.table::setDT(netmathtools::get_all_grades(h, name = name,
                                                        all_students = all_students, active = active))

  writeLines("Calculating Most Recent Assignments...")
  mvars <- c("CourseId", "CourseName", "Mentor", "Status", "LastName", "FirstName")
  # calculate the latest Lesson in which each student in each course has either
  # submitted an assignment or had one graded
  mln <- ags[!Value %in% c("Saved", "Unopened", "Opened"),
             list(MaxLesson = max(Lesson)), by = mvars]

  # merge back in with the original set in order to limit the lesson to only
  # the max lesson for each student and course calculated in the last step
  mti <- merge(ags, mln, by.x = c(mvars, "Lesson"), by.y = c(mvars, "MaxLesson"))

  # find the latest Try It submitted in the latest Lessons
  out <- mti[!Value %in% c("Saved", "Unopened", "Opened"),
      list(TryIt = max(TryIt)), by = c(mvars, "Lesson")][order(LastName, FirstName)]

  writeLines("Merging Student List...")
  # merge in the student list with the end dates, probably should make the date
  # format a parameter, but seems reasonable to expect a convention for now.
  # Return the non-matches for both the student list and the grade results, the
  # user had a choice to exclude students in the grades and undoubtly will want
  # to know if one of the students in their list was not found.
  student_list$EndDate <- as.Date(student_list$EndDate, format = "%m/%d/%Y")
  set <- merge(out, student_list, by = c("FirstName", "LastName"), ...)

  writeLines("Calculating Student Progress...")
  # for each line in the resulting set, calculate the progress compared to the
  # recommended pace for the course. All boundry condition handling done in the
  # lower level function
  set[!is.na(CourseId),
      c("ProgressStatus", "DaysBehind", "TryItsBehind", "LessonsBehind") := data.table::rbindlist(
        mapply(netmathtools::calc_progress, CourseId, Lesson, TryIt, EndDate,
               SIMPLIFY = FALSE, USE.NAMES = FALSE))]
  set[is.na(CourseId), ProgressStatus := "Not Found in Mathable"]

  # setkey to order the columns
  data.table::setkey(set, CourseName, Mentor, Status, LastName, FirstName)

  # if an output file is specified, then write to disk
  if (!is.null(outfile)) {
    writeLines("Writting to file...")
    write.csv(set, file = outfile, row.names = FALSE)
  }

  # strangely, the return only works when there has been an `<-` or `=` assignment
  # to another variable... suspect this has to do with being a data.table but
  # need to investigate further
  return(set)
}
