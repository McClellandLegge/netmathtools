#' Get the grades your students progress summary
#'
#' @inheritParams get_all_grades
#' @param student_list A data.table (data.frame) of students with the structure LastName, FirstName,
#'    EndDate (in header) with the EndDate in the mm/dd/yyyy format
#' @param outfile A filename to write results to.
#' @param ... Arguments to `merge` when combining the student list and the progress results
#' @return A data frame with columns:
#' \itemize{
#'    \item \code{FirstName} The first name of the student
#'    \item \code{LastName} The last name of the student
#'    \item \code{CourseId} The fully qualified course ID in Mathable
#'    \item \code{CourseName} The "short name" for the course
#'    \item \code{Mentor} The mentor name
#'    \item \code{Status} The status of the student, e.g. "Completed", "Active" or "Withdrawn"
#'    \item \code{Lesson} The Lesson number
#'    \item \code{TryIt} The Try It number
#'    \item \code{EndDate} The student's end date
#'    \item \code{ProgressStatus} The status of the student determined by the
#'        criteria set by the NetMath office
#'    \item \code{DaysLeft} The number of days left in the course the student has
#'    \item \code{DaysBehind} The number of days behind the recommended schedule
#'        the student is
#'    \item \code{TryItsBehind} The number of Try Its behind the recommended
#'        schedule the student is
#'    \item \code{LessonsBehind} The number of Lessons behind the recommended
#'        schedule the student is
#'    \item \code{CurrentPace} The number of Try Its submitted per day the
#'        student is averaging
#'    \item \code{CurrentInterp} A plain-english (approximate) interpretation of
#'        what the student's current pace means about their submission behavior
#'    \item \code{TryItsLeft} The number of Try Its the student has left in the
#'        course
#'    \item \code{NeededPace} The number of Try Its per day the student must
#'        average in order to complete the course on time
#'    \item \code{NeededInterp} A plain-english (approximate) interpretation of
#'        what the \code{NeededPace} means in terms of needed submission behavior
#'        from the student. Intentionally rounded up to the half day
#' }
#' @examples
#' \dontrun{
#' # Read in a student list from a google sheet
#' netmath_students_rss <- googlesheets::gs_url("https://docs.google.com/spreadsheets/...")
#'
#' # Format to data.table from data.frame and format the student's end date to
#' # the R datatype \code{Date}
#' netmath_students <- as.data.table(gs_read(netmath_students_rss))
#' netmath_students$EndDate <- as.Date(netmath_students$EndDate, format = "%m/%d/%Y")
#'
#' # Get a progress update on the students in the student list.
#' # The 'all.y = TRUE' is telling the function to keep all students in the
#' # student list -- helps detect any possible mis-spellings of names
#' prog <- get_student_progress(user = "mkemp6@netmath.illinois.edu",
#'                             passwd = "<passwd>",
#'                             student_list = netmath_students[, .(LastName, FirstName, EndDate)],
#'                             all.y = TRUE,
#'                             active = TRUE)
#' }
#' @import data.table
#' @export
get_student_progress <- function(h = NULL, user = NULL, passwd = NULL, student_list
                                 , all_students = TRUE, active = FALSE, outfile = NULL, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("`plyr` needed for this function to work. Please install it.",
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

  full_list <- unique(ags[, mvars, with = FALSE])

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

  fl_out <- merge(full_list, out, by = mvars, all.x = TRUE)

  writeLines("Merging Student List...")
  # merge in the student list with the end dates, probably should make the date
  # format a parameter, but seems reasonable to expect a convention for now.
  # Return the non-matches for both the student list and the grade results, the
  # user had a choice to exclude students in the grades and undoubtly will want
  # to know if one of the students in their list was not found.
  student_list$EndDate <- as.Date(student_list$EndDate, format = "%m/%d/%Y")
  set <- merge(fl_out, student_list, by = c("FirstName", "LastName", "CourseId"), ...)

  course_done <- set[Status %in% c("Completed", "Withdrawn", "Cancelled")]
  course_done[, ProgressStatus := "Course Ended"]

  working <- set[!Status %in% c("Completed", "Withdrawn", "Cancelled")]

  writeLines("Calculating Student Progress...")
  # for each line in the resulting set, calculate the progress compared to the
  # recommended pace for the course. All boundry condition handling done in the
  # lower level function
  fset <- plyr::ddply(working, colnames(working), function(x){
    with(x, netmathtools::calc_progress(CourseId, Lesson, TryIt, EndDate))
  }) %>%
    rbind(., course_done, fill = TRUE)

  # setkey to order the columns
  data.table::setkey(fset, CourseName, Mentor, Status, LastName, FirstName)

  # if an output file is specified, then write to disk
  if (!is.null(outfile)) {
    writeLines("Writting to file...")
    write.csv(fset, file = outfile, row.names = FALSE)
  }

  # strangely, the return only works when there has been an `<-` or `=` assignment
  # to another variable... suspect this has to do with being a data.table but
  # need to investigate further
  return(fset)
}
