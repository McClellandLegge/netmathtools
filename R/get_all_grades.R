#' Get the all grades your students for the courses you mentor
#'
#' @inheritParams get_mentor_courses
#' @param all_students Boolean, should all of the grades be pulled, or just the logged in Mentor's?
#' @param active Boolean, should only the active students be pulled?
#' @return A data frame with columns:
#' \itemize{
#'    \item \code{CourseId} The fully qualified course ID in Mathable
#'    \item \code{Mentor} The mentor name
#'    \item \code{Status} The status of the student, e.g. "Completed", "Active" or "Withdrawn"
#'    \item \code{Name} The full name of the student in Last Name, First Name form
#'    \item \code{LastName} The last name of the student
#'    \item \code{FirstName} The first name of the student
#'    \item \code{Value} The grade/status of the Try It
#'    \item \code{Lesson} The Lesson number
#'    \item \code{TryIt} The Try It number
#'    \item \code{CourseName} The "short name" for the course
#' }
#' @examples
#' \dontrun{
#' session <- login("mkemp6@netmath.illinois.edu", "<passwd>")
#' get_all_grades(h = session$handle, name = session$name)
#' }
#' @import data.table
#' @export
get_all_grades <- function(h = NULL, user = NULL, passwd = NULL, name = NULL, all_students = FALSE, active = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }

  # if no handle is provided, require that the user and password be passed to
  # initialize an on-the-fly login
  if (is.null(h)) {
    login <- netmathtools::login(user, passwd)
    h <- login$handle
    name <- login$name
  }

  # get the courses for which the logged in mentor has access to
  crs <- netmathtools::get_mentor_courses(h)

  # for each course, get the grades and return as a named list to allow us to
  # merge back in the other course meta information
  rtn <- sapply(crs$CourseId, netmathtools::get_grades_csv, h = h, name = name,
                all_students = all_students, active = active, simplify = FALSE, USE.NAMES = TRUE)

  # stack the grades, keeping the course courseID in the 'idcol'
  rl <- data.table::rbindlist(rtn, fill = TRUE, use.names = TRUE, idcol = "CourseId")

  # merge back in the course meta-information, drop undesired columns
  rl_ci <- dplyr::select(merge(rl, crs, by = "CourseId"),
                         -StudentCourseRecordId, -CourseCode)

  return(rl_ci)
}