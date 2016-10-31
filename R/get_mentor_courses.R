#' Get the courses for which you mentor
#'
#' @param h A valid handle to a Mathable session (logged in and cookies in jar)
#' @inheritParams login
#' @return A data frame with columns:
#' \itemize{
#'    \item \code{StudentCourseRecordId} The student course record id for the
#'        Mentor, who is always a student in the courses they have access to Mentor
#'    \item \code{CourseId} The fully-qualified Mathable course ID
#'    \item \code{CourseName} The "short name" of the course
#'    \item \code{CourseCode} The code for the course which usually includes the
#'        abbreviated course name (e.g. MATH220), the year it was created and the
#'        section (e.g. MM, HS or semester for EGR sections)
#' }
#' @examples
#' \dontrun{
#' # Use an existing session
#' session <- login("mkemp6@netmath.illinois.edu", "<passwd>")
#' get_mentor_courses(h = session$handle)
#'
#' # Or login on the fly (does not preserve session for subsequent calls)
#' get_mentor_courses(user = "mkemp6@netmath.illinois.edu", passwd = "<passwd>")
#' }
#' @import data.table
#' @export
get_mentor_courses <- function(h = NULL, user = NULL, passwd = NULL) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }

  # if no handle is provided, require that the user and password be passed to
  # initialize an on-the-fly login
  if (is.null(h)) {
    h <- netmathtools::login(user, passwd)$handle
  }

  # execute the curl call, convert the JSON object returned to a data.frame
  req <- paste0(netmathtools:::api, "/GetMentorCourses")
  rtn <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(req, handle = h)$content))$d

  # convert the data.frame to a data.table for convenience and return
  return(data.table::as.data.table(rtn))
}