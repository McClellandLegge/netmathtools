#' Get the all grades your students for the courses you mentor
#'
#' @inheritParams get_mentor_courses
#' @param all Boolean, should all of the grades be pulled, or just the logged in Mentor's?
#' @param active Boolean, should only the active students be pulled?
#' @return A data frame
#' @import data.table
#' @export
get_all_grades <- function(h = NULL, user = NULL, passwd = NULL, name = NULL, all = FALSE, active = FALSE) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }

  if (is.null(h)) {
    login <- netmathtools::login(user, passwd)
    h <- login$handle
    name <- login$name
  }

  crs <- netmathtools::get_mentor_courses(h)
  rtn <- sapply(crs$CourseId, netmathtools::get_grades_csv, h = h, name = name,
                all = all, active = active, simplify = FALSE, USE.NAMES = TRUE)

  rl <- data.table::rbindlist(rtn, fill = TRUE, use.names = TRUE, idcol = "CourseId")
  rl_ci <- dplyr::select(merge(rl, crs, by = "CourseId"), -StudentCourseRecordId, -CourseCode)

  return(rl_ci)
}