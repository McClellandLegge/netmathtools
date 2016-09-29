#' Get the courses for which you mentor
#'
#' @param h A valid handle to a Mathable session (logged in and cookies in jar)
#' @inheritParams login
#' @return A data frame
#' @export
get_mentor_courses <- function(h = NULL, user = NULL, passwd = NULL) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }

  if (is.null(h)) {
    h <- netmathtools::login(user, passwd)$handle
  }
  req <- paste0(netmathtools:::api, "/GetMentorCourses")
  rtn <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(req, handle = h)$content))$d
  return(data.table::as.data.table(rtn))
}