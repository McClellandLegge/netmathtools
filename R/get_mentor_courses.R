#' Get the courses for which you mentor
#'
#' @param h A valid handle to a Mathable session (logged in and cookies in jar)
#' @inheritParams login
#' @return A data frame
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