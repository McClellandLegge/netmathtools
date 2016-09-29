#' Get the grades your students for the courses you mentor
#'
#' @param course_id The fully qualified id for the course you'd like to pull grades for
#' @inheritParams get_mentor_courses
#' @param all Boolean, should all of the grades be pulled, or just the logged in Mentor's?
#' @param active Boolean, should only the active students be pulled?
#' @return A data frame
#' @export
get_grades_csv <- function(course_id, name, h = NULL, user = NULL,
                           passwd = NULL, all = FALSE, active = FALSE) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (is.null(h)) {
    login <- netmathtools::login(user, passwd)
    h <- login$handle
    name <- login$name
  }
  req <- paste0(netmathtools:::api, "/GetGradesCSV?courseId=", course_id)
  rtn <- data.table::fread(rawToChar(curl::curl_fetch_memory(req, handle = h)$content))
  if (nrow(rtn) == 0)
    return(NULL)
  rtn$FirstName <- gsub("([A-z]+), ([A-z]+)", "\\2", rtn$Name)
  rtn$LastName <- gsub("([A-z]+), ([A-z]+)", "\\1", rtn$Name)

  ss_rtn <- dplyr::select(rtn, Mentor, Status, Name, LastName, FirstName,
                          dplyr::contains("Try It"))
  m_rtn <- suppressWarnings(data.table::melt(ss_rtn, value.name = "Value",
                            id.vars = c("Mentor", "Status", "Name", "LastName", "FirstName")))
  m_rtn <- m_rtn[!grepl("^0", m_rtn$variable), ] # exclude try it 0
  m_rtn$Lesson <- as.numeric(gsub("(\\d+)\\.Try It (\\d{2})", "\\1", m_rtn$variable))
  m_rtn$TryIt <- as.numeric(gsub("(\\d+)\\.Try It (\\d{2})", "\\2", m_rtn$variable))
  m_rtn <- dplyr::select(m_rtn, -variable)

  if (isTRUE(all)) {
    if (isTRUE(active)) {
      return(m_rtn[m_rtn$Status == "Active", ])
    } else {
      return(m_rtn)
    }
  } else {
    if (active) {
      return(m_rtn[m_rtn$Mentor == name & m_rtn$Status == "Active", ])
    } else {
      return(m_rtn[m_rtn$Mentor == name, ])
    }
  }
}