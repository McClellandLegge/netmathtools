#' Get the grades your students for the courses you mentor
#'
#' @param course_id The fully qualified id for the course you'd like to pull grades for
#' @inheritParams get_mentor_courses
#' @param all_students Boolean, should all of the grades be pulled, or just the logged in Mentor's?
#' @param active Boolean, should only the active students be pulled?
#' @return A data frame
#' @import data.table
#' @export
get_grades_csv <- function(course_id, name, h = NULL, user = NULL,
                           passwd = NULL, all_students = FALSE, active = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`dplyr` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # if no handle is provided, require that the user and password be passed to
  # initialize an on-the-fly login
  if (is.null(h)) {
    login <- netmathtools::login(user, passwd)
    h <- login$handle
    name <- login$name
  }

  # attempt the curl request, which should return a csv if successful, we then
  # read the csv on the fly into memory as a data.table for convenience
  req <- paste0(netmathtools:::api, "/GetGradesCSV?courseId=", course_id)
  rtn <- suppressWarnings(data.table::fread(rawToChar(curl::curl_fetch_memory(req, handle = h)$content)))

  # if the curl request is not successful, kick back a null object
  if (nrow(rtn) == 0)
    return(NULL)

  # extract the first and last name with some ugly regex... NEEDS WORK!
  rtn[, `:=`(
    FirstName = gsub("([A-z]+), ([A-z]+)", "\\2", Name)
    , LastName = gsub("([A-z]+), ([A-z]+)", "\\1", Name)
  )]

  # try to handle any unusual names without commas, assume the first string is
  # the first name and all others will be included in the last name
  rtn[!grepl(",", Name), `:=`(
    FirstName = gsub("([A-z]+) ([A-z ]+)", "\\1", Name)
    , LastName = gsub("([A-z]+) ([A-z ]+)", "\\2", Name)
  )]

  # select only the columns that are explicitly specified and those that contain
  # the phrase "Try It", which excludes Basics, Tutorials and Literacy for
  # simplicify of the calculation
  ss_rtn <- dplyr::select(rtn, Mentor, Status, Name, LastName, FirstName,
                          dplyr::contains("Try It"))

  # convert to long form to facilitate the calculations
  m_rtn <- suppressWarnings(data.table::melt(ss_rtn, value.name = "Value",
                            id.vars = c("Mentor", "Status", "Name", "LastName", "FirstName")))

  # exclude Lesson 0 records
  m_rtn <- m_rtn[!grepl("^0", m_rtn$variable), ]

  # strip out the Lesson and Try It number from the title
  m_rtn$Lesson <- as.numeric(gsub("(\\d+)\\.Try It (\\d{2})", "\\1", m_rtn$variable))
  m_rtn$TryIt <- as.numeric(gsub("(\\d+)\\.Try It (\\d{2})", "\\2", m_rtn$variable))

  # drop the intermediate variable
  m_rtn <- dplyr::select(m_rtn, -variable)

  # decide what to return based on the user input, should only active students
  # be returned? Should only the logged in mentor's students be returned?
  if (isTRUE(all_students)) {
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