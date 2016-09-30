#' Get the grades your students progress summary
#'
#' @inheritParams get_all_grades
#' @param student_list A csv file of students with the structure LastName, FirstName,
#'    EndDate (in header) with the EndDate in the mm/dd/yyyy format
#' @param outfile A filename to write results to.
#' @return A data frame
#' @import data.table
#' @export
get_student_progress <- function(h = NULL, user = NULL, passwd = NULL, student_list
                                 , all = FALSE, active = FALSE, outfile = NULL) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (is.null(h)) {
    login <- netmathtools::login(user, passwd)
    h <- login$handle
    name <- login$name
  }

  writeLines("Extracting Grades...")
  ags <- setDT(netmathtools::get_all_grades(h))

  writeLines("Calculating Most Recent Assignments...")
  mvars <- c("CourseId", "Mentor", "Status", "LastName", "FirstName")
  mln <- ags[!Value %in% c("Saved", "Unopened", "Opened"),
             list(MaxLesson = max(Lesson)), by = mvars]
  mti <- merge(ags, mln, by.x = c(mvars, "Lesson"), by.y = c(mvars, "MaxLesson"))
  out <- mti[!Value %in% c("Saved", "Unopened", "Opened"),
      list(TryIt = max(TryIt)), by = c(mvars, "Lesson")][order(LastName, FirstName)]

  writeLines("Merging Student List...")
  sl <- data.table::fread(student_list)
  sl$EndDate <- as.Date(sl$EndDate, format = "%m/%d/%Y")
  set <- merge(out, sl, by = c("FirstName", "LastName"), all.y = TRUE)

  writeLines("Calculating Student Progress...")
  set[!is.na(CourseId),
      c("Status", "DaysBehind", "TryItsBehind") := rbindlist(
        mapply(netmathtools::calc_progress, CourseId, Lesson, TryIt, EndDate,
               SIMPLIFY = FALSE, USE.NAMES = FALSE))]

  if (!is.null(outfile)) {
    writeLines("Writting to file...")
    write.csv(set, file = outfile, row.names = FALSE)
  }
  return(set)
}
