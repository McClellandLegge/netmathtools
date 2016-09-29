#' Get the grades your students progress summary
#'
#' @inheritParams get_all_grades
#' @param outfile A filename to write results to.
#' @return A data frame
#' @export
get_student_progress <- function(h = NULL, user = NULL, passwd = NULL
                                 , all = FALSE, active = FALSE, outfile = NULL) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }
  require("data.table")

  if (is.null(h)) {
    login <- netmathtools::login(user, passwd)
    h <- login$handle
    name <- login$name
  }

  ags <- setDT(netmathtools::get_all_grades(h))
  mvars <- c("CourseId", "Mentor", "Status", "LastName", "FirstName")
  mln <- ags[!Value %in% c("Saved", "Unopened", "Opened"),
             list(MaxLesson = max(Lesson)), by = mvars]
  mti <- merge(ags, mln, by.x = c(mvars, "Lesson"), by.y = c(mvars, "MaxLesson"))
  out <- mti[!Value %in% c("Saved", "Unopened", "Opened"),
      list(TryIt = max(TryIt)), by = c(mvars, "Lesson")][order(LastName, FirstName)]
  if (!is.null(outfile)) {
    writeLines("Writting to file...")
    write.csv(out, file = outfile, row.names = FALSE)
  }
  return(out)
}