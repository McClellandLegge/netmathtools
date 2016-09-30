#' Calculate a student's status in their course
#'
#' @param course_id The course id for which the student is enrolled, used to
#'    select the correct schedule
#' @param latest_lesson The latest lesson that the student has completed
#' @param latest_tryit The latest Try It that the student has completed
#' @param end_date The end date of the student
#' @import data.table
#' @return A data frame
#' @export
calc_progress <- function(course_id, latest_lesson, latest_tryit, end_date) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }

  sched <- setDT(getElement(netmathtools:::schedules, course_id))
  if (nrow(sched) == 0) {
    # warning(paste("No schedule populated for class:", course_id))
    return(data.table::data.table(Status = "No Schedule", DaysBehind = NA, TryItsBehind = NA))
  }

  days_left <- as.numeric(end_date - Sys.Date())
  if (days_left < 0) {
    return(data.table::data.table(Status = "Course Ended", DaysBehind = NA, TryItsBehind = NA))
  }

  should_be <- sched[nrow(sched) - days_left - 1, list(Days = max(Days)), by = .(Lesson, `Try It`)]
  actually <- sched[Lesson == latest_lesson & `Try It` == latest_tryit, list(Days = max(Days)), by = .(Lesson, `Try It`)]
  days_diff <- should_be$Days - actually$Days
  tryits_diff <- nrow(unique(sched[Days <= should_be$Days & Days > actually$Days,
                                   c("Lesson", "Try It"), with = FALSE]))
  res <- data.table::data.table(DaysBehind = days_diff, dummy = days_diff,
                                TryItsBehind = tryits_diff)
  setkey(netmathtools:::status, low, high)
  sa <- foverlaps(res, netmathtools:::status, by.y = c("low", "high"),
                  by.x = c("DaysBehind", "dummy"))
  out <- dplyr::select(sa, -low, -high, -dummy)
  return(out)
}