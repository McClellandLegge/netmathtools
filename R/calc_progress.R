#' Calculate a student's status in their course
#'
#' @param course_id The course id for which the student is enrolled, used to
#'    select the correct schedule
#' @param latest_lesson The latest lesson that the student has completed
#' @param latest_tryit The latest Try It that the student has completed
#' @param end_date The end date of the student, must be an R date object
#' @import data.table magrittr
#' @return A data table with columns:
#' \itemize{
#'    \item \code{ProgressStatus} The status of the student determined by the
#'        criteria set by the NetMath office
#'    \item \code{DaysLeft} The number of days left in the course the student has
#'    \item \code{DaysBehind} The number of days behind the recommended schedule
#'        the student is
#'    \item \code{TryItsBehind} The number of Try Its behind the recommended
#'        schedule the student is
#'    \item \code{LessonsBehind} The number of Lessons behind the recommended
#'        schedule the student is
#'    \item \code{CurrentPace} The number of Try Its submitted per day the
#'        student is averaging
#'    \item \code{CurrentInterp} A plain-english (approximate) interpretation of
#'        what the student's current pace means about their submission behavior
#'    \item \code{TryItsLeft} The number of Try Its the student has left in the
#'        course
#'    \item \code{NeededPace} The number of Try Its per day the student must
#'        average in order to complete the course on time
#'    \item \code{NeededInterp} A plain-english (approximate) interpretation of
#'        what the \code{NeededPace} means in terms of needed submission behavior
#'        from the student. Intentionally rounded up to the half day
#' }
#' @examples
#' calc_progress("deployedcourses/uiuc_netmath_math461_r2012_mm",
#'               latest_lesson = 4,
#'               latest_tryit = 3,
#'               end_date = as.Date("12/25/2016", format = "%m/%d/%Y"))
#' @export
calc_progress <- function(course_id, latest_lesson, latest_tryit, end_date) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }
  if (! requireNamespace("magrittr", quietly = TRUE)) {
    stop("`magrittr` needed for this function to work. Please install it.", call. = FALSE)
  }

  if (is.na(latest_lesson)) {
    latest_lesson <- latest_tryit <- 1
  }

  if (is.na(end_date)) {
    return(data.table::data.table(ProgressStatus = "No End Date",
                                  DaysLeft = NA,
                                  DaysBehind = NA,
                                  TryItsBehind = NA,
                                  LessonsBehind = NA,
                                  CurrentPace = NA,
                                  CurrentInterp = NA,
                                  TryItsLeft = NA,
                                  NeededPace = NA,
                                  NeededInterp = NA))
  }

  days_left <- as.numeric(end_date - Sys.Date())
  sched <- setDT(getElement(netmathtools:::schedules, course_id))
  if (nrow(sched) == 0) {
    return(data.table::data.table(ProgressStatus = "No Schedule",
                                  DaysLeft = days_left,
                                  DaysBehind = NA,
                                  TryItsBehind = NA,
                                  LessonsBehind = NA,
                                  CurrentPace = NA,
                                  CurrentInterp = NA,
                                  TryItsLeft = NA,
                                  NeededPace = NA,
                                  NeededInterp = NA))
  }

  if (days_left < 0) {
    return(data.table::data.table(ProgressStatus = "Course Ended",
                                  DaysLeft = NA,
                                  DaysBehind = NA,
                                  TryItsBehind = NA,
                                  LessonsBehind = NA,
                                  CurrentPace = NA,
                                  CurrentInterp = NA,
                                  TryItsLeft = NA,
                                  NeededPace = NA,
                                  NeededInterp = NA))
  }

  interval <- 0.5 # (days)
  at_day <- max(sched[Lesson == latest_lesson & `Try It` == latest_tryit]$Days)
  should_day <- max(nrow(sched) - days_left - 1, 1)
  should_be <- sched[should_day, list(Days = max(Days)), by = .(Lesson, `Try It`)]
  actually <- sched[Days == at_day, list(Days = max(Days)), by = .(Lesson, `Try It`)]
  days_diff <- should_be$Days - actually$Days
  tryits_diff <- nrow(unique(sched[(Days <= should_be$Days & Days > actually$Days) |
                                     (Days > should_be$Days & Days <= actually$Days),
                                   c("Lesson", "Try It"), with = FALSE]))
  lesson_diff <- should_be$Lesson - actually$Lesson
  tryits_left <- sched[Days > at_day, .(Lesson, `Try It`)] %>%
    unique %>% nrow
  needed_pace <-  round(tryits_left / days_left, digits = 2)
  current_pace <- sched[Days <= at_day, .(Lesson, `Try It`)] %>%
    unique %>% nrow %>% magrittr::divide_by(should_day) %>% round(digits = 2)
  np_interp <- paste0("need to submit an average of ", ceiling(needed_pace / interval) * interval, " Try It(s) a day")
  cp_interp <- ifelse(current_pace < 1,
                      paste0("currently submitting a Try It every ",
                             round(1 / current_pace / interval) * interval, " days"),
                      paste0("currently submitting ",
                             round(current_pace / interval) * interval, " Try It(s) a day"))


  res <- data.table::data.table(
    DaysLeft = days_left,
    DaysBehind = days_diff,
    TryItsBehind = tryits_diff,
    LessonsBehind = lesson_diff,
    CurrentPace = current_pace,
    CurrentInterp = cp_interp,
    TryItsLeft = tryits_left,
    NeededPace = needed_pace,
    NeededInterp = np_interp,
    dummy = lesson_diff)
  setkey(netmathtools:::status, low, high)
  sa <- foverlaps(res, netmathtools:::status, by.y = c("low", "high"),
                  by.x = c("LessonsBehind", "dummy"))
  out <- dplyr::select(sa, -low, -high, -dummy)
  return(out)
}