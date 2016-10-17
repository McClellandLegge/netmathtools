#' Calculate a student's status in their course
#'
#' @param course_id The course id for which the student is enrolled, used to
#'    select the correct schedule
#' @param latest_lesson The latest lesson that the student has completed
#' @param latest_tryit The latest Try It that the student has completed
#' @param end_date The end date of the student
#' @import data.table magrittr
#' @return A data frame
#' @export
calc_progress <- function(course_id, latest_lesson, latest_tryit, end_date) {
  if (! requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.", call. = FALSE)
  }
  if (! requireNamespace("magrittr", quietly = TRUE)) {
    stop("`magrittr` needed for this function to work. Please install it.", call. = FALSE)
  }

  if (is.na(end_date)) {
    return(data.table::data.table(ProgressStatus = "No End Date",
                                  DaysLeft = NA,
                                  DaysBehind = NA,
                                  TryItsBehind = NA,
                                  LessonsBehind = NA,
                                  CurrentPace = NA,
                                  CurrentInterp = NA,
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
                                  NeededPace = NA,
                                  NeededInterp = NA))
  }

  interval <- 0.5 # (days)
  at_day <- max(sched[Lesson == latest_lesson & `Try It` == latest_tryit]$Days)
  should_day <- nrow(sched) - days_left - 1
  should_be <- sched[should_day, list(Days = max(Days)), by = .(Lesson, `Try It`)]
  actually <- sched[Days == at_day, list(Days = max(Days)), by = .(Lesson, `Try It`)]
  days_diff <- should_be$Days - actually$Days
  tryits_abs <- nrow(unique(sched[(Days <= should_be$Days & Days > actually$Days) |
                                     (Days > should_be$Days & Days <= actually$Days),
                                   c("Lesson", "Try It"), with = FALSE]))
  tryits_diff <- ifelse(sign(days_diff) == -1L, -tryits_abs, tryits_abs)
  lesson_diff <- should_be$Lesson - actually$Lesson
  needed_pace <- sched[Days > at_day, .(Lesson, `Try It`)] %>%
    unique %>% nrow %>% magrittr::divide_by(days_left) %>% round(digits = 2)
  current_pace <- sched[Days <= at_day, .(Lesson, `Try It`)] %>%
    unique %>% nrow %>% magrittr::divide_by(should_day) %>% round(digits = 2)
  np_interp <- paste0("Needs to submit ", ceiling(needed_pace / interval) * interval, " Try It(s) a day")
  cp_interp <- ifelse(current_pace < 1,
                      paste0("Currently submitting a Try It every ",
                             ceiling(1 / current_pace / interval) * interval, " days"),
                      paste0("Currently submitting ",
                             floor(current_pace / interval) * interval, " Try Its(s) a day"))


  res <- data.table::data.table(
    DaysLeft = days_left,
    DaysBehind = days_diff,
    TryItsBehind = tryits_diff,
    LessonsBehind = lesson_diff,
    CurrentPace = current_pace,
    CurrentInterp = cp_interp,
    NeededPace = needed_pace,
    NeededInterp = np_interp,
    dummy = lesson_diff)
  setkey(netmathtools:::status, low, high)
  sa <- foverlaps(res, netmathtools:::status, by.y = c("low", "high"),
                  by.x = c("LessonsBehind", "dummy"))
  out <- dplyr::select(sa, -low, -high, -dummy)
  return(out)
}