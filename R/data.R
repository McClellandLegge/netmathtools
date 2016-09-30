#' Recommended Schedules for all the NetMath courses
#'
#' A R list structure with named elements of the courses containing schedules with the following layout:
#' @usage netmathtools:::schedules
#' @format A data.frame with N rows representing the expected length of the course
#' \describe{
#'   \item{Days}{Day number, since the beginning of the course, ranges from 1-N}
#'   \item{Lesson}{The Lesson expected to be completed on Days = k}
#'   \item{Try It}{The Try It in Lesson expected to be completed on Days = k}
#' }
"schedules"

#' Progress status table defining the Green - Red criteria
#'
#' A R data.table structure containing the criteria for assiging the progress status
#' @usage netmathtools:::status
#' @format A data.frame
#' \describe{
#'   \item{low}{The low value on the range}
#'   \item{high}{The high value on the range}
#'   \item{ProgressStatus}{The designation that should be assigned for that range}
#' }
"status"

#' The URL for the Mathable API endpoint being useds
#'
#' @usage netmathtools:::api
#' @format A character string
"api"