schedules <- list(
  `deployedcourses/uiuc_netmath_math461_r2012_mm` = data.table::fread("data-raw/netmath461.csv")
  , `deployedcourses/uiuc_netmath_ma461egr_fs2016_fall16` = data.table::fread("data-raw/netmath461EGR.csv")
)

status <- data.table::data.table(
  low = c(-999, 1, 3, 4)
  , high = c(0, 2, 3, 999)
  , ProgressStatus = c("Green", "Yellow", "Orange", "Red")
)

api <- netmathtools:::api
devtools::use_data(schedules, api, status, internal = TRUE, overwrite = TRUE)
