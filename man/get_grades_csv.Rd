% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_grades_csv.R
\name{get_grades_csv}
\alias{get_grades_csv}
\title{Get the grades your students for the courses you mentor}
\usage{
get_grades_csv(course_id, name, h = NULL, user = NULL, passwd = NULL,
  all_students = FALSE, active = FALSE)
}
\arguments{
\item{course_id}{The fully qualified id for the course you'd like to pull grades for}

\item{h}{A valid handle to a Mathable session (logged in and cookies in jar)}

\item{user}{Your username, e.g. <netid>@netmath.illinois.edu}

\item{passwd}{Your password Mathable password}

\item{all_students}{Boolean, should all of the grades be pulled, or just the logged in Mentor's?}

\item{active}{Boolean, should only the active students be pulled?}
}
\value{
A data frame with columns:
\itemize{
   \item \code{Mentor} The mentor name
   \item \code{Status} The status of the student, e.g. "Completed", "Active" or "Withdrawn"
   \item \code{Name} The full name of the student in Last Name, First Name form
   \item \code{LastName} The last name of the student
   \item \code{FirstName} The first name of the student
   \item \code{Value} The grade/status of the Try It
   \item \code{Lesson} The Lesson number
   \item \code{TryIt} The Try It number
}
}
\description{
Get the grades your students for the courses you mentor
}
\examples{
\dontrun{
### login on the fly (does not preserve session for subsequent calls)
get_grades_csv(course_id = "deployedcourses/uiuc_netmath_math461_r2012_mm",
               name = "Mcclelland Kemp",
               user = "mkemp6@netmath.illinois.edu",
               passwd = "<passwd>")

### Use an existing session
session <- login("mkemp6@netmath.illinois.edu", "<passwd>")
get_grades_csv(course_id = "deployedcourses/uiuc_netmath_math461_r2012_mm",
               name = "Mcclelland Kemp",
               h = session$handle)
}
}
