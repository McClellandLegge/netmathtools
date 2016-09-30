#' Log into Mathable
#'
#' @param user Your username, e.g. <netid>@netmath.illinois.edu
#' @param passwd Your password Mathable password
#' @description Will log in the user, find the Mentor Name and return a curl handle
#' @return A list with the mentor name and curl handle
#' @export
login <- function(user, passwd) {
  # create a new curl handle, which automatically saves the cookies upon
  # successfull login. Convert from a JSON object just to save the Mentor name
  h <- curl::new_handle()
  req <- paste0(netmathtools:::api, "/LogIn?LoginName=", user, "&Password=", passwd)
  rtn <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(req, handle = h)$content))$d

  # Raise any error that the curl call produced
  if (!is.null(rtn$error$password))
    stop(rtn$error$password)

  # extract the Mentor name from the data.frame version of the returned JSON object
  name <- getElement(rtn, "name")
  writeLines(paste0(name, " logged in."))

  # return the handle and the name separately in a list
  return(list(handle = h, name = name))
}