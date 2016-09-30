#' Log into Mathable
#'
#' @param user Your username, e.g. <netid>@netmath.illinois.edu
#' @param passwd Your password Mathable password
#' @description Will log in the user, find the Mentor Name and return a curl handle
#' @return A list with the mentor name and curl handle
#' @export
login <- function(user, passwd) {
  h <- curl::new_handle()
  req <- paste0(netmathtools:::api, "/LogIn?LoginName=", user, "&Password=", passwd)
  rtn <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(req, handle = h)$content))$d

  if (!is.null(rtn$error$password))
    stop(rtn$error$password)

  name <- getElement(rtn, "name")
  writeLines(paste0(name, " logged in."))

  return(list(handle = h, name = name))
}