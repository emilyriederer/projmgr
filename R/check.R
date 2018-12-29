#' Find requests remaining and reset time
#'
#' Source: copied from \code{httr} vignette "Best practices for API packages" by Hadley Wickham
#'
#' @param ref Any repository reference being used. Repository information is stripped out and
#'     only authentication credentials are used to determine the rate limit.
#'
#' @return Informative message on requests remaining and reset time
#' @export
#'
#' @family check
#'
#' @examples
#' \dontrun{
#' experigit <- create_repo_ref('emilyriederer', 'experigit')
#' check_rate_limit(experigit)
#' }

check_rate_limit <- function(ref){

  ref[['repo_path']] <- ''
  req <- get_engine("rate_limit", ref)
  core <- req[[1]]$resources$core
  reset <- as.POSIXct(core$reset, origin = "1970-01-01")
  cat(core$remaining, " / ", core$limit,
      " (Resets at ", strftime(reset, "%H:%M:%S"), ")\n",
      sep = "")

}

#' Check for valid authentication credentials
#'
#' @param ref Any repository reference being used. Repository information is stripped out and
#'     only authentication credentials are used to determine the rate limit.
#'
#' @return If valid, returns related GitHub username. Else returns 401 Unauthorized error.
#' @export
#'
#' @examples
#' \dontrun{
#' experigit <- create_repo_ref('emilyriederer', 'experigit')
#' check_authentication(experigit)
#' }

check_authentication <- function(ref){

  ref[['repo_path']] <- ''
  req <- get_engine("user", ref)
  cat("+ Login: ", req[[1]]$login, "\n", "+ Type: ", req[[1]]$type, "\n", sep = '')

}


#' Check internet connection (re-export of curl::has_internet())
#'
#' Basic wrapper around \code{curl::has_internet()}
#'
#' @return Returns TRUE is connected to internet and false otherwise
#' @export
#'
#' @family check
#'
#' @examples
#' \dontrun{
#' check_internet()
#' }

check_internet <- function(){

  # check if curl package installed
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package \"curl\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  curl::has_internet()

}
