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

#' Check for valid credentials and repo permissions
#'
#' @param ref Any repository reference being used. Repository information is stripped out and
#'     only authentication credentials are validated.
#'
#' @return Prints GitHub username as determined by credentials (if valid) and repo-level permissions (if any),
#'     else throws 401 Unauthorized error.
#' @export
#'
#' @examples
#' \dontrun{
#' experigit <- create_repo_ref('emilyriederer', 'experigit')
#' check_authentication(experigit)
#' }

check_credentials <- function(ref){

  # get information of authenticating user
  auth_ref <- ref
  auth_ref[['repo_path']] <- ''
  auth_req <- get_engine("user", auth_ref)

  # get information on repo collaborators
  perm_req <- try(get_engine("/collaborators", ref), silent = TRUE)
  if("try-error" %in% class(perm_req)){
  perm_req <- list(admin = FALSE, push = FALSE, pull = FALSE)
  }
  else{
  login_match <- vapply(perm_req,
                        FUN = function(x) x[["login"]] == auth_req[[1]]$login,
                        FUN.VALUE = logical(1))
  perm_req <- perm_req[[which(login_match)]]$permissions
  }

  cat("-- With provided credentials -- \n",
      "+ Login: ", auth_req[[1]]$login, "\n",
      "+ Type: ", auth_req[[1]]$type, "\n",
      "-- In the ", ref$repo_name, " repo -- \n",
      "+ Admin: ", perm_req$admin, "\n",
      "+ Push: ", perm_req$push, "\n",
      "+ Pull: ", perm_req$pull, "\n",
      sep = '')

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
