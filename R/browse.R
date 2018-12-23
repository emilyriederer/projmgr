#' View GitHub API documentation
#'
#' Read relevant parts of documentation for the GitHub API to learn more about
#'
#' @param action Character string denoting action you wish to complete: "get" (list existing) or "post" (creating new)
#' @param object Character string denoting object on wish you want to apply an action. Supports
#'     "milestone", "issue", "issue event"
#'
#' @return Returns URL in non-interactive session or launches browser to docs in interactive session
#' @export
#'
#' @examples
#' \dontrun{
#' browse_docs('get', 'milestone')
#' }

browse_docs <- function(
  action = c('get', 'post'),
  object = c('milestone', 'issue', 'issue event')){

  action <- match.arg(action)
  object <- match.arg(object)

  if(action == 'post' & object == 'issue event'){
    stop("No relevant documentation for posting an issue event. Issue events are consequences of other actions.")
  }

  url_prefix <- "https://developer.github.com/v3"

  # url mapping
  urls <- tibble::tribble(
    ~action, ~object, ~url_suffix,
    'get', 'milestone', "issues/milestones/#list-milestones-for-a-repository",
    'get', 'issue', "issues/#list-issues-for-a-repository",
    'get', 'issue event', "issues/events/#list-events-for-an-issue",
    'post', 'milestone', "issues/milestones/#create-a-milestone",
    'post', 'issue', "issues/#create-an-issue",
    'post', 'issue event', NA_character_
  )

  view_url(url_prefix, urls$url_suffix[urls$action == action & urls$object == object])

}


# Internal function to support opening to GitHub documentation
# Functionality and implementation inspired by usethis
#' @keywords internal

view_url <- function(..., open = interactive()) {
url <- paste(..., sep = "/")
if (open) {
  cat(paste("Opening URL", url))
  utils::browseURL(url)
} else {
  cat(paste("Open URL", url))
}
invisible(url)
}
