# All browse_ function implementations are inspired by usethis package

#' Browse a given GitHub repo
#'
#' Opens browser to a given GitHub repo. Inspired by similar `browse_` functions included in
#' the `usethis` package.
#'
#' @param repo_ref Repository reference as created by `create_repo_ref()`
#'
#' @return Returns URL in non-interactive session or launches browser to docs in interactive session
#' @export
#'
#' @examples
#' \dontrun{
#' my_repo <- create_repo_ref("repo_owner", "repo")
#' browse_repo(my_repo)
#' }

browse_repo <- function(repo_ref) {

  # base url varies depending on if private/ent or public
  url_prefix <-
    if(repo_ref$base_url != 'https://api.github.com/'){
      repo_ref$base_url
    }
  else{"https://github.com/"}

  view_url(url_prefix, repo_ref$repo_owner, repo_ref$repo_name)

}

#' Browse issues for given GitHub repo
#'
#' Opens browser to GitHub issues for a given repo. Inspired by similar `browse_` functions included in
#' the `usethis` package.
#'
#' @inheritParams browse_repo
#' @param number Optional argument of issue number, if opening page for specific issue is desired
#'
#' @return Returns URL in non-interactive session or launches browser to docs in interactive session
#' @export
#'
#' @examples
#' \dontrun{
#' my_repo <- create_repo_ref("repo_owner", "repo")
#' browse_issues(my_repo)
#' }

browse_issues <- function(repo_ref, number = "") {

  # base url varies depending on if private/ent or public
  url_prefix <-
    if(repo_ref$base_url != 'https://api.github.com/'){
      repo_ref$base_url
    }
  else{"https://github.com/"}

  view_url(url_prefix, repo_ref$repo_owner, repo_ref$repo_name, "issues", number)

}

#' Browse milestones for given GitHub repo
#'
#' Opens browser to GitHub milestones for a given repo. Inspired by similar `browse_` functions included in
#' the `usethis` package.
#'
#' @inheritParams browse_repo
#' @param number Optional argument of milestone ID, if opening page for specific milestone is desired
#'
#' @return Returns URL in non-interactive session or launches browser to docs in interactive session
#' @export
#'
#' @examples
#' \dontrun{
#' my_repo <- create_repo_ref("repo_owner", "repo")
#' browse_milestones(my_repo)
#' }

browse_milestones <- function(repo_ref, number = "") {

  # base url varies depending on if private/ent or public
  url_prefix <-
    if (repo_ref$base_url != 'https://api.github.com/') {
      repo_ref$base_url
    }
  else{"https://github.com/"}

  if (number == "") {
    view_url(url_prefix, repo_ref$repo_owner, repo_ref$repo_name, "milestones")
  }
  else{
    view_url(url_prefix, repo_ref$repo_owner, repo_ref$repo_name, "milestone", number)
  }

}


#' View GitHub API documentation
#'
#' Opens browser to relevant parts of GitHub API documentation to learn more about field
#' definitions and formatting. Inspired by similar `browse_` functions included in
#' the `usethis` package.
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
  object = c('milestone', 'issue', 'issue event', 'issue comment', 'repo labels')){

  # validate inputs ----
  action <- match.arg(action)
  object <- match.arg(object)

  # define mapping ----
  browse_data <-
    data.frame(stringsAsFactors = FALSE,
      action = rep(c("get", "post"), each = 5),
      object = rep(c("milestone", "issue", "issue event", "repo labels", "issue comment"), 2),
      url_suffix = c("issues/milestones/#list-milestones-for-a-repository",
                     "issues/#list-issues-for-a-repository",
                     "issues/events/#list-events-for-an-issue",
                     "issues/labels/#list-all-labels-for-this-repository",
                     "issues/comments/#list-comments-on-an-issue",
                     "issues/milestones/#create-a-milestone",
                     "issues/#create-an-issue",
                     NA,
                     "issues/labels/#create-a-label",
                     "issues/comments/#create-a-comment")
  )

  # create url ----
  if (action == 'post' & object == 'issue event') {
    stop("No relevant documentation for posting an issue event. Issue events are consequences of other actions.")
  }

  url_prefix <- "https://developer.github.com/v3"
  url_suffix <- browse_data$url_suffix[browse_data$action == action & browse_data$object == object]

  # render results ----
  view_url(url_prefix, url_suffix)

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
