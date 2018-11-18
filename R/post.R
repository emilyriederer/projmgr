#' Post issue to GitHub repository
#'
#' @inherit post_engine return params
#' @param title Issue title (required)
#' @export
#' @family post, issues
#'
#' @examples
#' \dontrun{
#' tidytracker <- create_repo_ref('emilyriederer', 'tidytracker')
#' post_issue(tidytracker,
#'   title = 'this is my issue's title',
#'   body = 'this is my issue's body',
#'   labels = c('priority:high', 'bug'))
#' }
#' \dontrun{
#' # can be used in conjunction with reprex pkg
#' # example assumes code for reprex is on clipboard
#' reprex(venue = "gh")
#' post_issue(tidytracker,
#'             title = "something is broken",
#'             body = paste( clipr::read_clip(), collapse = "\n") )
#' }

post_issue <- function(ref, title, ...){

  validate_inputs(list(...),
                  allowed_vars = c("body", "milestone",
                                   "labels", "assignees"))

  post_engine(api_endpoint = "/issues",
             ref = ref,
             title = title,
             ...)

}

#' Post milestone to GitHub repository
#'
#' @inherit post_engine return params
#' @param title Milestone title (required)
#' @export
#' @family post, milestones
#'
#' @examples
#' \dontrun{
#' tidytracker <- create_repo_ref('emilyriederer', 'tidytracker')
#' post_milestone(tidytracker,
#'   title = 'this is my milestone's title',
#'   description = 'this is my milestone's description',
#'   due_on = '2018-12-31T12:59:59z')
#' }

post_milestone <- function(ref, title, ...){

  validate_inputs(list(...),
                  allowed_vars = c("state", "description", "due_on"))

  post_engine(api_endpoint = "/milestones",
              ref = ref,
              title = title,
              ...)

}
