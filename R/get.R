#' Get issues from GitHub repository
#'
#' @inherit get_engine return params
#' @export
#' @family get, issues
#'
#' @examples
#' \dontrun{
#' tidytracker <- create_repo_ref('emilyriederer', 'tidytracker')
#' tt_issues_res <- get_issues(tidytracker)
#' tt_issues <- parse_issues(tt_issues_res)
#' }

get_issues <- function(ref, ...){

  validate_inputs(list(...),
                  allowed_vars = c("milestone", "state", "assignee",
                                   "creator", "mentioned", "labels",
                                   "sort", "direction", "since"))

  get_engine(api_endpoint = "/issues",
             ref = ref,
             ...)

}

#' Get milestones from GitHub repository
#'
#' @inherit get_engine return params
#' @export
#' @family get, milestones
#'
#' @examples
#' \dontrun{
#' tidytracker <- create_repo_ref("emilyriederer", "tidytracker")
#' tt_milestones_res <- get_milestones(tidytracker)
#' milestones <- parse_milestones(tt_milestones_res)
#' }

get_milestones <- function(ref, ...){

  validate_inputs(list(...),
                  allowed_vars = c("state", "sort", "direction"))

  get_engine(api_endpoint = "/milestones",
             ref = ref,
             ...)

}
