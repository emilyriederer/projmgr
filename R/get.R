#' Get issues from GitHub repository
#'
#' @inherit get_engine return params
#' @export
#' @family get
#' @family issues
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
                                   "number", "sort", "direction",
                                   "since"))

  get_engine(api_endpoint = "/issues",
             ref = ref,
             ...)

}

#' Get events for a specific issue from GitHub repository
#'
#' @inherit get_engine return params
#' @param number Number of issue
#' @export
#'
#' @family get
#' @family issues
#' @family events
#'
#' @examples
#' \dontrun{
#' tidytracker <- create_repo_ref('emilyriederer', 'tidytracker')
#' tt_events_res <- get_issue_events(tidytracker, number = 1)
#' tt_events <- parse_issue_events(tt_events_res)
#' }

get_issues_events <- function(ref, number){

  get_engine(api_endpoint = paste0("/issues/", number, "events"),
             ref = ref)

}

#' Get milestones from GitHub repository
#'
#' @inherit get_engine return params
#' @export
#'
#' @family get
#' @family milestones
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
