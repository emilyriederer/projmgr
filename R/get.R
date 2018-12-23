#' Get issues from GitHub repository
#'
#' @inherit get_engine return params
#' @export
#' @family get
#' @family issues
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' issues_res <- get_issues(myrepo)
#' issues <- parse_issues(issues_res)
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
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' events_res <- get_issue_events(myrepo, number = 1)
#' events <- parse_issue_events(events_res)
#' }

get_issue_events <- function(ref, number){

  res <- get_engine(api_endpoint = paste0("/issues/", number, "/events"),
             ref = ref)

  # append the relevant issue number to each element
  res <- purrr::map(res, ~purrr::list_modify(., "number" = number))

  res

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
#' myrepo <- create_repo_ref("emilyriederer", "myrepo")
#' milestones_res <- get_milestones(myrepo)
#' milestones <- parse_milestones(milestones_res)
#' }

get_milestones <- function(ref, ...){

  validate_inputs(list(...),
                  allowed_vars = c("state", "sort", "direction"))

  get_engine(api_endpoint = "/milestones",
             ref = ref,
             ...)

}
