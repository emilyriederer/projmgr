#' Get issues from GitHub repository
#'
#' @inherit get_engine return params
#' @param limit Number of records to return, passed directly to \code{gh} documentation. Defaults to
#'     1000 and provides message if number of records returned equals the limit
#'
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

get_issues <- function(ref, limit = 1000, ...){

  args <- list(...)

  if("number" %in% names(args)){

    message("'number' parameter supercedes all other options passed to get_issues")
    res <- get_engine(api_endpoint = paste("/issues/", args$number), ref = ref)
    return(res)

  }

  validate_inputs(args,
                  allowed_vars = c("milestone", "state", "assignee",
                                   "creator", "mentioned", "labels",
                                   "sort", "direction","since"))

  res <- get_engine(api_endpoint = "/issues",
             ref = ref,
             limit = limit,
             ...)

  # notify user if seem to be hitting record limit
  if(length(res) == limit){
    message(
      paste("Number of results equals max number allowed by limit argument.",
             "Additional relevant records may be getting excluded.",
            "Consider making your query more specific or increasing the limit."),
      sep = "/n")
  }

  return(res)

}

#' Get events for a specific issue from GitHub repository
#'
#' In addition to information returned by GitHub API, appends field "number" for the issue number
#' to which the returned events correspond.
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

#' Get comments for a specific issue from GitHub repository
#'
#' In addition to information returned by GitHub API, appends field "number" for the issue number
#' to which the returned comments correspond.
#'
#' @inherit get_engine return params
#' @param number Number of issue
#' @export
#'
#' @family get
#' @family issues
#' @family comments
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' comments_res <- get_issue_comments(myrepo, number = 1)
#' comments <- parse_issue_comments(comments_res)
#' }

get_issue_comments <- function(ref, number, ...){

    validate_inputs(list(...), allowed_vars = c("since"))

  res <- get_engine(api_endpoint = paste0("/issues/", number, "/comments"),
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


#' Get all labels for a repository
#'
#' @inherit get_engine return params
#' @param ... Additional arguments (none currently implemented on GitHub API)
#' @export
#'
#' @family get
#' @family labels
#'
#' @examples
#' \dontrun{
#' labels <- get_repo_labels(my_repo) %>% parse_repo_labels()
#' }

get_repo_labels <- function(ref, ...) {

  validate_inputs(list(...),
                  allowed_vars = c(""))

  get_engine(api_endpoint = "/labels",
             ref = ref,
             ...)

}

