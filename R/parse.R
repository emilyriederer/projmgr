#' Parse issues overview from \code{get_issues}
#'
#' @param res List returned by corresponding \code{get_} function
#' @return \code{tibble} datasets with one record / issue
#' @export
#'
#' @family issues
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_reference('emilyriederer', 'myrepo')
#' issues_res <- get_issues(myrepo)
#' issues <- parse_issues(issues_res)
#' }

parse_issues <- function(res){

  if(is.character(res)){stop("Results object contains no elements to parse.")}

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  title = res[[.]]$title,
                  body = res[[.]]$body %||% NA,
                  state = res[[.]]$state,
                  created_at = as.Date(res[[.]]$created_at %>% substring(1,10)),
                  closed_at = as.Date(substring(res[[.]]$closed_at %||% NA, 1,10)),
                  user_login = res[[.]]$user$login,
                  n_comments = res[[.]]$comments,
                  url = res[[.]]$html_url,
                  number = res[[.]]$number,
                  milestone_title = res[[.]]$milestone$title %||% NA,
                  milestone_id = res[[.]]$milestone$id %||% NA,
                  milestone_state = res[[.]]$milestone$state %||% NA,
                  due_on = substring(res[[.]]$milestone$due_on %||% NA,1,10),
                  assignee_login = res[[.]]$assignee$login %||% NA,
                  assignees_login = list(res[[.]]$assignees %>% purrr::map_chr('login')),
                  labels_name = list(res[[.]]$labels %>% purrr::map_chr('name'))
                ))

}

#' Parse issue events from \code{get_issues_events}
#'
#' @inheritParams parse_issues
#' @return \code{tibble} datasets with one record / issue-event
#' @export
#'
#' @family issues
#' @family events
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' events_res <- get_issue_events(myrepo, number = 1)
#' events <- parse_issue_events(events_res)
#' }

parse_issue_events <- function(res){

  if(is.character(res)){stop("Results object contains no elements to parse.")}

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  number = res[[.]]$number,
                  id = res[[.]]$id,
                  actor_login = res[[.]]$actor$login,
                  event = res[[.]]$event,
                  created_at = as.Date(res[[.]]$created_at %>% substring(1,10)),
                  label_name = res[[.]]$label$name %||% NA,
                  milestone_title = res[[.]]$milestone$title %||% NA
                ))

}

#' Parse issue comments from \code{get_issues_comments}
#'
#' @inheritParams parse_issues
#' @inherit get_issue_comments examples
#' @return \code{tibble} datasets with one record / issue-comment
#' @export
#'
#' @family issues
#' @family comments

parse_issue_comments <- function(res){

  if(is.character(res)){stop("Results object contains no elements to parse.")}

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  url = res[[.]]$html_url,
                  id = res[[.]]$id,
                  user_login = res[[.]]$user$login,
                  created_at = as.Date(substring(res[[.]]$created_at %||% NA, 1, 10)),
                  updated_at = as.Date(substring(res[[.]]$updated_at %||% NA, 1, 10)),
                  author_association = res[[.]]$author_association,
                  body = res[[.]]$body,
                  number = res[[.]]$number
                ))

}

#' Parse milestones from \code{get_milestones}
#'
#' @inheritParams parse_issues
#' @return `tibble` datasets with one record / milestone
#' @export
#'
#' @family milestones
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref("emilyriederer", "myrepo")
#' milestones_res <- get_milestones(myrepo)
#' milestones <- parse_milestones(milestones_res)
#' }

parse_milestones <- function(res){

  if(is.character(res)){stop("Results object contains no elements to parse.")}

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  title = res[[.]]$title,
                  number = res[[.]]$number,
                  description = res[[.]]$description %||% NA,
                  creator_login = res[[.]]$creator$login,
                  n_open_issues = res[[.]]$open_issues,
                  n_closed_issues = res[[.]]$closed_issues,
                  state = res[[.]]$state,
                  created_at = as.Date(substring(res[[.]]$created_at %||% NA, 1, 10)),
                  updated_at = as.Date(substring(res[[.]]$updated_at %||% NA, 1, 10)),
                  due_on = as.Date(substring(res[[.]]$due_on %||% NA,1,10)),
                  closed_at = as.Date(substring(res[[.]]$closed_at %||% NA,1,10))
                ))

}

#' Parse labels from \code{get_repo_labels}
#'
#' @inheritParams parse_issues
#' @return `tibble` datasets with one record / label
#' @export
#'
#' @family labels
#'
#' @inherit get_repo_labels examples

parse_repo_labels <- function(res){

  if(is.character(res)){stop("Results object contains no elements to parse.")}

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  name = res[[.]]$name,
                  url = res[[.]]$url,
                  color = res[[.]]$color,
                  default = res[[.]]$default,
                  id = res[[.]]$id,
                  node_id = res[[.]]$node_id
                ))

}
