#' Parse issues overview from \code{get_issues}
#'
#' @param res List returned by corresponding \code{get_} function
#' @return \code{tibble} datasets with one record / issue
#' @export
#'
#' @family parse
#' @family issues
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_reference('emilyriederer', 'myrepo')
#' issues_res <- get_issues(myrepo)
#' issues <- parse_issues(issues_res)
#' }

parse_issues <- function(res){

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  title = res[[.]]$title,
                  body = res[[.]]$body %||% NA,
                  state = res[[.]]$state,
                  created_at = as.Date(res[[.]]$created_at %>% substring(1,10)),
                  closed_at = as.Date(substring(res[[.]]$closed_at %||% NA, 1,10)),
                  created_by = res[[.]]$user$login,
                  n_comments = res[[.]]$comments,
                  url = res[[.]]$html_url,
                  id = res[[.]]$number,
                  milestone_title = res[[.]]$milestone$title %||% NA,
                  milestone_id = res[[.]]$milestone$id %||% NA,
                  milestone_state = res[[.]]$milestone$state %||% NA,
                  due_on = substring(res[[.]]$milestone$due_on %||% NA,1,10),
                  point_of_contact = res[[.]]$assignee$login %||% NA,
                  assignee = list(res[[.]]$assignees %>% purrr::map_chr('login')),
                  label = list(res[[.]]$labels %>% purrr::map_chr('name'))
                ))

}

#' Parse issue events from \code{get_issues_events}
#'
#' @inheritParams parse_issues
#' @return \code{tibble} datasets with one record / issue-event
#' @export
#'
#' @family parse
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

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  id = res[[.]]$id,
                  actor = res[[.]]$actor$login,
                  event = res[[.]]$event,
                  created_at = res[[.]]$created_at,
                  label = res[[.]]$label$name %||% NA,
                  milestone_title = res[[.]]$milestone$title %||% NA
                ))

}

#' Parse milestones from \code{get_milestones}
#'
#' @inheritParams parse_issues
#' @return `tibble` datasets with one record / milestone
#' @export
#'
#' @family parse
#' @family milestones
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref("emilyriederer", "myrepo")
#' milestones_res <- get_milestones(myrepo)
#' milestones <- parse_milestones(milestones_res)
#' }

parse_milestones <- function(res){

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  title = res[[.]]$title,
                  number = res[[.]]$number,
                  description = res[[.]]$description %||% NA,
                  creator = res[[.]]$creator$login,
                  open_issues = res[[.]]$open_issues,
                  closed_issues = res[[.]]$closed_issues,
                  state = res[[.]]$state,
                  created_at = as.Date(substring(res[[.]]$created_at %||% NA, 1, 10)),
                  updated_at = as.Date(substring(res[[.]]$updated_at %||% NA, 1, 10)),
                  due_on = as.Date(substring(res[[.]]$due_on %||% NA,1,10)),
                  closed_at = as.Date(substring(res[[.]]$closed_at %||% NA,1,10))
                ))

}
