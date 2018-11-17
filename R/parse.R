#' Parse issues overview from \code{get_issues}
#'
#' @param res List returned by corresponding \code{get_} function
#' @return \code{tibble} datasets with one record / issue
#' @export
#'
#' @family parse, issues
#'
#' @examples
#' \dontrun{
#' tidytracker <- create_repo_reference('emilyriederer', 'tidytracker')
#' tt_issues_res <- get_issues(tidytracker)
#' tt_issues <- parse_issues(tt_issues_res)
#' }

parse_issues <- function(res){

  purrr::map_df(1:length(res),
                ~tibble::tibble(
                  title = res[[.]]$title,
                  body = res[[.]]$body %||% NA,
                  state = res[[.]]$state,
                  created_at = res[[.]]$created_at %>% substring(1,10),
                  closed_at = substring(res[[.]]$closed_at %||% NA, 1,10),
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

#' Parse milestones from \code{get_milestones}
#'
#' @inheritParams parse_issues
#' @return `tibble` datasets with one record / milestone
#' @export
#'
#' @family parse, milestones
#'
#' @examples
#' \dontrun{
#' tidytracker_ref <- create_repo_ref("emilyriederer", "tidytracker")
#' tt_milestones_res <- get_milestones(tidytracker_ref)
#' tt_milestones <- parse_milestones(tt_milestones_res)
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
                  created_at = substring(res[[.]]$created_at %||% NA, 1, 10),
                  updated_at = substring(res[[.]]$updated_at %||% NA, 1, 10),
                  due_on = substring(res[[.]]$due_on %||% NA,1,10),
                  closed_at = substring(res[[.]]$closed_at %||% NA,1,10)
                ))

}
