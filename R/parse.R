# Define internal helper functions for formatting different datatypes
fmt_safe_chr <- function(x) {ifelse( is.null(x) , NA_character_, x )}
fmt_safe_int <- function(x) {ifelse( is.null(x) , NA_integer_, x)}
fmt_safe_lgl <- function(x) {ifelse( is.null(x), NA, x)}
fmt_safe_date <- function(x) {ifelse( is.null(x), NA, as.Date( substring(x, 1, 10)))}

#' Parse issues overview from \code{get_issues}
#'
#' @param res List returned by corresponding \code{get_} function
#' @return data.frame with one record / issue
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

parse_issues <- function( res ) {

  mapped_elts <-
    sapply( res ,
            FUN = function(x)
              data.frame( url = fmt_safe_chr( x[["html_url"]] ),
                          id = fmt_safe_int( x[["id"]] ),
                          number = fmt_safe_int( x[["number"]] ),
                          title = fmt_safe_chr( x[["title"]] ),
                          user_login = fmt_safe_chr( x[["user"]]$login ),
                          user_id = fmt_safe_int( x[["user"]]$id ),
                          state = fmt_safe_chr( x[["state"]] ),
                          locked = fmt_safe_lgl( x[["locked"]] ),
                          milestone_title = fmt_safe_chr( x[["milestone"]]$title ),
                          milestone_id = fmt_safe_int( x[["milestone"]]$id ),
                          milestone_number = fmt_safe_int( x[["milestone"]]$number ),
                          milestone_state = fmt_safe_chr( x[["milestone"]]$state ),
                          milestone_created_at = fmt_safe_date( x[["milestone"]]$due_on ),
                          milestone_closed_at = fmt_safe_date( x[["milestone"]]$due_on ),
                          milestone_due_on = fmt_safe_date( x[["milestone"]]$due_on ),
                          n_comments = fmt_safe_int( x[["comments"]] ),
                          created_at = fmt_safe_date( x[["created_at"]] ),
                          updated_at = fmt_safe_date( x[["updated_at"]] ),
                          closed_at = fmt_safe_date( x[["closed_at"]] ),
                          author_association = fmt_safe_chr( x[["author_association"]] ),
                          body = fmt_safe_chr( x[["body"]] ),
                          stringsAsFactors = FALSE
              ),
            simplify = FALSE
    )

  # special handling for list columns ----
  labels_names <- sapply(issues,
                         FUN = function(y)
                           vapply(y[["labels"]], FUN = function(x) x$name, FUN.VALUE = character(1) ))
  assignees_login <- sapply(issues,
                            FUN = function(y)
                              vapply(y[["assignees"]], FUN = function(x) x$login, FUN.VALUE = character(1) ))

  data <- do.call(rbind, mapped_elts)
  data$labels_name <- labels_names
  data$assignees_login <- assignees_login

  return(data)

}

#' Parse issue events from \code{get_issues_events}
#'
#' This function convert list output returned by get into a dataframe. Due to the diverse
#' fields for different types of events, many fields in the dataframe may be NA.
#'
#' Currently, the following event types are unsupported (with regard to processing all
#' of their fields) due to their additional bulk and limited utility with respect to
#' this packages functionality. Please file an issue if you disagree:
#' \itemize{
#'  \item{"(removed_from/moved_columns_in/added_to)_project"}{Since this package has limited value with GitHub projects}
#'  \item{"converted_note_to_issue"}{Since issue lineage is not a key concern}
#'  \item{"head_ref_(deleted/restored)"}{Since future support for pull requests would likely be handled separately}
#'  \item{"merged"}{Same justification as head_ref}
#'  \item{"review_(requested/dismissed/request_removed)}{Same justification as head_ref}
#' }
#'
#' @inheritParams parse_issues
#' @return Dataframe with one record / issue-event
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

                  # label events
                  label_name = res[[.]]$label$name %||% NA,

                  # milestone events
                  milestone_title = res[[.]]$milestone$title %||% NA,

                  # assignment events
                  assignee_login = res[[.]]$assignee$login %||% NA,
                  assigner_login = res[[.]]$assigner$login %||% NA,

                  # rename events
                  rename_from = res[[.]]$rename$from %||% NA,
                  rename_to = res[[.]]$rename$to %||% NA
                ))

}

#' Parse issue comments from \code{get_issues_comments}
#'
#' @inheritParams parse_issues
#' @inherit get_issue_comments examples
#' @return Dataframe with one record / issue-comment
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
#' @return Dataframe with one record / milestone
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
                  url = res[[.]]$html_url,
                  created_at = as.Date(substring(res[[.]]$created_at %||% NA, 1, 10)),
                  updated_at = as.Date(substring(res[[.]]$updated_at %||% NA, 1, 10)),
                  due_on = as.Date(substring(res[[.]]$due_on %||% NA,1,10)),
                  closed_at = as.Date(substring(res[[.]]$closed_at %||% NA,1,10))
                ))

}

#' Parse labels from \code{get_repo_labels}
#'
#' @inheritParams parse_issues
#' @return Dataframe with one record / label
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

