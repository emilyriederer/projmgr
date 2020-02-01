# Define internal helper functions for formatting different datatypes
fmt_safe_chr <- function(x) {ifelse( is.null(x) , NA_character_, x )}
fmt_safe_int <- function(x) {ifelse( is.null(x) , NA_integer_, x)}
fmt_safe_lgl <- function(x) {ifelse( is.null(x), NA, x)}
fmt_safe_date <- function(x) {as.Date( substring( ifelse( is.null(x), NA, x) , 1, 10)) }

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
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' issues_res <- get_issues(myrepo)
#' issues <- parse_issues(issues_res)
#' }

parse_issues <- function( res ) {

  if(is.character(res)){stop("Results object contains no elements to parse.")}

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
                          milestone_created_at = fmt_safe_date( x[["milestone"]]$created_at ),
                          milestone_closed_at = fmt_safe_date( x[["milestone"]]$due_on ),
                          milestone_due_on = fmt_safe_date( x[["milestone"]]$due_on ),
                          n_comments = fmt_safe_int( x[["comments"]] ),
                          created_at = fmt_safe_date( x[["created_at"]] ),
                          updated_at = fmt_safe_date( x[["updated_at"]] ),
                          closed_at = fmt_safe_date( x[["closed_at"]] ),
                          author_association = fmt_safe_chr( x[["author_association"]] ),
                          body = fmt_safe_chr( x[["body"]] ),
                          repo_owner = fmt_safe_chr( x[["repo_owner"]] ),
                          repo_name = fmt_safe_chr( x[["repo_name"]] ),
                          stringsAsFactors = FALSE
              ),
            simplify = FALSE
    )

  # special handling for list columns ----
  labels_names <- sapply(res,
                         FUN = function(y)
                           vapply(y[["labels"]], FUN = function(x) x$name, FUN.VALUE = character(1) ))
  assignees_login <- sapply(res,
                            FUN = function(y)
                              vapply(y[["assignees"]], FUN = function(x) x$login, FUN.VALUE = character(1) ))

  # combine components ----
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

  mapped_elts <-
    sapply( res ,
            FUN = function(x)
              data.frame( # guaranteed fields for all events
                id = fmt_safe_int( x[["id"]] ),
                number = fmt_safe_int( x[["number"]] ),
                actor_login = fmt_safe_chr( x[["actor"]]$login ),
                event = fmt_safe_chr( x[["event"]] ),
                created_at = fmt_safe_date( x[["created_at"]] ),
                repo_owner = fmt_safe_chr( x[["repo_owner"]] ),
                repo_name = fmt_safe_chr( x[["repo_name"]] ),

                # possible fields depending on event type
                label_name = fmt_safe_chr( x[["label"]]$name ),
                milestone_title = fmt_safe_chr( x[["milestone"]]$title ),
                assignee_login = fmt_safe_chr( x[["assignee"]]$login ),
                assigner_login = fmt_safe_chr( x[["assigner"]]$login ),
                rename_from = fmt_safe_chr( x[["rename"]]$from ),
                rename_to = fmt_safe_chr( x[["rename"]]$to ),
                stringsAsFactors = FALSE
              ),
            simplify = FALSE
    )

  data <- do.call(rbind, mapped_elts)
  return(data)

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

  if (is.character(res)) {stop("Results object contains no elements to parse.")}

  mapped_elts <-
    sapply( res ,
            FUN = function(x)
              data.frame(
                url = fmt_safe_chr( x[["html_url"]] ),
                id = fmt_safe_int( x[["id"]] ),
                user_login = fmt_safe_chr( x[["user"]]$login ),
                created_at = fmt_safe_date( x[["created_at"]] ),
                updated_at = fmt_safe_date( x[["updated_at"]] ),
                author_association = fmt_safe_chr( x[["author_association"]] ),
                body = fmt_safe_chr( x[["body"]] ),
                number = fmt_safe_chr( x[["number"]] ),
                repo_owner = fmt_safe_chr( x[["repo_owner"]] ),
                repo_name = fmt_safe_chr( x[["repo_name"]] ),
                stringsAsFactors = FALSE
              ),
            simplify = FALSE
    )

  data <- do.call(rbind, mapped_elts)
  return(data)

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

  if (is.character(res)) {stop("Results object contains no elements to parse.")}

  mapped_elts <-
    sapply( res ,
            FUN = function(x)
              data.frame(
                title = fmt_safe_chr( x[["title"]]),
                number = fmt_safe_chr( x[["number"]] ),
                description = fmt_safe_chr( x[["description"]] ),
                creator_login = fmt_safe_chr( x[["creator"]]$login ),
                n_open_issues = fmt_safe_int( x[["open_issues"]] ),
                n_closed_issues = fmt_safe_int( x[["closed_issues"]] ),
                state = fmt_safe_chr( x[["state"]] ),
                url = fmt_safe_chr( x[["html_url"]] ),
                created_at = fmt_safe_date( x[["created_at"]] ),
                updated_at = fmt_safe_date( x[["updated_at"]] ),
                closed_at = fmt_safe_date( x[["closed_at"]] ),
                due_on = fmt_safe_date( x[["due_on"]] ),
                repo_owner = fmt_safe_chr( x[["repo_owner"]] ),
                repo_name = fmt_safe_chr( x[["repo_name"]] ),
                stringsAsFactors = FALSE
              ),
            simplify = FALSE
    )

  data <- do.call(rbind, mapped_elts)
  return(data)

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

  if (is.character(res)) {stop("Results object contains no elements to parse.")}

  mapped_elts <-
    sapply( res ,
            FUN = function(x)
              data.frame(
                name = fmt_safe_chr( x[["name"]]),
                url = fmt_safe_chr( x[["url"]] ),
                color = fmt_safe_chr( x[["color"]] ),
                default = fmt_safe_chr( x[["default"]] ),
                id = fmt_safe_int( x[["id"]] ),
                repo_owner = fmt_safe_chr( x[["repo_owner"]] ),
                repo_name = fmt_safe_chr( x[["repo_name"]] ),
                stringsAsFactors = FALSE
              ),
            simplify = FALSE
    )

  data <- do.call(rbind, mapped_elts)
  return(data)

}

