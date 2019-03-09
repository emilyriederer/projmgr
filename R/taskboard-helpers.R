#' Tag "in-progress" items for taskboard visualization
#'
#' The \code{viz_taskboard()} function creates a three-column layout of entities that are
#' not started, in progress, or done. Objects are classified as done when they have a
#' \code{state} of "closed". Object are classified as "To-Do" when they are neither "Closed"
#' or "In Progress". However, what constistutes "In Progress" is user and project dependent.
#' Thus, these functions let users specify what they mean.
#'
#' General options:
#'
#' \itemize{
#'   \item is_created_before: Was created before a user-specified data (as "YYYY-MM-DD" character string)
#' }
#'
#' Issue-specific options:
#'
#' \itemize{
#'   \item is_labeled_with: User-specified label (as character string) exists
#'   \item is_assigned: Has been assigned to anyone
#'   \item is_assigned_to: Has been assigned to specific user-specified login (as character string)
#'   \item is_in_a_milestone: Has been put into any milestone
#'   \item is_in_milestone: Has been put into a specific milestone
#' }
#'
#' Milestone-specific options:
#'
#' \itemize{
#'   \item is_part_closed: Has any of its issues closed
#'   \item is_due: Has a due date
#'   \item is_due_before: Has a due data by or before a user-specified date (as "YYYY-MM-DD" character string)
#' }
#'
#'
#' @return Function to be passed as \code{in_progress_when} argument in \code{viz_taskboard()}
#' @name taskboard_helpers
#'
#' @examples
#' \dontrun{
#' viz_taskboard(issues, in_progress_when = is_labeled_with('in-progress'))
#' viz_taskboard(milestones, in_progress_when = is_created_before('2018-12-31'))
#' viz_taskboard(issues, in_progress_when = is_in_milestone())
#' }
NULL

#' @export
#' @name taskboard_helpers
is_labeled <- function(){
  function(data){
    stopifnot("labels_name" %in% names(data))
    vapply(data$labels_name, FUN = function(x) length(x) > 0, FUN.VALUE = logical(1))
  }
}

#' @export
#' @param label Label name as character
#' @name taskboard_helpers
is_labeled_with <- function(label){
  function(data){
    stopifnot("labels_name" %in% names(data))
    vapply(data$labels_name, FUN = function(x) label %in% x, FUN.VALUE = logical(1))
  }
}

#' @export
#' @name taskboard_helpers
is_assigned <- function(){
  function(data){
    stopifnot("assignees_login" %in% names(data))
    vapply(data$assignees_login, FUN = function(x) length(x) > 0, FUN.VALUE = logical(1))
  }
}

#' @export
#' @param login User login as character
#' @name taskboard_helpers
is_assigned_to <- function(login){
  function(data){
    stopifnot("assignees_login" %in% names(data))
    vapply(data$assignees_login, FUN = function(x) login %in% x, FUN.VALUE = logical(1))
  }
}

#' @export
#' @name taskboard_helpers
is_in_a_milestone <- function(){
  function(data){
    stopifnot("milestone_number" %in% names(data))
    !is.na(data$milestone_number)
  }
}

#' @export
#' @param number Milestone number
#' @name taskboard_helpers
is_in_milestone <- function(number){
  function(data){
    stopifnot("milestone_number" %in% names(data))
    !is.na(data$milestone_number) & data$milestone_number == number
  }
}

#' @export
#' @param created_date Date as character in "YYYY-MM-DD" format
#' @name taskboard_helpers
is_created_before <- function(created_date){
  function(data){
    stopifnot("created_at" %in% names(data))
    data$created_at < created_date
  }
}

#' @export
#' @name taskboard_helpers
is_part_closed <- function(){
  function(data){
    stopifnot("n_closed_issues" %in% names(data))
    !is.na(data$n_closed_issues) & data$n_closed_issues > 0
  }
}

#' @export
#' @name taskboard_helpers
is_due <- function(){
  function(data){
    stopifnot("due_on" %in% names(data))
    !is.na(data$due_on)
  }
}

#' @export
#' @param due_date Date as character in "YYYY-MM-DD" format
#' @name taskboard_helpers
is_due_before <- function(due_date){
  function(data){
    stopifnot("due_on" %in% names(data))
    !is.na(data$due_on) & data$due_on < due_date
  }
}
