#' Learn about optional fields for related get_ functions
#'
#' The \code{help} family of functions lists the optional query parameters available
#' for each of the related \code{get_} functions. When no optional arguments are
#' available, a blank character vector is returned.
#'
#' For more details on these parameters, please use the \code{browse_docs()} function
#' to navigate to the appropriate part of the GitHub API documentation.
#'
#' @return Character string of optional field names
#' @name help
#'
#' @examples
#' help_get_issues()
#' help_get_milestones()
NULL

#' @export
#' @name help
help_get_issues <- function(){

  c("milestone", "state", "assignee",
    "creator", "mentioned", "labels",
    "sort", "direction","since")

}

#' @export
#' @name help
help_get_issue_events <- function(){

  message("This request has no optional fields")
  return("")

}

#' @export
#' @name help
help_get_issue_comments <- function(){

  "since"

}

#' @export
#' @name help
help_get_milestones <- function(){

  c("state", "sort", "direction")

}

#' @export
#' @name help
help_get_repo_label <- function(){

  message("This request has no optional fields")
  return("")

}

#' @export
#' @name help
help_post_issue <- function(){

  c("body", "milestone","labels", "assignees")

}

#' @export
#' @name help
help_update_issue <- function(){

  c("title", "body", "milestone","labels", "assignees", "state")

}



#' @export
#' @name help
help_post_milestone <- function(){

  c("state", "description", "due_on")

}
