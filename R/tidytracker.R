#' \code{tidytracker} package
#'
#' A quick and easy wrapper for working with GitHub and other project management tools
#'
#' See the README on
#' \href{https://github.com/emilyriederer/tidytracker}{GitHub}
#'
#' @docType package
#' @name tidytracker
NULL

globalVars <- c(
  # from plan:
  "issue", "milestone", "milestone_title",
  # from viz:
  "created_at", "closed_at", "state", "id", "title", "id_label",
  "board_col", "board_pos", "number", "board_group",
  "dummy_var", "lag", "index", "base", "status",
  "Initial", "Opened", "Closed", "Final", "n",
  # from tidyverse
  ".")

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(globalVars)
