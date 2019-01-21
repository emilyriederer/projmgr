#' \code{projmgr} package
#'
#' A quick and easy wrapper for working with GitHub and other project management tools
#'
#' See the README on
#' \href{https://github.com/emilyriederer/projmgr}{GitHub}
#'
#' @docType package
#' @name projmgr
NULL

globalVars <- c(
  # from plan:
  "issue", "milestone", "milestone_title",
  # from viz:
  "created_at", "closed_at", "state", "id", "title", "id_label",
  "board_col", "board_pos", "number", "board_group", "label",
  "dummy_var", "lag", "index", "base", "status", "gantt_y", "gantt_col",
  "psuedo_start_var", "psuedo_end_var", "end_var", "start_var", "taskboard_text",
  "Initial", "Opened", "Closed", "Final", "n", "assignees_login",
  "label", "login", "number", "date",
  # from tidyverse
  ".")

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(globalVars)
