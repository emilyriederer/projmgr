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
  ".data" # for ggplot nse
)

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(globalVars)
