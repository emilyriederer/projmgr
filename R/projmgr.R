#' `projmgr` package
#'
#' A quick and easy wrapper for working with GitHub and other project management tools
#'
#' See the README on
#' [GitHub](https://github.com/emilyriederer/projmgr)
#'
#' @docType package
#' @name projmgr
#' @keywords internal
NULL

globalVars <- c(
  ".data" # for ggplot nse
)

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(globalVars)
