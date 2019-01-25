#' Read plan from YAML
#'
#' This function reads a carefully constructed YAML file representing a project plan (of
#' milestones and issues). YAML is converted into an R list structure which can then be passed
#' to \code{post_plan()} to build infrastructure for your repository.
#'
#' Please see the "Building Custom Plans" vignette for more details.
#'
#' @param input Either filepath to YAML file or character string. Assumes filepath if ends in ".yml"
#'     and assumes string otherwise.
#'
#' @return List containing plan compatible with \code{post_plan()} or \code{post_todo()}
#' @export
#'
#' @family plans and todos
#'
#' @examples
#' \dontrun{
#' # This example uses example file included in pkg
#' # You should be able to run example as-is after creating your own repo reference
#' file_path <- system.file("extdata", "plan.yml", package = "projmgr", mustWork = TRUE)
#' my_plan <- read_plan(file_path)
#' post_plan(ref, my_plan)
#' }

read_plan <- function(input){

  # check if yaml package installed
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package \"yaml\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # determine input type
  stub <- substring( trimws(input), nchar(trimws(input)) - 3)

  # check if at least one of filepath or chars in not NA
  if(stub == ".yml"){
    parsed <- yaml::yaml.load_file(input,
                                        handlers =
                                          list(expr = function(x) eval(parse(text = x))))
  }
  else{
    parsed <- yaml::yaml.load(input,
                                   handlers =
                                     list(expr = function(x) eval(parse(text = x))))
  }

  class(parsed) <- c("plan", class(parsed))
  return(parsed)

}

#' Read to-do list from YAML
#'
#' This function reads a carefully constructed YAML file representing a to-do list (of issues).
#' YAML is converted into an R list structure which can then be passed to  \code{post_todo()}
#' to build infrastructure for your repository.
#'
#' Please see the "Building Custom Plans" vignette for more details.
#'
#' @inherit read_plan return params
#' @export
#'
#' @family plans and todos
#'
#' @examples
#' \dontrun{
#' # This example uses example file included in pkg
#' # You should be able to run example as-is after creating your own repo reference
#' file_path <- system.file("extdata", "todo.yml", package = "projmgr", mustWork = TRUE)
#' my_todo <- read_todo(file_path)
#' post_todo(ref, my_todo)
#' }

read_todo <- function(input){

  # check if yaml package installed
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package \"yaml\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # determine input type
  stub <- substring( trimws(input), nchar(trimws(input)) - 3)

  # check if at least one of filepath or chars in not NA
  if(stub == ".yml"){
    parsed <- yaml::yaml.load_file(input,
                                   handlers =
                                     list(expr = function(x) eval(parse(text = x))))
  }
  else{
    parsed <- yaml::yaml.load(input,
                              handlers =
                                list(expr = function(x) eval(parse(text = x))))
  }

  class(parsed) <- c("todo", class(parsed))
  return(parsed)

}

#' Post plan (milestones + issues) to GitHub repository
#'
#' Post custom plans (i.e. create milestones and issues) based on yaml read in by
#' \code{read_plan}. Please see the "Building Custom Plans" vignette for details.
#'
#' @inherit post_engine params
#' @param distinct Logical value to denote whether issues with the same title
#'     as a current open issue should be allowed. Passed to \code{get_issues()}
#' @inherit read_plan examples
#' @param plan Plan list as read with \code{read_plan()}
#' @export
#'
#' @return Dataframe with numbers (identifiers) of posted milestones and issues and issue title
#'
#' @family plans and todos

post_plan <- function(ref, plan, distinct = TRUE){

  # create milestones ----

  ## extract milestone-related variables
  milestones <- lapply(plan,
                       FUN = function(x) x[intersect( names(x), help_post_milestone() )])
  req_milestones <- lapply(milestones,
                       FUN = function(x) do.call( function(...) post_milestone(ref, ...), x))

  # wrangle list elements ----

  ## convert key milestone info to issue-length vectors
  issues_per_milestone <- vapply(ref, FUN = length, FUN.VALUE = integer(1), USE.NAMES = FALSE )
  milestone_num <- vapply( req_milestones, FUN = function(x) x[["number"]], FUN.VALUE = integer(1) )
  milestone_title <- vapply(milestones, FUN = function(x) x[["title"]], FUN.VALUE = characer(1))
  milestone_num_rep <- rep(milestone_num, issues_per_milestone)
  milestone_title_rep <- rep(milestone_title, issues_per_milestone)

  ## wrangle milestone nums into issue data
  issues <- unlist( sapply(z, FUN = function(x) x[["issue"]]) )
  issues <- mapply( FUN = function(x,y){
                            x[["milestone"]] <- y
                            return(x)
                          }, issues, milestone_ids)

  # create issues ----
  req_issues <- lapply(issues,
                       FUN = function(x) do.call(function(...) post_issue(ref, distinct = distinct, ...), x))

  # return dataframe of identifiers ----
  results <-
    data.frame(
      milestone_number = milestone_num_rep,
      milestone_title = milestone_title_rep,
      issue_number = req_issues,
      issue_title = vapply( issues, FUN = function(x) x[["title"]], FUN.VALUE = character(1) )
    )


}

#' Post to-do list (issues) to GitHub repository
#'
#' Post custom to-do lists (i.e. issues) based on yaml read in by \code{read_todo}.
#' Please see the "Building Custom Plans" vignette for details.
#'
#' Currently has know bug in that cannot be used to introduce new labels.
#'
#' @inherit post_engine params
#' @inherit post_issue return
#' @inherit read_todo examples
#' @param todo To-do R list structure as read with \code{read_todo()}
#' @param distinct Logical value to denote whether issues with the same title
#'     as a current open issue should be allowed. Passed to \code{get_issues()}
#' @export
#'
#' @family plans and todos

post_todo <- function(ref, todo, distinct = TRUE){

  req <- vapply( todo,
                 FUN = function(y)
                   do.call( function(...) post_issue(ref, distinct = distinct, ...), y),
                 FUN.VALUE = character(1) )
  return( req )

}

#' Print YAML template to console
#'
#' Prints YAML templates for either a plan or to-do list to the console as an example
#' for developing your own custom plans and to-do lists. Inspired by similar \code{template_}
#' functions included in the \code{pkgdown} package.
#'
#' @param template One of \code{"plan"} or \code{"todo"} denoting template desired
#'
#' @return Prints template to console
#' @export
#' @family plans and todos
#'
#' @examples
#' template_yaml('plan')
#' template_yaml('todo')

template_yaml <- function(template = c('plan', 'todo')) {

  template <- match.arg(template)

  path <- system.file("extdata",
                      paste0(template, "-ex.yml"),
                      package = "projmgr",
                      mustWork = TRUE)

  cat(readLines(path), sep = '\n')

}
