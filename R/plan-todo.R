#' Read plan or to-do list from YAML
#'
#' This function reads a carefully constructed YAML file representing a project plan (of
#' milestones and issues) or a to-do list (of issues). YAML is converted into an R list
#' structure which can then be passed to \code{post_plan()} or \code{post_todo()}
#' to build infrastructure for your repository.
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
#' file_path <- system.file("extdata", "plan.yml", package = "tidytracker", mustWork = TRUE)
#' my_plan <- read_yaml(file_path)
#' post_plan(ref, my_plan)
#' }
#' \dontrun{
#' # This example uses example file included in pkg
#' # You should be able to run example as-is after creating your own repo reference
#' file_path <- system.file("extdata", "todo.yml", package = "tidytracker", mustWork = TRUE)
#' my_todo <- read_yaml(file_path)
#' post_todo(ref, my_todo)
#' }

read_yaml <- function(input){

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

  return(parsed)

}

#' Post plan (milestones + issues) to GitHub repository
#'
#' Post custom plans (i.e. create milestons and issues) based on yaml read in by
#' \code{read_yaml}. Please see the "Building Custom Plans" vignette for details.
#'
#' @inherit post_engine params
#' @inherit read_yaml examples
#' @param plan Plan list as read with \code{read_yaml()}
#' @export
#'
#' @return Dataframe with numbers (identifiers) of posted milestones and issues and issue title
#'
#' @family plans and todos
#' @importFrom dplyr distinct mutate pull select transmute

post_plan <- function(ref, plan){

  # create milestones
  milestone_num_dist <-
    purrr::map(plan, ~purrr::list_modify(., "issue" = NULL)) %>%
    purrr::map(., ~purrr::pmap(., ~post_milestone(ref, ...))) %>%
    unlist()

  # extract issue info from plan and append milestone identifiers
  milestone_num <-
    map(plan, "issue") %>%
    map(length) %>%
    map2(milestone_num_dist, ~rep(.y, .x)) %>%
    unlist() %>%
    as.integer()

  # create issues
  issues_prep <-
    purrr::map(plan, "issue") %>%
    purrr::flatten() %>%
    purrr::map2(milestone_num, ~c(.x, milestone = .y))

  # post issues
  issue_num <- purrr::map_chr(issues_prep,
                                ~do.call(function(...) post_issue(ref, ...), .x))

  return(
    data.frame(milestone_number = purrr::map_int(issues_prep, "milestone"),
                      issue_number = issue_num,
                      issue_title = purrr::map_chr(issues_prep, "title"))
    )

}

#' Post to-do list (issues) to GitHub repository
#'
#' Post custom to-do lists (i.e. issues) based on yaml read in by \code{read_yaml}.
#' Please see the "Building Custom Plans" vignette for details.
#'
#' Currently has know bug in that cannot be used to introduce new labels.
#'
#' @inherit post_engine params
#' @inherit post_issue return
#' @inherit read_yaml examples
#' @param todo To-do R list structure as read with \code{read_yaml()}
#' @export
#'
#' @family plans and todos
#' @importFrom dplyr distinct mutate pull select transmute


post_todo <- function(ref, todo){

  # create issues
  res_issues <- purrr::map(todo, ~purrr::pmap(., ~post_issue(ref, ...)))

  return( unlist(res_issues) )

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
                      package = "tidytracker",
                      mustWork = TRUE)

  cat(readLines(path), sep = '\n')

}
