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
  stub <- substring( trimws(input), nchar(trimws(input)) - 4)

  # check if at least one of filepath or chars in not NA
  if(stub == ".yml"){
    plan_parsed <- yaml::yaml.load_file(input,
                                        handlers =
                                          list(expr = function(x) eval(parse(text = x))))
  }
  else{
    plan_parsed <- yaml::yaml.load(input,
                                   handlers =
                                     list(expr = function(x) eval(parse(text = x))))
  }

  return(plan_parsed)

}

#' Post plan (milestones + issues) to GitHub repository
#'
#' Post custom plans (i.e. create milestons and issues) based on yaml read in by
#' \code{read_yaml}. Please see the "Building Custom Plans" vignette for details.
#'
#' @inherit post_engine return params
#' @inherit read_yaml examples
#' @param plan Plan list as read with \code{read_yaml()}
#' @export
#'
#' @family plans and todos
#' @importFrom dplyr distinct mutate pull select transmute

post_plan <- function(ref, plan){

  # create milestones
  req_milestones <-
    plan %>%
    purrr::map(~purrr::list_modify(., "issue" = NULL)) %>%
    purrr::map(., ~purrr::pmap(., ~post_milestone(ref, ...)))

  # wrangle issues
  milestone_ids <-
    purrr::modify_depth(req_milestones, .f = "number", .depth = 2) %>% unlist()

  num_issues_by_milestone <-
    plan %>% purrr::map("issue") %>% purrr::map(length)

  milestone_nums <-
    purrr::map2(milestone_ids, num_issues_by_milestone, rep) %>%
    unlist() %>%
    as.integer()

  # create issues
  req_issues <-
    plan %>%
    purrr::map("issue") %>%
    purrr::flatten() %>%
    purrr::map2(milestone_nums, ~c(.x, milestone = .y)) %>%
    purrr::map(~purrr::modify_at(.,
                                .at = c("assignees", "labels"),
                                ~if(length(.) == 1){c(.,.)}else{.})) %>%
    purrr::map(~purrr::modify_at(.,
                                 .at = c('assignees', 'labels'),
                                 .f = list)) %>%
    purrr::map(~purrr::pmap(., ~post_issue(ref, ...)))

  return(req_issues)

}

#' Post to-do list (issues) to GitHub repository
#'
#' Post custom to-do lists (i.e. issues) based on yaml read in by \code{read_yaml}.
#' Please see the "Building Custom Plans" vignette for details.
#'
#' Currently has know bug in that cannot be used to introduce new labels.
#'
#' @inherit post_engine return params
#' @inherit read_yaml examples
#' @param todo To-do R list structure as read with \code{read_yaml()}
#' @export
#'
#' @family plans and todos
#' @importFrom dplyr distinct mutate pull select transmute


post_todo <- function(ref, todo){

  # create issues
  req_issues <-
    todo %>%
    purrr::map(~purrr::modify_at(.,
                                .at = c("assignees", "labels"),
                                ~if(length(.) == 1){c(.,.)}else{.})) %>%
    purrr::map(~purrr::modify_at(.,
                                 .at = c('assignees', 'labels'),
                                 .f = list)) %>%
    purrr::map(~purrr::pmap(., ~post_issue(ref, ...)))

  return(req_issues)

}

#' Print YAML template to console
#'
#' Prints YAML templates for either a plan or to-do list to the console as an example
#' for developing your own custom plans and to-do lists.
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
