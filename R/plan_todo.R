#' Read plan or to-do list from YAML
#'
#' This function reads a carefully constructed YAML file representing a project plan (of
#' milestones and issues) or a to-do list (of issues). YAML is converted into an R list
#' structure which can then be passed to \code{tidytracker::post_plan} or \code{tidytracker::post_todo}
#' to build infrastructure for your repository.
#'
#' Please see the "Building Custom Plans" vignette for more details.
#'
#' @param filepath Filepath of YAML file. Either this or \code{chars} is required.
#' @param chars Character object containing YAML. Either this or \code{chars} in required.
#'
#' @return List containing plan compatible with \code{tidytracker::post_plan} or |code{tidytracker::post_todo}
#' @export
#'
#' @family plans and todos
#'
#' @examples
#' \dontrun{
#' # This example uses example file included in pkg
#' # You should be able to run example as-is after creating your own repo reference
#' file_path <- system.file("extdata", "plan_yaml.txt", package = "tidytracker", mustWork = TRUE)
#' my_plan <- read_plan_todo_yaml(file_path)
#' post_plan(ref, my_plan)
#' }
#' \dontrun{
#' # This example uses example file included in pkg
#' # You should be able to run example as-is after creating your own repo reference
#' file_path <- system.file("extdata", "plan_yaml.txt", package = "tidytracker", mustWork = TRUE)
#' my_todo <- read_plan_todo_yaml(file_path)
#' post_todo(ref, my_todo)
#' }

read_plan_todo_yaml <- function(filepath = NA, chars = NA){

  # check if yaml package installed
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package \"yaml\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # check if at least one of filepath or chars in not NA
  if(is.na(filepath) & is.na(chars)){
    stop("Please provide either a filepath or a character vector of YAML.",
         call. = FALSE)
  }
  else if(!is.na(filepath) & !is.na(chars)){
    message("Both filepath and chars provided. Default is to use filepath.")
    plan_parsed <- yaml::yaml.load_file(filepath,
                                        handlers = list(expr = function(x) eval(parse(text = x))))
  }
  else if(!is.na(chars)){
    plan_parsed <- yaml::yaml.load(chars,
                                   handlers = list(expr = function(x) eval(parse(text = x))))
  }
  else{ # => !is.na(filepath)
    plan_parsed <- yaml::yaml.load_file(filepath,
                                        handlers = list(expr = function(x) eval(parse(text = x))))
  }

  return(plan_parsed)

}

#' Post plan (milestones + issues) to GitHub repository
#'
#' @inherit post_engine return params
#' @inherit read_plan_todo_yaml examples
#' @param plan Plan list as read with \code{tidytracker::read_plan_yaml}
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
  milestone_nums <- purrr::map2(milestone_ids, num_isses_by_milestone, rep) %>% unlist() %>% as.integer()

  # create issues
  req_issues <-
    plan %>%
    purrr::map("issue") %>%
    purrr::flatten %>%
    purrr::map2(milestone_nums, ~c(.x, milestone = .y)) %>%
    purrr::map(~purrr::modify_at(.,
                                .at = c("assignees", "labels"),
                                ~if(length(.) == 1){c(.,.)}else{.})) %>%
    purrr::map(~purrr::modify_at(.,
                                 .at = c('assignees', 'labels'),
                                 .f = list)) %>%
    purrr::map(~purrr::pmap(., ~post_issue(ref, ...)))

  return(issues_req)

}

#' Post plan (milestones + issues) to GitHub repository
#'
#' @inherit post_engine return params
#' @inherit read_plan_todo_yaml examples
#' @param todo To-do R list structure as read with \code{tidytracker::read_plan_yaml}
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

  return(issues_req)

}
