#' Read plan from YAML
#'
#' This function reads a carefully constructed YAML file representing a project plan of
#' milestones and issues. YAML is converted into a \code{tibble} data structure which can
#' be passed to \code{tidytracker::post_plan} to buidl repository infrastrucuture.
#'
#' Please see the "Building Custom Plans" vignette for more details.s
#'
#' @param filepath Filepath of YAML file. Either this or \code{chars} is required.
#' @param chars Character object containing YAML. Either this or \code{chars} in required.
#'
#' @return List containing plan compatible with \code{tidytracker::post_plan}
#' @export
#'
#' @family plan
#'
#' @examples
#' \dontrun{
#' # This example uses example file included in pkg. You should be able to run example as-is
#' file_path <- system.file("extdata", "plan_yaml.txt", package = "tidytracker", mustWork = TRUE)
#' my_plan <- read_plan_yaml("ext/my_project_plan.yaml")
#' post_plan(my_plan)
#' }

read_plan_yaml <- function(filepath = NA, chars = NA){

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
#' @inherit post_engine return params examples
#' @param plan Plan list as read with \code{tidytracker::read_plan_yaml}
#' @export
#'
#' @family plan
#' @importFrom dplyr distinct mutate pull select transmute
#'
#' @examples
#' \dontrun{
#' # This example uses example file included in pkg. You should be able to run example as-is
#' file_path <- system.file("extdata", "plan_yaml.txt", package = "tidytracker", mustWork = TRUE)
#' my_plan <- read_plan_yaml("ext/my_project_plan.yaml")
#' post_plan(my_plan)
#' }


post_plan <- function(ref, plan){

  # create milestones
  req_milestones <-
    parsed %>%
    purrr::map(~purrr::list_modify(., "issue" = NULL)) %>%
    purrr::map(., ~purrr::pmap(., ~post_milestone(ref, ...)))

  # wrangle issues
  milestone_ids <-
    purrr::modify_depth(req_milestones, .f = "number", .depth = 2) %>% unlist()
  num_issues_by_milestone <-
    parsed %>% purrr::map("issue") %>% purrr::map(length)
  milestone_nums <- purrr::map2(milestone_ids, num_isses_by_milestone, rep) %>% unlist() %>% as.integer()

  # create issues
  req_issues <-
    parsed %>%
    purrr::map("issue") %>%
    purrr::flatten %>%
    purrr::map2(milestone_nums, ~c(.x, milestone = .y)) %>%
    purrr:map(~purrr::modify_at(.,
                                .at = c("assignees", "labels"),
                                ~if(length(.) == 1){c(.,.)}else{.})) %>%
    purrr::map(~purrr::modify_at(.,
                                 .at = c('assignees', 'labels'),
                                 .f = list)) %>%
    purrr::map(~purrr::pmap(., ~post_issue(ref, ...)))

  return(issues_req)

}
