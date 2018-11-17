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
#' @return \code{tibble} containing plan compatible with \code{tidytracker::post_plan}
#' @export
#'
#' @family plan
#'
#' @examples
#' \dontrun{
#' plan_tbl <- read_plan_yaml("ext/my_project_plan.yaml")
#' post_plan(plan_tbl)
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

  # convert yaml to tibble
  plan_tbl <- purrr::map_df(plan_parsed, tibble::as.tibble)

  return(plan_tbl)

}

#' Post plan (milestones + issues) to GitHub repository
#'
#' @inherit post_engine return params
#' @param plan_tbl \code{tibble} of plan as created with \code{read_plan_yaml}
#' @export
#'
#' @family plan
#'
#' @examples
#' \dontrun{
#' plan_tbl <- read_plan_yaml("ext/my_project_plan.yaml")
#' post_plan(plan_tbl)
#' }

post_plan <- function(ref, plan_tbl){

  # create milestones
  milestone_req <-
    plan_tbl %>%
    select(-issue) %>%
    distinct() %>%
    pmap(., ~post_milestone(ref, ...))

  # create issues
  issues_req <-
    plan_tbl %>%
    tidyr::nest(issue, .key = "issue") %>%
    mutate(milestone = unlist(purrr::map_dbl(milestones_req, "number"))) %>%
    select(milestone, issue) %>%
    tidyr::unnest() %>%
    transmute(issue = map2(milestone, issue, ~c(milestone = .x, .y))) %>%
    pull(issue) %>%
    purrr::map(~purrr::modify_at(., c("assignees", "labels"), ~if(length(.) == 1){c(.,.)}else{.})) %>%
    purrr::map(~purrr::modify_at(., c("assignees", "labels"), list)) %>%
    purrr::map(., ~purrr::pmap(., ~post_issue(ref, ...)))

  return(issues_req)

}
