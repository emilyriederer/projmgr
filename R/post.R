#' Post issue to GitHub repository
#'
#' @inherit post_engine params
#' @param title Issue title (required)
#' @param distinct Logical value to denote whether issues with the same title
#'     as a current open issue should be allowed
#' @export
#' @family post
#' @family issues
#'
#' @return Number (identifier) of posted issue
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' post_issue(myrepo,
#'   title = 'this is my issue's title',
#'   body = 'this is my issue's body',
#'   labels = c('priority:high', 'bug'))
#' }
#' \dontrun{
#' # can be used in conjunction with reprex pkg
#' # example assumes code for reprex is on clipboard
#' reprex::reprex(venue = "gh")
#' post_issue(myrepo,
#'             title = "something is broken",
#'             body = paste( clipr::read_clip(), collapse = "\n") )
#' }

post_issue <- function(ref, title, ..., distinct = TRUE){

  # check for unique title if desired ----
  if(distinct){

    issue_titles <- purrr::map_chr( get_issues(ref, state = 'open') , "title" )

    if(any(title == issue_titles)){ # when title not distinct
      stop(
        paste("New title is not distinct with current open issues. \n",
              "Please change title or set distinct = FALSE."),
             call. = FALSE
              )
      }
  }

  # check that rest of inputs are valid per github api ----
  args <- list(...)

  validate_inputs(args,
                  allowed_vars = c("body", "milestone",
                                   "labels", "assignees"))
  if("labels" %in% names(args)){args[["labels"]] <- I(args[["labels"]])}
  if("assignees" %in% names(args)){args[["assignees"]] <- I(args[["assignees"]])}

  # submit request ----
  api_fx <- function(...){
    tidytracker:::post_engine(api_endpoint = "/issues",
             ref = ref,
             title = title,
             ...)
  }

  res <- do.call(api_fx, args)

  return( res[['number']] )

}

#' Post milestone to GitHub repository
#'
#' @inherit post_engine params
#' @param title Milestone title (required)
#' @export
#' @family post
#' @family milestones
#'
#' @return Number (identifier) of posted milestone
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' post_milestone(myrepo,
#'   title = 'this is my milestone's title',
#'   description = 'this is my milestone's description',
#'   due_on = '2018-12-31T12:59:59z')
#' }

post_milestone <- function(ref, title, ...){

  # check for duplicates before attempting to post ----
  # github automatically disallows duplicate milestone titles,
  # so not an optional as in post_issues
    milestone_titles <- purrr::map_chr( get_milestones(ref, state = 'open') , "title" )
    if(any(title == milestone_titles)){ # when title not distinct
      stop("New title is not distinct with current open milestones. Please change title.",
             call. = FALSE)
    }

  # check that rest of inputs are valid per github api ----
  validate_inputs(list(...), allowed_vars = c("state", "description", "due_on"))

  res <- post_engine(api_endpoint = "/milestones",
              ref = ref,
              title = title,
              ...)

  return( res[['number']] )

}
