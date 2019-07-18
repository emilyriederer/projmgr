#' Post issue to GitHub repository
#'
#' @inherit post_engine params
#' @param title Issue title (required)
#' @param distinct Logical value to denote whether issues with the same title
#'     as a current open issue should be allowed
#' @export
#' @family issues
#'
#' @return Number (identifier) of posted issue
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' post_issue(myrepo,
#'   title = 'this is the issue title',
#'   body = 'this is the issue body',
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
  if (distinct) {

    open_issues <- get_issues(ref, state = 'open')

    issue_titles <-
      if (is.list(open_issues)) {
        vapply(open_issues, FUN = function(x) x[["title"]], FUN.VALUE = character(1))
      }
    else{""}

    if ( title %in% issue_titles ) { # when title not distinct
      stop(
        paste("New issue title is not distinct with current open issues. \n",
              "Please change title or set distinct = FALSE."), call. = FALSE)
    }
  }


  # check that rest of inputs are valid per github api ----
  args <- list(...)
  validate_inputs(args, allowed_vars = help_post_issue() )
  if (length(args[['labels']] == 1)) {args[["labels"]] <- I(args[["labels"]])}
  if (length(args[['assignees']] == 1)) {args[["assignees"]] <- I(args[["assignees"]])}

  # submit request ----
  api_fx <- function(...){ post_engine(api_endpoint = "/issues",
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
#' @family milestones
#'
#' @return Number (identifier) of posted milestone
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' post_milestone(myrepo,
#'   title = 'this is the title of the milestone',
#'   description = 'this is the long and detailed description',
#'   due_on = '2018-12-31T12:59:59z')
#' }

post_milestone <- function(ref, title, ...){

  # check for duplicates before attempting to post ----
  # github automatically disallows duplicate milestone titles,
  # so not an optional as in post_issues
  open_milestones <- get_milestones(ref, state = 'open')
  milestone_titles <- if (is.list(open_milestones)) {
    vapply( open_milestones, FUN = function(x) x[["title"]], FUN.VALUE = character(1))  }
  else{""}
  if (any(title == milestone_titles)) { # when title not distinct
    stop("New milestone title is not distinct with current open milestones. Please change title.",
         call. = FALSE)
  }

  # check that rest of inputs are valid per github api ----
  validate_inputs(list(...), help_post_milestone())

  res <- post_engine(api_endpoint = "/milestones",
                     ref = ref,
                     title = title,
                     ...)

  return( res[['number']] )

}
