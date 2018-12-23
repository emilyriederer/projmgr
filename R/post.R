#' Post issue to GitHub repository
#'
#' @inherit post_engine return params
#' @param title Issue title (required)
#' @param distinct Logical value to denote whether issues with the same title
#'     as a current open issue should be allowed
#' @export
#' @family post
#' @family issues
#'
#' @return Character string. "Duplicate" if not posted due to title conflict or "Success" if successfully posted.
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

  if(distinct){

    issue_titles <- purrr::map_chr( get_issues(ref, state = 'open') , "title" )

    if(any(title == issue_titles)){ # when title not distinct
          return("Duplicate")
      }
  }

  validate_inputs(list(...),
                  allowed_vars = c("body", "milestone",
                                   "labels", "assignees"))

  post_engine(api_endpoint = "/issues",
             ref = ref,
             title = title,
             ...)

  return("Success")

}

#' Post milestone to GitHub repository
#'
#' @inherit post_engine return params
#' @param title Milestone title (required)
#' @export
#' @family post
#' @family milestones
#'
#' @return Character string. "Duplicate" if not posted due to title conflict or "Success" if successfully posted.
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

  # check for duplicates before attempting to post
  # github automatically disallows duplicate milestone titles,
  # so not an optional as in post_issues
    milestone_titles <- purrr::map_chr( get_milestones(ref, state = 'open') , "title" )
    if(any(title == milestone_titles)){ # when title not distinct
      return("Duplicate")
    }

  validate_inputs(list(...),
                  allowed_vars = c("state", "description", "due_on"))

  post_engine(api_endpoint = "/milestones",
              ref = ref,
              title = title,
              ...)

  return("Success")

}
