#' Print issue-milestone progress in RMarkdown friendly way
#'
#' Interprets dataframe or tibble of issues by breaking apart milestones and listing each
#' issue title as open or closed, and uses HTML to format results in a highly readable and
#' attractive way. Resulting object returned is a character vector of HTML code with the added
#' class of \code{'knit_asis'} so that when included in an RMarkdown document knitting to HTML,
#' the results will be correctly rendered as HTML.
#'
#' @param issues Dataframe or tibble of issues and milestones, as returned by
#'     \code{get_issues()} and \code{parse_issues()}
#'
#' @return Returns character string of HTML with class attribute to be correctly
#'     shown "as-is" in RMarkdown
#' @export
#' @family issues
#'
#' @examples
#' \dontrun{
#' In RMarkdown:
#' ```{r}
#' issues <- get_issues(repo, state = 'all') %>% parse_issues()
#' report_progress(issues)
#' ```
#'}

report_progress <- function(issues){

  # prep data ----

  df <- issues[!is.na(issues$milestone_title),]
  milestone_title <- unique(df$milestone_title)
  issue_closed_count <-
    stats::aggregate(df$state,
                     by= list(df$milestone_title) ,
                     FUN= function(x) sum(x == 'closed'))$x
  issue_count <- vapply( unique(df$milestone_title),
                         FUN = function(x) sum(df$milestone_title == x),
                         FUN.VALUE = integer(1))
  issue_title <- df$title
  state <- df$state

  # write html ----
  milestone_html <- fmt_milestone(milestone_title, issue_closed_count, issue_count)
  issue_html <- fmt_issue( issue_title, state )
  issue_html_grp <- stats::aggregate(issue_html,
                                     by = list(df$milestone_title),
                                     FUN = function(x) paste(x, collapse = " "))$x
  milestone_issue_html_grp <- paste(milestone_html, "<ul style = 'list-style: none;'>", issue_html_grp, "</ul>")

  # final output ----
  html <- paste("<p/>", paste(milestone_issue_html_grp, collapse = " "), "<p/>")
  class(html) <- c("knit_asis", class(html))
  return(html)

}

#' Print plan in RMarkdown friendly way
#'
#' Interprets list representation of plan, using HTML to format results in a highly readable and
#' attractive way. Resulting object returned is a character vector of HTML code with the added
#' class of \code{'knit_asis'} so that when included in an RMarkdown document knitting to HTML,
#' the results will be correctly rendered as HTML.
#'
#' @param plan List of project plan, as returned by \code{read_plan()}
#'
#' @inherit report_progress return
#' @export
#' @family plans and todos
#'
#' @examples
#' \dontrun{
#' In RMarkdown:
#' ```{r}
#' my_plan <- read_plan("my_plan.yml")
#' report_plan(my_plan)
#' ```
#'}

report_plan <- function(plan){

  # prep data ----
  milestone_title <- vapply(plan, FUN = function(x) x[["title"]], FUN.VALUE = character(1))
  issue_count <- vapply(plan, FUN = function(x) length(x[["issue"]]), FUN.VALUE = integer(1))
  issue_title <- unlist(sapply(plan,
                               FUN = function(x) vapply(x[["issue"]], FUN = function(y) y[["title"]], FUN.VALUE = character(1))
                               )
                        )

  # write html ----
  milestone_html <- fmt_milestone(milestone_title, 0, issue_count)
  issue_html <- fmt_issue( issue_title, "open" )
  issue_html_grp <- stats::aggregate(issue_html,
                                     by = list( rep(1:length(issue_count), issue_count) ),
                                     FUN = function(x) paste(x, collapse = " "))$x
  milestone_issue_html_grp <- paste("<p>",milestone_html, "<ul style = 'list-style: none;'>", issue_html_grp, "</ul>")

  # final output ----
  html <- paste("<p/>", paste(milestone_issue_html_grp, collapse = " "), "<p/>")
  class(html) <- c("knit_asis", class(html))
  return(html)

}

#' Print to-do lists in RMarkdown friendly way
#'
#' Interprets list representation of to-do list, using HTML to format results in a highly readable and
#' attractive way. Resulting object returned is a character vector of HTML code with the added
#' class of \code{'knit_asis'} so that when included in an RMarkdown document knitting to HTML,
#' the results will be correctly rendered as HTML.
#'
#' @param todo List of to-do list, as returned by \code{read_todo()}
#'
#' @inherit report_progress return
#' @export
#' @family plans and todos
#'
#' @examples
#' \dontrun{
#' In RMarkdown:
#' ```{r}
#' my_todo <- read_todo("my_todo.yml")
#' report_todo(my_todo)
#' ```
#'}

report_todo <- function(todo){

  # prep data ----
  issue_title <- vapply(todo, FUN = function(x) x[["title"]], FUN.VALUE = character(1))

  # write html ----
  milestone_html <- fmt_milestone("To Do", 0, length(issue_title))
  issue_html <- fmt_issue( issue_title, "open" )
  issue_html_grp <- paste(issue_html, collapse = " ")
  milestone_issue_html_grp <- paste("<p>",milestone_html, "<ul style = 'list-style: none;'>", issue_html_grp, "</ul>")

  # final output ----
  html <- paste("<p/>", paste(milestone_issue_html_grp, collapse = " "), "<p/>")
  class(html) <- c("knit_asis", class(html))
  return(html)

}


#' Print issue comments in RMarkdown friendly way
#'
#' Interprets dataframe or tibble of issues by breaking apart milestones and listing each
#' issue title as open or closed, and uses HTML to format results in a highly readable and
#' attractive way. Resulting object returned is a character vector of HTML code with the added
#' class of \code{'knit_asis'} so that when included in an RMarkdown document knitting to HTML,
#' the results will be correctly rendered as HTML.
#'
#' @param comments Dataframe or tibble of comments for a single issue, as returned by \code{get_issue_comments()}
#' @param issue Optional dataframe or tibble of issues, as returned by \code{get_issues()}. If provided,
#'     output includes issue-level data such as the title, initial description, creation date, etc.
#'
#' @inherit report_progress return
#' @export
#' @family issues
#' @family comments
#'
#' @examples
#' \dontrun{
#' In RMarkdown:
#' ```{r}
#' issue <- get_issues(repo, number = 15) %>% parse_issues()
#' comments <- get_issue_comments(rep, number = 15) %>% parse_issue_comments()
#' report_discussion(issue, comments)
#' ```
#'}

report_discussion <- function(comments, issue = NA){

  # validate inputs ----
  comments_number <- unique(comments$number)
  if(length( unique(comments$number )) != 1){
    stop("Comments dataframe contains comments for more than 1 issue. Please limit data to a single issue.")
  }

  # write html ----
  html <- paste( do.call(fmt_comment, comments) , collapse = " ")

  # include issue-level data if provided ----
  if(!is.na(issue)){

    # validate inputs ----
    issue_number <- unique(issue$number)
    if(!any(comments_number == issue_number)){
      stop("Issues dataframe does not contain same issue number as comments dataframe.")
    }
    if( length(issue_number) > 1){
      issue <- issue[issue$number == comments_number, ]
    }

    # write html ----
    issue_html <- do.call(fmt_issue_desc, issue)
    html <- paste(issue_html, html)
  }

  # final output ----
  class(html) <- c("knit_asis", class(html))
  return(html)

}
