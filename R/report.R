#' Print issue-milestone progress in RMarkdown friendly way
#'
#' Interprets dataframe or tibble of items (e.g. issues) by breaking apart groups (e.g. milestones), listing each
#' item title as open or closed, and using HTML to format results in a highly readable and
#' attractive way. Resulting object returned is a character vector of HTML code with the added
#' class of \code{'knit_asis'} so that when included in an RMarkdown document knitting to HTML,
#' the results will be correctly rendered as HTML.
#'
#' The resulting HTML unordered list (\code{<ul>}) is tagged with class 'report_progress' for custom CSS styling.
#'
#' Items without a related group are put into an "Ungrouped" category. Filter these out before using this function if
#' you wish to only show items that are in a group.
#'
#' @param issues Dataframe or tibble of issues and milestones, as returned by \code{get_issues()} and \code{parse_issues()}
#' @param group_var Character string variable name by which to group issues. Defaults to \code{"milestone_title"}
#' @param link_url Boolean. Whether or not to provide link to each item, as provided by \code{url} column in dataset
#' @param show_ratio Boolean. Whether or not to report (# Closed Items / # Total Items) for each group as a ratio
#' @param show_pct Boolean. Whether or not to report (# Closed Items / # Total Items) for each group as a percent
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

report_progress <- function(issues, group_var = "milestone_title",
                            link_url = TRUE, show_ratio = TRUE, show_pct = TRUE){

  # prep data ----
  df <- issues
  df[[group_var]][is.na(df[[group_var]])] <- "Ungrouped"
  group_vals <- df[[group_var]]
  group_title <- unique(group_vals)
  item_closed_count <- vapply(group_title,
                              function(x) sum(group_vals == x & df$state == 'closed'),
                              integer(1) )
  item_count <- vapply( group_title ,
                        FUN = function(x) sum(group_vals == x),
                        FUN.VALUE = integer(1))
  item_title <- df$title
  state <- df$state

  # write html ----
  title_html <- fmt_group(group_title, item_closed_count, item_count, show_ratio, show_pct)
  item_html <- fmt_item(item_title, state, if (link_url) df$url else NULL)
  item_html_grp <- vapply(group_title,
                          FUN = function(x) paste(item_html[group_vals == x], collapse = " "),
                          FUN.VALUE = character(1))

  # combine across groups ----
  html <- paste(
    title_html,
    "<ul  class = 'report_progress' style = 'list-style: none;'>",
    item_html_grp,
    "</ul>"
  )
  html_grp <- paste(
    title_html,
    "<ul  class = 'report_progress' style = 'list-style: none;'>",
    item_html_grp,
    "</ul>")

  # final output ----
  html <- paste("<p/>", paste(html_grp, collapse = " "), "<p/>")
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
#' The resulting HTML unordered list (<ul>) is tagged with class 'report_plan' for custom CSS styling.
#'
#' @param plan List of project plan, as returned by \code{read_plan()}
#' @inheritParams report_progress
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

report_plan <- function(plan, show_ratio = TRUE){

  # prep data ----
  milestone_title <- vapply(plan, FUN = function(x) x[["title"]], FUN.VALUE = character(1))
  issue_count <- vapply(plan, FUN = function(x) length(x[["issue"]]), FUN.VALUE = integer(1))

  # write html ----
  title_html <- fmt_group(milestone_title, 0, sum(issue_count), show_ratio = show_ratio, show_pct = FALSE)
  issue_html_grp <- vapply(plan,
                           FUN = function(x)
                             paste( vapply(x[["issue"]],
                                           FUN = function(y) fmt_item( y[["title"]], "open" ),
                                           FUN.VALUE = character(1)) ,
                                    collapse = " "),
                           FUN.VALUE = character(1))
  milestone_issue_html_grp <- paste("<p/>",
                                    title_html,
                                    "<ul class = 'report_plan' style = 'list-style: none;'>",
                                    issue_html_grp,
                                    "</ul>")

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
#' The resulting HTML unordered list (\code{<ul>}) is tagged with class 'report_todo' for custom CSS styling.
#'
#' @param todo List of to-do list, as returned by \code{read_todo()}
#' @inheritParams report_progress
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

report_todo <- function(todo, show_ratio = TRUE){

  # prep data ----
  issue_title <- vapply(todo, FUN = function(x) x[["title"]], FUN.VALUE = character(1))

  # write html ----
  milestone_html <- fmt_group("To Do", 0, length(issue_title), show_ratio = show_ratio, show_pct = FALSE)
  issue_html <- fmt_item( issue_title, state = "open" )
  issue_html_grp <- paste(issue_html, collapse = " ")
  milestone_issue_html_grp <- paste("<p>",milestone_html, "<ul class = 'report_todo' style = 'list-style: none;'>", issue_html_grp, "</ul>")

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
#' HTML output is wrapped in a <div> of class 'report_disccusion' for custom CSS styling.
#'
#' @param comments Dataframe or tibble of comments for a single issue, as returned by \code{get_issue_comments()}
#' @param issue Optional dataframe or tibble of issues, as returned by \code{get_issues()}. If provided,
#'     output includes issue-level data such as the title, initial description, creation date, etc.
#' @inheritParams report_progress
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

report_discussion <- function(comments, issue = NA, link_url = TRUE){

  # validate inputs ----
  comments_number <- unique(comments$number)
  if (length( unique(comments$number )) != 1) {
    stop("Comments dataframe contains comments for more than 1 issue. Please limit data to a single issue.")
  }

  # write html ----
  html <- paste( do.call(fmt_comment, comments) , collapse = " ")

  # include issue-level data if provided ----
  if (!is.na(issue)) {

    # validate inputs ----
    issue_number <- unique(issue$number)
    if (!any(comments_number == issue_number)) {
      stop("Issues dataframe does not contain same issue number as comments dataframe.")
    }
    if ( length(issue_number) > 1) {
      issue <- issue[issue$number == comments_number, ]
    }

    # write html ----
    issue_html <- do.call(fmt_issue_desc, issue)
    issue_link <- if (link_url) add_link("Visit on GitHub", issue$url) else ""
    html <-
      paste(
        "<div class = 'report_discussion'>",
        issue_html,
        issue_link,
        html,
        "</div>")
  }

  # final output ----
  class(html) <- c("knit_asis", class(html))
  return(html)

}
