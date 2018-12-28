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

  # define fx for format checkboxes ----
  unicode_checkbox <- function(x){ifelse(x == "open", " &#9744;", " &#9745;")}

  # prep data ----

  df <- issues %>%
    dplyr::distinct(title, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(milestone_title)) %>%
    dplyr::group_by(milestone_title) %>%
    tidyr::nest()

  # write html ----

  html <- ""
  for(i in 1:nrow(df)){

    html <- paste(html,
                  "<p>",
                  "<strong>", df$milestone_title[i], "</strong>",
                  "<i>",
                  "(",
                  round(100*sum(df$data[[i]]$state == 'closed')/nrow(df$data[[i]])),
                  "% Complete - ",
                  sum(df$data[[i]]$state == 'closed'),"/",nrow(df$data[[i]]),
                  " Issues)",
                  "</i>",
                  "<ul>"
    )

    for(j in 1:nrow(df$data[[i]])){
      html <- paste(html,
                    "<li>",
                    unicode_checkbox(df$data[[i]]$state[j]),
                    df$data[[i]]$title[j] ,
                    "</li>")
    }

    html <- paste(html, "</ul>", "</p>")
  }

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

  milestones <- purrr::map_chr(plan, "title")

  # write html ----

  html <- ""
  for(i in 1:length(milestones)){

    html <- paste(html,
                  "<p>",
                  "<strong>", milestones[i], "</strong>",
                  "<i>",
                  "( 0 % Complete - 0 /",
                  length(plan[[i]]$issue),
                  " Issues)",
                  "</i>",
                  "<ul>"
    )

    issues <- purrr::map_chr(plan[[i]]$issue, "title")

    for(j in 1:length(issues)){
      html <- paste(html,
                    "<li>",
                    " &#9744;",
                    issues[j],
                    "</li>")
    }

    html <- paste(html, "</ul>", "</p>")
  }

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

  issues <- purrr::map_chr(todo, "title")

  # write html ----

  html <- "<p> <ul>"

    for(i in 1:length(issues)){
      html <- paste(html,
                    "<li>",
                    " &#9744;",
                    issues[i],
                    "</li>")
    }

  html <- paste(html, "</ul>", "</p>")

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
#'     output includes issue-level data such as the title, intitial description, creation date, etc.
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

  # internal fx for comment fmting
  format_comment <- function(user_login, author_association, body, created_at, updated_at, ...){

    header <- paste("<p><hr><strong>", user_login, "(", author_association, ") wrote at", created_at, ": </strong>")
    text <- paste("<p/><blockquote>", body, "</blockquote><p/>")
    bottom <- ifelse(is.na(updated_at) | created_at == updated_at,
                     "", paste("<em> This comment was last updated at", updated_at, "</em>") )

    return( paste(header, text, bottom) )

  }

  # internal fx for issue fmting
  format_issue <- function(title, body, state, created_at, closed_at, user_login, url, number, ...){

    title <- paste0("<strong>Issue: #", number, ": ", title, "</strong>")
    meta <- paste("Created by", user_login, "on", created_at)
    status <- ifelse(state == 'Open', '', paste("Closed on", closed_at))
    url <- paste("<a href =' ", url, "'> Visit on GitHub </a>")
    combined_header <- paste(title, meta, status, url, sep = "<br>")

    body <- paste("<em>Issue Description: </em><br><blockquote>", body, "</blockquote>")

    return( paste(combined_header, "<p>", body, "<p>") )

  }

  # validate inputs ----
  comments_number <- unique(comments$number)

  if(length(comments_number) != 1){
    stop("Comments dataframe contains comments for more than 1 issue. Please limit data to a single issue.")
  }

  html <- ""
  for(i in 1:nrow(comments)){
    next_comment_html <- do.call(format_comment, comments[i,])
    html <- paste(html, next_comment_html)
  }

  # include issue-level data if provided ----

  if(!is.na(issue)){

    issue_number <- unique(issue$number)

    if( length(intersect(issue_number, comments_number)) == 0){
      stop("Issues dataframe does not contain same issue number as comments dataframe.")
    }
    if( length(issue_number) > 1){
      issue <- issue[issue$number == comments_number, ]
    }

    # generate html from dataframes ----
    issue_html <- do.call(format_issue, issue)
    html <- paste(issue_html, html)

  }

  class(html) <- c("knit_asis", class(html))
  return(html)

}

