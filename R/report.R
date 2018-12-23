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

  class(html) <- "knit_asis"
  return(html)

}


#' Print plan in RMarkdown friendly way
#'
#' Interprets list representation of plan, using HTML to format results in a highly readable and
#' attractive way. Resulting object returned is a character vector of HTML code with the added
#' class of \code{'knit_asis'} so that when included in an RMarkdown document knitting to HTML,
#' the results will be correctly rendered as HTML.
#'
#' @param plan List of project plan, as returned by \code{read_yaml()}
#'
#' @return Returns character string of HTML with class attribute to be correctly
#'     shown "as-is" in RMarkdown
#' @export
#' @family plans and todos
#'
#' @examples
#' \dontrun{
#' In RMarkdown:
#' ```{r}
#' my_plan <- read_yaml("my_plan.yml")
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

  class(html) <- "knit_asis"
  return(html)

}

#' Print to-do lists in RMarkdown friendly way
#'
#' Interprets list representation of to-do list, using HTML to format results in a highly readable and
#' attractive way. Resulting object returned is a character vector of HTML code with the added
#' class of \code{'knit_asis'} so that when included in an RMarkdown document knitting to HTML,
#' the results will be correctly rendered as HTML.
#'
#' @param todo List of to-do list, as returned by \code{read_yaml()}
#'
#' @return Returns character string of HTML with class attribute to be correctly
#'     shown "as-is" in RMarkdown
#' @export
#' @family plans and todos
#'
#' @examples
#' \dontrun{
#' In RMarkdown:
#' ```{r}
#' my_todo <- read_yaml("my_todo.yml")
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

  class(html) <- "knit_asis"
  return(html)

}

