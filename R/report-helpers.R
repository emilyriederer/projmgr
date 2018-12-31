# These functions are used inside the report_ fx family to generate HTML

#' @keywords internal
fmt_checkbox <- function(x){ifelse(x == "open", " &#9744;", " &#9745;")}

#' @keywords internal
fmt_milestone <- function(milestone_title, n_closed, n) {
  paste(
    "<strong>", milestone_title, "</strong>",
    "<em>",
    "(", round( 100* n_closed / n), "% Complete - ",
    n_closed,"/",n," Issues)",
    "</em>"
  )
}

#' @keywords internal
fmt_issue <- function(title, state){

  paste("<li>",fmt_checkbox(state), title , "</li>")

}

#' @keywords internal
fmt_comment <- function(user_login, author_association, body, created_at, updated_at, ...){

  header <- paste("<p/><hr><strong>", user_login, "(", author_association, ") wrote at", created_at, ": </strong>")
  text <- paste("<p/><blockquote>", body, "</blockquote><p/>")
  bottom <- ifelse(is.na(updated_at) | created_at == updated_at,
                   "", paste("<em> This comment was last updated at", updated_at, "</em>") )

  return( paste(header, text, bottom) )

}

#' @keywords internal
fmt_issue_desc <- function(title, body, state, created_at, closed_at, user_login, url, number, ...){

  title <- paste0("<strong>Issue: #", number, ": ", title, "</strong>")
  meta <- paste("Created by", user_login, "on", created_at)
  status <- ifelse(state == 'Open', '', paste("Closed on", closed_at))
  url <- paste("<a href =' ", url, "'> Visit on GitHub </a>")
  combined_header <- paste(title, meta, status, url, sep = "<br>")

  body <- paste("<em>Issue Description: </em><br><blockquote>", body, "</blockquote>")

  return( paste(combined_header, "<p/>", body, "<p/>") )

}
