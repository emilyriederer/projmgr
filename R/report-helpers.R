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

#' @keywords internal
fmt_taskboard_item <- function(class, link, url, title){

  paste(
    "<div class = 'task", class, "'>",
    ifelse(link, paste0("<a href = '", url, "' target='_blank'>"), ""),
    title,
    ifelse(link, "</a>", ""),
    "</div>"
  )

}

#' @keywords internal
gen_taskboard_css <- function(rand_id, colors, hover){

  ## note that paste is used over new-lines to enhance readability while ensuring no encoding issues
  ## rand_id functions to make css unique to each indiv plot versus all taskboards in a document
    paste0("<style>",
           ".taskboard{display: grid;grid-gap: 1%;grid-template-columns: 1fr 1fr 1fr;margin-bottom: 50px;}",
           ".head{background-color: #a9a9a9;border: 1px solid #d3d3d3;text-align: center;font-weight: strong;}",
           ".task{text-align: center;padding: 2%;margin: 2%;border: 1px solid #d3d3d3;box-shadow: 2px 2px 5px grey;transition-duration:0.5s;}",
           ifelse(hover, paste0(".", rand_id, " .task:hover{margin: 0%;padding: 4%;}"), ""),
           ".", rand_id," .ns{background-color:", colors[1], ";}",
           ".", rand_id," .ip{background-color:", colors[2], ";}",
           ".", rand_id," .dn{background-color:", colors[3], ";}",
           "div.taskboard a, div.taskboard a:visited{color: black;text-decoration: none;}",
           "div.taskboard a:hover{color: black;text-decoration: underline;}",
           "div.taskboard a:active{color: white;}</style>")

}
