# These functions are used inside the report_ fx family to generate HTML

#' @keywords internal
add_encoding <- function(code) {

  # check if htmlTools package installed
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package \"htmltools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  code <- gsub("`", "", code)
  htmltools::htmlEscape(code)

}

#' @keywords internal
add_checkbox <- function(code, state){

  box <- ifelse(state == "open", " &#9744;", " &#9745;")
  paste(box, code)

}

#' @keywords internal
add_link <- function(code, link){

  paste0("<a href = '", link,"'>", code, "</a>")

}

#' @keywords internal
add_listitem_tags <- function(code){

  paste("<li>", code, "</li>")

}

#' @keywords internal
add_strong_tags <- function(code){

  paste("<strong>", code, "</strong>")

}

#' @keywords internal
add_completion_stats <- function(code, n_closed, n, show_ratio = TRUE, show_pct = TRUE){

  if (!show_ratio && !show_pct) return(code)
  ratio <- if (show_ratio) paste(n_closed, "/", n) else ""
  pct   <- if (show_pct)   paste(round( 100 * n_closed / n), "% Complete") else ""
  dash  <- if (show_ratio && show_pct) "-" else ""
  stats <- paste("<em>(",pct, dash, ratio,")</em>")
  paste(code, stats)

}

#' @keywords internal
fmt_group <- function(text, n_closed, n, show_ratio = TRUE, show_pct = TRUE) {

  res <- add_encoding(text)
  res <- add_strong_tags(res)
  res <- add_completion_stats(res, n_closed, n, show_ratio, show_pct)
  res

}

#' @keywords internal
fmt_item <- function(text, state, url = NULL) {

  res <- add_encoding(text)
  if (!is.null(url)) res <- add_link(text, url)
  res <- add_checkbox(res, state)
  res <- add_listitem_tags(res)
  res

}

#' @keywords internal
fmt_comment <- function(user_login, author_association, body, created_at, updated_at, ...){

  header <- paste("<p/><hr><strong>", user_login, "(", author_association, ") wrote at", created_at, ": </strong>")
  text <- paste("<p/><blockquote>", add_encoding(body), "</blockquote><p/>")
  bottom <- ifelse(is.na(updated_at) | created_at == updated_at,"",
                   paste("<em> This comment was last updated at", updated_at, "</em>") )

  return( paste(header, text, bottom) )

}

#' @keywords internal
fmt_issue_desc <- function(title, body, state, created_at, closed_at, user_login, url, number, ...){

  title <- paste0("<strong>Issue: #", number, ": ", add_encoding(title), "</strong>")
  meta <- paste("Created by", user_login, "on", created_at)
  status <- ifelse(is.na(closed_at), '', paste("Closed on", closed_at))
  combined_header <- paste(title, meta, status, sep = "<br>")
  body <- paste("<em>Issue Description: </em><br><blockquote>", add_encoding(body), "</blockquote>")

  return( paste(combined_header, "<p/>", body, "<p/>") )

}

#' @keywords internal
fmt_taskboard_item <- function(class, link, url, title){

  paste(
    "<div class = 'task", class, "'>",
    ifelse(link, paste0("<a href = '", url, "' target='_blank'>"), ""),
    add_encoding(title),
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
