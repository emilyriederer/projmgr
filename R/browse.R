


#' Internal function to support opening to GitHub documentation
#'
#' Functionality and implementation inspired by usethis
#'
#' @keywords internal

view_url <- function(..., open = interactive()) {
url <- paste(..., sep = "/")
if (open) {
  cat(paste("Opening URL", url))
  utils::browseURL(url)
} else {
  cat(paste("Open URL", url))
}
invisible(url)
}
