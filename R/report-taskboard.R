#' Report HTML-based task board of item status
#'
#' Produces three column task board showing any relevant objects (typically issues or milestones)
#' as "Not Started", "In Progress", or "Done".
#'
#' The following logic is used to determine the status of each issue:
#' \itemize{
#'   \item Done: Items with a \code{state} of "closed"
#'   \item In Progress: Custom logic via \code{in_progress_when}. See \code{?taskboard_helpers} for details.
#'   \item Not Started: Default case for items neither In Progress or Closed
#' }
#'
#' @inheritParams viz_taskboard
#' @param include_link Boolean whether or not to include links back to GitHub
#' @param hover Boolean whether or not tasks should be animated to slightly enlarge on hover
#' @param colors Character vector of hex colors for not started, in progress, and complete tasks (respectively)
#'
#' @return Returns character string of HTML/CSS with class attribute to be correctly shown "as-is" in RMarkdown
#' @export
#'
#' @examples
#' \dontrun{
#' # in RMarkdown
#' ```{r}
#' issues <- get_issues(myrepo, milestone = 1) %>% parse_issues()
#' report_taskboard(issues, in_progress_when = is_labeled_with('in-progress'))
#' ````
#' }

report_taskboard <- function(data,
                             in_progress_when,
                             include_link = FALSE,
                             hover = FALSE,
                             colors = c("#f0e442", "#56b4e9", "#009e73") ){

  stopifnot(is.function(in_progress_when))
  stopifnot(is.logical(include_link))
  stopifnot(is.logical(hover))
  stopifnot(length(colors) == 3)
  stopifnot(is.character(colors))
  stopifnot(all(grepl("^#\\w{6}$", colors)))

  # create datasets by group ----
  done <- data[data$state == "closed",]
  inprog <- data[data$state != "closed" & in_progress_when(data),]
  notstart <- data[data$state != "closed" & !in_progress_when(data), ]

  # html ----
  done_html <- mapply( function(url, title) fmt_taskboard_item("dn", include_link, url, title), done$url, done$title )
  inprog_html <- mapply( function(url, title) fmt_taskboard_item("ip", include_link, url, title), inprog$url, inprog$title )
  notstart_html <- mapply( function(url, title) fmt_taskboard_item("ns", include_link, url, title), notstart$url, notstart$title )

  # merge columns ----
  ## note that paste is used over new-lines to enhance readability while ensuring no encoding issues
  n_ns <- nrow(notstart)
  n_ip <- nrow(inprog)
  n_dn <- nrow(done)
  n_max <- max(n_ns, n_ip, n_dn, na.rm = TRUE)
  if(n_ns < n_max){notstart_html[(n_ns+1):n_max] <- "<div></div>"}
  if(n_ip < n_max){inprog_html[(n_ip+1):n_max] <- "<div></div>"}
  if(n_dn < n_max){done_html[(n_dn+1):n_max] <- "<div></div>"}
  html_tasks <- paste(paste(notstart_html, inprog_html, done_html), collapse = "")
  html <- paste0("<div class = 'taskboard'>",
                 "<div class = 'head'>Not Started</div>",
                 "<div class = 'head'>In Progress</div>",
                 "<div class = 'head'>Done</div>",
                 html_tasks,
                 "</div>")

  # add css ----
  ## note that paste is used over new-lines to enhance readability while ensuring no encoding issues
  rand_id <- paste(sample(letters, 10, replace = TRUE), collapse = "")
  css <- gen_taskboard_css(rand_id, colors, hover)

  # aggregate code components ----
  code <- paste("<div class = '", rand_id, "'>",css, html, "</div>")
  class(code) <- c("knit_asis", class(code))
  return(code)

}
