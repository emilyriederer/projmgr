#' Visualize Gannt-style chart of closed issues
#'
#' Produces plot of all closed issues from start date to end date, based on output of
#' issues from \code{get_issues()} and \code{parse_issues()}. Plot
#' has one row per issues spanning from \code{start} to \code{end}.
#'
#' By default, the start date is the issue's \code{created_at} date, and the end date is
#' the issue's \code{closed_at} date. However, either of these can be altered via the
#' \code{start} and \code{end} parameters since these dates might not be reflective of the
#' true timeframe (e.g. if issues are posted well in advance of work beginning.)
#'
#' @param issues Issues dataset as produced by \code{parse_issues()}
#' @param start Unquoted variable name denoting issue start date
#' @param end Unquoted variable name denoting issue end date
#' @param str_wrap_width Number of characters before text of issue title begins to wrap
#'
#' @return ggplot object
#' @export
#' @seealso viz_linked
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' issues <- get_issues(myrepo, state = "closed") %>% parse_issues()
#' viz_gantt_closed(issues)
#' }

viz_gantt_closed <- function(issues, start = created_at, end = closed_at, str_wrap_width = 30){

  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  start_var <- enquo(start)
  end_var <- enquo(end)

  g <-
  issues %>%
    dplyr::filter(state == "closed") %>%
    dplyr::mutate(id_label = factor(number, levels = number, labels = title)) %>%
    ggplot(aes(
      x = !!start_var, xend = !!end_var,
      y = id_label, yend = id_label,
      col = -1*as.integer(difftime(!!end_var, !!start_var, "days"))
    )) +
    geom_segment(size = 8) +
    geom_point(aes(x = !!start_var), size = 2) +
    geom_point(aes(x = !!end_var), size = 2) +
    labs(
      title = "Closed Issues",
      x = "", y = ""
    ) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = str_wrap_width)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(col = FALSE)

  # add metadata to be used with viz_linked

  class(g) <- c(class(g), "gantt")
  g[['str_wrap_width']] <- str_wrap_width

  return(g)
}
