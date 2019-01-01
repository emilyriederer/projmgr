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
#' @param data Dataset, such as those representing issues or milestones (i.e. \code{parse_issues()} or
#'     \code{parse_milestones()}). Must have unique \code{title} variable and variables  to specify for
#'     \code{start} and \code{end}
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
#' viz_gantt(issues)
#' }

viz_gantt <- function(data, start = created_at, end = closed_at, str_wrap_width = 30){

  start_var <- enquo(start)
  end_var <- enquo(end)

  # filter data to valid values having start and end
  plot_data <-
    data %>%
    dplyr::filter(state == "closed", !is.na(!!start_var), !is.na(!!end_var)) %>%
    dplyr::arrange(dplyr::desc(!!start_var)) %>%
    dplyr::mutate(y = factor(title, levels = title))

  g <-
    ggplot(plot_data, aes(
      x = !!start_var, xend = !!end_var,
      y = y, yend = y,
      col = -1*as.integer(difftime(!!end_var, !!start_var, "days"))
    )) +
    geom_segment(size = 8) +
    geom_point(aes(x = !!start_var), size = 2) +
    geom_point(aes(x = !!end_var), size = 2) +
    labs(title = "Time to Completion") +
    scale_y_discrete(labels = function(x)
      purrr::map(x, ~paste(strwrap(., width = str_wrap_width), collapse = "\n"))
                     ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")

  # add metadata to be used with viz_linked

  class(g) <- c("gantt", class(g))
  g[['str_wrap_width']] <- str_wrap_width

  return(g)
}
