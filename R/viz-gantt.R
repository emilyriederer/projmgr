#' Visualize Gantt-style chart of planned or actual time to completion
#'
#' Produces plot with one vertical bar from the specified \code{start} variable's value to the
#' \code{end} variable's value. Common uses would be to visualize time-to-completion for issues
#' gotten by (\code{get_issues} and \code{parse_issues}) or milestones. Bars are colored by
#' duration with longer bars as a darker shade of blue, and start/completion is denoted by
#' points at the ends of the bars.
#'
#' By default, the start date is the issue's \code{created_at} date, and the end date is
#' the issue's \code{closed_at} date. However, either of these can be altered via the
#' \code{start} and \code{end} parameters since these dates might not be reflective of the
#' true timeframe (e.g. if issues are posted well in advance of work beginning.)
#'
#' Unfinished tasks (where the value of the \code{end} variable is \code{NA}) are colored grey and
#' do not have dots on their bars. Unstarted tasks are dropped because user intent is ambiguous
#' in that case.
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
#' @examples
#' \dontrun{
#' issues <- get_issues(myrepo, state = "closed") %>% parse_issues()
#' viz_gantt(issues)
#' }

viz_gantt <- function(data, start = created_at, end = closed_at, str_wrap_width = 30){

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    message(
      paste0("Package \"dplyr\" is needed for this function. Please install \"dplyr\" to continue."),
      call. = FALSE)
  }

  if (!requireNamespace("rlang", quietly = TRUE)) {
    message(
      paste0("Package \"rlang\" is needed for this function. Please install \"rlang\" to continue."),
      call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message(
      paste0("Package \"ggplot2\" is needed for this function. Please install."),
      call. = FALSE)
  }

  start_var <- enquo(start)
  end_var <- enquo(end)

  # filter data to valid values having start and end
  plot_data <-
    data %>%
    dplyr::filter(!is.na(!!start_var)) %>%
    dplyr::arrange(dplyr::desc(!!start_var)) %>%
    dplyr::mutate(gantt_y = factor(title, levels = title),
                  start_var = !!start_var,
                  end_var = !!end_var,
                  psuedo_start_var = dplyr::if_else(is.na(start_var), min(start_var, na.rm = TRUE), start_var),
                  psuedo_end_var = dplyr::if_else(is.na(end_var), max(end_var, na.rm = TRUE), end_var)
    )

  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank

  g <-
    ggplot2::ggplot(plot_data,
           aes(x = psuedo_start_var, xend = psuedo_end_var, y = gantt_y, yend = gantt_y,
               col = -1*as.integer(difftime(!!end_var, !!start_var, "days"))
           )) +
    ggplot2::geom_segment(size = 8) +
    ggplot2::geom_point(aes(x = start_var), size = 2) +
    ggplot2::geom_point(aes(x = end_var), size = 2) +
    ggplot2::labs(title = "Time to Completion") +
    ggplot2::scale_y_discrete(labels = function(x)
      purrr::map(x, ~paste(strwrap(., width = str_wrap_width), collapse = "\n"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")

  # add metadata to be used with viz_linked
  class(g) <- c("gantt", class(g))
  g[['str_wrap_width']] <- str_wrap_width

  return(g)
}
