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

viz_gantt <- function(data, start = "created_at", end = "closed_at", str_wrap_width = 30){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message(
      paste0("Package \"ggplot2\" is needed for this function. Please install."),
      call. = FALSE)
  }

  # prep data to valid values ----
  plot_data <- data[!is.na(data[[start]]),]
  plot_data <- plot_data[order(data[[start]], decreasing = TRUE), ]
  plot_data$gantt_y <- factor(plot_data$title, levels = plot_data$title)
  plot_data$start_var <- plot_data[[start]]
  plot_data$end_var <-  plot_data[[end]]
  plot_data$psuedo_start_var <-
    ifelse(is.na(plot_data[[start]]),
           max(plot_data[[start]], na.rm = TRUE),
           plot_data[[start]]) %>%
    as.Date(origin = '1970-01-01')
  plot_data$psuedo_end_var <-
    ifelse(is.na(plot_data[[end]]),
           max(plot_data[[end]], na.rm = TRUE),
           plot_data[[end]]) %>%
    as.Date(origin = '1970-01-01')
  plot_data$gantt_col <- -1*as.integer(difftime(plot_data$end_var, plot_data$start_var, "days"))

  # plot data ----
  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank

  g <-
    ggplot2::ggplot(plot_data,
                    aes(x = .data$psuedo_start_var, xend = .data$psuedo_end_var,
                        y = .data$gantt_y, yend = .data$gantt_y,
                        col = .data$gantt_col)
    ) +
    ggplot2::geom_segment(size = 8) +
    ggplot2::geom_point(aes(x = .data$start_var), size = 2) +
    ggplot2::geom_point(aes(x = .data$end_var), size = 2) +
    ggplot2::labs(title = "Time to Completion") +
    ggplot2::scale_y_discrete(labels = function(x)
      vapply(x,
             FUN = function(x) paste(strwrap(x, width = str_wrap_width), collapse = "\n"),
             FUN.VALUE = character(1) ,
             USE.NAMES = FALSE)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   legend.position = "none")

  return(g)
}
