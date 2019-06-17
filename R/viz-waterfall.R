#' Visualize waterfall of opened, closed, and pending items over timeframe
#'
#' Creates a four-bar waterfall diagram. Within the specified timeframe, shows initial,
#' newly opened, newly closed, and final open counts. Works with either issues or milestones,
#' as obtained by the \code{get} and \code{parse} functions.
#'
#' The following logic is used to classify issues:
#'
#' \itemize{
#'   \item Initial: \code{start < start_date and (end > start_date or state == 'open')}
#'   \item Open: \code{start >= start_date and start <= end_date}
#'   \item Closed: \code{end >= start_date and end <= end_date}
#'   \item Final: \code{start < end_date and (end > end_date or state == 'open')}
#' }
#'
#' The exact accuracy of the logic depends on filtering that has already been done to the dataset. Think carefully
#' about the population you wish to represent when \code{get}ting your data.
#'
#' @inheritParams viz_gantt
#' @param data Dataset, such as those representing issues or milestones (i.e. \code{parse_issues()} or
#'     \code{parse_milestones()}). Must have \code{state} variable and variables to specify for
#'     \code{start} and \code{end}
#' @param start_date Character string in 'YYYY-MM-DD' form for first date to be considered (inclusive)
#' @param end_date Character string in 'YYYY-MM-DD' form for last date to be considered (inclusive)
#'
#' @return ggplot object
#' @family issues
#' @export
#'
#' @examples
#' \dontrun{
#' viz_waterfall(milestones, '2017-01-01', '2017-03-31')
#' }

viz_waterfall <- function(data,
                                   start_date, end_date,
                                   start = "created_at", end = "closed_at"){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message(
      paste0("Package \"ggplot2\" is needed for this function. Please install."),
      call. = FALSE)
  }

  if(!grepl("\\d{4}-\\d{2}-\\d{2}", start_date) || !grepl("\\d{4}-\\d{2}-\\d{2}", end_date)){
    stop("Invalid date format. Please format start_date and end_date as 'YYYY-MM-DD' ",
         call. = FALSE)
  }

  initial <- sum(data[[start]] < start_date & (data[[end]] > start_date | data$state == 'open'), na.rm = TRUE)
  opened <- sum(data[[start]] >= start_date & data[[start]] <= end_date, na.rm = TRUE)
  closed <- sum(data[[end]] >= start_date & data[[start]] <= end_date, na.rm = TRUE)
  final <- sum(data[[start]] < end_date & (data[[end]] > end_date | data$state == 'open'), na.rm = TRUE)
  plot_data <- data.frame(
    status = c('Initial', 'Opened', 'Closed', 'Final'),
    n = c(initial, opened, closed, final),
    index = 1:4,
    sign = c(1,1,-1,1),
    base = c(0, initial, initial + opened, 0)
  )

  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank

  ggplot2::ggplot(plot_data,
         aes( xmin = .data$index - 0.25, xmax = .data$index + 0.25,
              ymin = .data$base, ymax = .data$base + .data$sign*.data$n,
              fill = .data$status)
  ) +
    ggplot2::geom_rect() +
    ggplot2::geom_text(
      aes( x = .data$index,
           y = (2*.data$base + .data$sign*.data$n)/2,
           label = .data$n),
      color = 'black') +
    ggplot2::scale_x_continuous(
      breaks = 1:4,
      labels = c('Initial', 'Opened', 'Closed', 'Final')) +
    ggplot2::scale_fill_manual(values =
                        c('Initial' = "#56B4E9",
                          'Opened' = "#F0E442",
                          'Closed' = "#009E73",
                          'Final' = "#56B4E9")
    ) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::labs(title = "Issue Progress Waterfall",
         subtitle = paste("From", start_date, "to", end_date)
    ) +
    ggplot2::theme(
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          strip.text.y = ggplot2::element_text(angle = 180),
          axis.text.y = element_blank())
}
