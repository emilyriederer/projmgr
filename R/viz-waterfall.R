#' Visualize waterfall of issues opened, closed, and pending over timeframe
#'
#' @inheritParams viz_gantt
#' @param issues Issues dataset as produced by \code{parse_issues()}
#' @param start_date Character string in 'YYYY-MM-DD' form for first date to be considered
#' @param end_date Character string in 'YYYY-MM-DD' form for last date to be considered
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @family issues
#' @export
#'
#' @examples
#' \dontrun{
#' viz_progress_waterfall(issues, '2017-01-01', '2017-03-31')
#' }

viz_waterfall_issues <- function(issues,
                                   start_date, end_date,
                                   start = created_at, end = closed_at){

  # initial <- sum(issues$created_at <= start_date &
  #                  (issues$closed_at >= start_date | issues$state == 'open'),
  #                na.rm = TRUE)
  # opened <- sum(issues$created_at >= start_date & issues$created_at <= end_date, na.rm = TRUE)
  # closed <- sum(issues$closed_at >= start_date & issues$closed_at <= end_date, na.rm = TRUE)
  # final <- sum(issues$created_at <= end_date & issues$state == 'open', na.rm = TRUE)
  #
  # plot_data <-
  # data.frame(
  #   index = 1:4,
  #   status = c('Initial', 'Opened', 'Closed', 'Final'),
  #   n = c(initial, opened, closed, final),
  #   sign = c(1, 1, -1, 1),
  #   base = c(0, initial, initial+opened, 0),
  #   stringsAsFactors = FALSE
  # )

  start_var <- enquo(start)
  end_var <- enquo(end)

  issues <-
    dplyr::mutate(issues, dummy_var = 1) %>%
    dplyr::group_by(dummy_var, add = TRUE)
  group_vars <- dplyr::group_vars(issues)

  plot_data <-
   dplyr::summarize(issues,
              Initial = sum(!!start_var < start_date &
                              (!!end_var > start_date | state == 'open'),
                            na.rm = TRUE),
              Opened = sum(!!start_var >= start_date & !!start_var <= end_date,
                           na.rm = TRUE),
              Closed = sum(!!end_var >= start_date & !!end_var <= end_date,
                           na.rm = TRUE),
              Final = sum(!!start_var < end_date &
                            (!!end_var > end_date | state == 'open'),
                          na.rm = TRUE)
    ) %>%
    dplyr::select(dplyr::one_of(group_vars), Initial, Opened, Closed, Final) %>%
    tidyr::gather(status, n, -dplyr::one_of(group_vars)) %>%
    dplyr::arrange(!!!syms(group_vars)) %>%
    dplyr::mutate(
      index = 1:4 ,
      sign = c(1,1,-1,1) ,
      base = ifelse(status != "Final", cumsum(dplyr::lag(n, 1, default = 0)), 0)
    )

  ggplot(plot_data,
         aes( xmin = index - 0.25, xmax = index + 0.25,
              ymin = base, ymax = base + sign*n,
              fill = status)
  ) +
    geom_rect() +
    geom_text(
      aes( x = index,
           y = (2*base + sign*n)/2,
           label = n),
      color = 'black') +
    scale_x_continuous(breaks = 1:4, labels = c('Initial', 'Opened', 'Closed', 'Final')) +
    scale_fill_manual(values =
                        c('Initial' = "#56B4E9",
                          'Opened' = "#F0E442",
                          'Closed' = "#009E73",
                          'Final' = "#56B4E9")
    ) +
    guides(fill = FALSE) +
    labs(title = "Issue Progress Waterfall",
         subtitle = paste("From", start_date, "to", end_date)
    ) +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          strip.text.y = element_text(angle = 180),
          axis.text.y = element_blank())
}
