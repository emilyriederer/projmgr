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
#' Note that this function respects \code{dplyr::group_by()}. If grouped data is passed, it will preserve
#' the grouping and separate plots can be made with facetting.
#'
#' @inheritParams viz_gantt
#' @param data Dataset, such as those representing issues or milestones (i.e. \code{parse_issues()} or
#'     \code{parse_milestones()}). Must have \code{state} variable and variables to specify for
#'     \code{start} and \code{end}
#' @param start_date Character string in 'YYYY-MM-DD' form for first date to be considered (inclusive)
#' @param end_date Character string in 'YYYY-MM-DD' form for last date to be considered (inclusive)
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @family issues
#' @export
#'
#' @examples
#' \dontrun{
#' viz_waterfall(milestones, '2017-01-01', '2017-03-31')
#' }
#' \dontrun{
#' issues %>%
#' dplyr::group_by(milestone) %>%
#' viz_waterfall('2017-01-01', '2017-03-31') +
#' facet_grid(milestone ~ .)
#' }

viz_waterfall <- function(data,
                                   start_date, end_date,
                                   start = created_at, end = closed_at){

  if(!grepl("\\d{4}-\\d{2}-\\d{2}", start_date) || !grepl("\\d{4}-\\d{2}-\\d{2}", end_date)){
    stop("Invalid date format. Please format start_date and end_date as 'YYYY-MM-DD' ",
         call. = FALSE)
  }

  start_var <- enquo(start)
  end_var <- enquo(end)

  prep_data <-
    dplyr::mutate(data, dummy_var = 1) %>%
    dplyr::group_by(dummy_var, add = TRUE)
  group_vars <- dplyr::group_vars(data)

  plot_data <-
   dplyr::summarize(prep_data,
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
