#' Visualize Agile-style task board of issue status
#'
#' Produces three column task board of all issues categorized as "Not Started", "In Progress",
#' or "Done". When interativitity is enabled (by changing the  \code{.interactive} parameter
#' from its default of \code{FALSE}), a tooltip is enabled to show the \code{created_at} date
#' for each issue.
#'
#' The following logic is used to determine the status of each issue:
#' \itemize{
#'   \item{Not Started:}{ \code{closed_at} is NA and no \code{in-progress} label}
#'   \item{In Progress:}{ \code{closed_at} is NA but \code{in-progress} label exists}
#'   \item{Done:}{ \code{closed_at} is not NA}
#' }
#'
#' @inheritParams viz_gantt_closed
#' @param .interactive If true, uses \code{ggplotly} to add interative tooltip with issue created date
#'
#' @return ggplot object
#' @export
#' @family issues
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' tidytracker_issues <- get_issues(tidytracker, milestone = 1) %>% parse_issues()
#' viz_task_board(tidytracker_issues)
#' }

viz_task_board <- function(issues, .interactive = FALSE){

  data <- issues

  # create board headers ----
  data$board_group <- "Not Started"
  data$board_group[purrr::map_lgl(data$label, ~"in-progress" %in% .)] <- "In Progress"
  data$board_group[!is.na(data$closed_at)] <- "Done"
  data$board_group <- factor(data$board_group,
                             levels = c("Not Started", "In Progress", "Done"))

  # create position and color aesthetics ----
  data$board_pos <- -1 * stats::ave(data$id, data$board_group, FUN = seq_along)
  data$board_col <- sample(letters[1:20], nrow(data), replace = TRUE)


  # create ggplot object of task board ----
  g <-
    ggplot(data,
           aes(x = 1,
               y = board_pos,
               Created = created_at
           )) +
    geom_tile(aes(fill = board_col), width = 0.9, height = 0.9, size = 2) +
    geom_text(aes(label =
                    paste0("#", id, ": ", title) %>%
                    stringr::str_wrap(width = 20)
    )) +
    facet_grid(. ~ board_group, drop = FALSE) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          strip.text.x = element_text(size = 12),
          legend.position = 'none')

  if(!.interactive){
    return(g)
  }
  else{

    if (!requireNamespace("plotly", quietly = TRUE)) {
      message(
        paste0("Package \"plotly\" is needed to enable interactivity.",
               "Please install \"plotly\" and rerun if you want tooltip."),
        call. = FALSE)
    }

    return(plotly::ggplotly(g, tooltip = "Created"))

  }

}
