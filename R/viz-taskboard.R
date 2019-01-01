#' Visualize Agile-style task board of item status
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
#' @inheritParams viz_gantt
#' @param data Dataset, such as those representing issues or milestones (i.e. from \code{parse_issues()}).
#'     Must have \code{state} variable.
#' @param in_progress_when Function with parameter \code{data} that returns Boolean vector. Generally, one of the
#'  taskboard helper functions. See \code{?taskboard_helpers} for details.
#'
#' @return ggplot object
#' @export
#' @seealso viz_linked
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' issues <- get_issues(myrepo, milestone = 1) %>% parse_issues()
#' viz_taskboard(issues, in_progress_when = is_labelled_with('in-progress'))
#' viz_taskboard(issues, in_progress_when = is_in_a_milestone())
#' }

viz_taskboard <- function(data, in_progress_when, str_wrap_width = 20){

  stopifnot(is.function(in_progress_when))

  # create classification ----
  statuses <- c("Not Started", "In Progress", "Done")
  data <-
    dplyr::mutate(data,
                  board_group = dplyr::case_when(
                    state == 'closed' ~ statuses[3],
                    in_progress_when(data) ~ statuses[2],
                    TRUE ~ statuses[1]
                  ))
  data$board_group <- factor(data$board_group, levels = statuses)

  # create helper aesthetics for size, position, text fmt ----
  data$board_pos <- stats::ave(data$number, data$board_group, FUN = seq_along)
  text_components <-
    purrr::map2(data$number, data$title,
                ~strwrap(paste0("#", .x, ": ", .y), width = str_wrap_width))
  data$taskboard_text <- purrr::map_chr(text_components, ~paste(., collapse = "\n"))
  height <- max( purrr::map_int(text_components, length) ) * 2

  # create ggplot object of task board ----
  g <-
    ggplot(data, aes(x = 0, y = 0)) +
    geom_rect(aes(fill = board_group), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_text(aes(label = taskboard_text)) +
    facet_grid(board_pos ~ board_group, drop = FALSE, space = "fixed") +
    scale_fill_manual(values = c("Not Started" = "#F0E442",
                                 "In Progress" = "#56B4E9",
                                 "Done" = "#009E73")) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.background.y = element_blank(),
      strip.text.y = element_blank(),
      legend.position = 'none'
    )

  # add metadata to be used with viz_linked ----
  class(g) <- c(class(g), "taskboard")
  g[['str_wrap_width']] <- str_wrap_width

  return(g)

}
