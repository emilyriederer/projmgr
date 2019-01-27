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
#' @param text_size Text size
#'
#' @return ggplot object
#' @export
#' @seealso viz_linked
#'
#' @examples
#' \dontrun{
#' issues <- get_issues(myrepo, milestone = 1) %>% parse_issues()
#' viz_taskboard(issues, in_progress_when = is_labeled_with('in-progress'))
#' viz_taskboard(issues, in_progress_when = is_in_a_milestone())
#' }

viz_taskboard <- function(data, in_progress_when, str_wrap_width = 30, text_size = 3){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message(
      paste0("Package \"ggplot2\" is needed for this function. Please install."),
      call. = FALSE)
  }

  stopifnot(is.function(in_progress_when))

  # create classification ----
  statuses <- c("Not Started", "In Progress", "Done")
  data$board_group <- statuses[1]
  data$board_group[in_progress_when(data)] <- statuses[2]
  data$board_group[data$state == 'closed'] <- statuses[3]
  data$board_group <- factor(data$board_group, levels = statuses)

  # create helper aesthetics for size, position, text fmt ----
  data$board_pos <- stats::ave(data$number, data$board_group, FUN = seq_along)
  text_components <- mapply(
    FUN = function(x,y) strwrap(paste0("#", x, ": ", y), width = str_wrap_width),
    x = data$number, y = data$title,
    USE.NAMES = FALSE,
    SIMPLIFY = FALSE
  )
  data$taskboard_text <- vapply(
    text_components,
    FUN = function(x) paste(x, collapse = "\n"),
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  height <- max( vapply( text_components, FUN = length, FUN.VALUE = integer(1) )) * 2

  # create ggplot object of task board ----
  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank

  g <-
    ggplot2::ggplot(data, aes(x = 0, y = 0)) +
    ggplot2::geom_rect(aes(fill = board_group), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggplot2::geom_text(aes(label = taskboard_text), size = text_size) +
    ggplot2::facet_grid(board_pos ~ board_group, drop = FALSE, space = "fixed") +
    ggplot2::scale_fill_manual(values = c("Not Started" = "#F0E442",
                                 "In Progress" = "#56B4E9",
                                 "Done" = "#009E73")) +
    ggplot2::theme(
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
