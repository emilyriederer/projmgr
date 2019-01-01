#' Visualize Agile-style task board of issue status
#'
#' Produces three column task board of all issues categorized as "Not Started", "In Progress",
#' or "Done".
#'
#' The following logic is used to determine the status of each issue:
#' \itemize{
#'   \item{Not Started:}{ \code{closed_at} is NA and no \code{in-progress} label}
#'   \item{In Progress:}{ \code{closed_at} is NA but \code{in-progress} label exists}
#'   \item{Done:}{ \code{closed_at} is not NA}
#' }
#'
#' @inheritParams viz_gantt
#' @param issues Issues dataset as produced by \code{parse_issues()}
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
#' viz_task_board(issues)
#' }

viz_taskboard <- function(issues, str_wrap_width = 20){

  data <- issues

  # create status classification ----
  data$board_group <- "Not Started"
  data$board_group[purrr::map_lgl(data$labels_name, ~"in-progress" %in% .)] <- "In Progress"
  data$board_group[!is.na(data$closed_at)] <- "Done"
  data$board_group <- factor(data$board_group,
                             levels = c("Not Started", "In Progress", "Done"))

  # create helper aesthetics for size, position, text fmt ----
  data$board_pos <- stats::ave(data$number, data$board_group, FUN = seq_along)
  label_components <-
    purrr::map2(data$number, data$title,
                ~strwrap(paste0("#", .x, ": ", .y), width = str_wrap_width))
  data$label <- purrr::map_chr(label_components, ~paste(., collapse = "\n"))
  #data$height <- max( purrr::map_int(label_components, length) ) * 2
  height <- max( purrr::map_int(label_components, length) ) * 2

  # create ggplot object of task board ----
  g <-
    ggplot(data, aes(x = 0, y = 0)) +
    geom_rect(aes(fill = board_group), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_text(aes(label = label)) +
    geom_point(data = data.frame(y = c(-5*height, 5*height), x = c(0,0)), col = NA) +
    facet_grid(board_pos ~ board_group) +
    scale_y_continuous(limits = c(round(-1 * height / 1.75), round(height / 1.75))) +
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
      #panel.spacing.y = unit(0, "lines"),
      legend.position = 'none'
    )

  # add metadata to be used with viz_linked ----
  class(g) <- c(class(g), "taskboard")
  g[['str_wrap_width']] <- str_wrap_width

  return(g)

}
