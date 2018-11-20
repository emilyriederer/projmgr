#' Visualized closed issues by open date
#'
#' Creates Gannt-style chart showing var for length of time each issue was opened.
#' Issues are colored by duration of time open.
#'
#' @param issues Issues dataset from output of \code{tidytracker::parse_issues}
#'
#' @return Gantt style \code{ggplot2} object with one row / issue
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' dplyr <- create_repo_ref('tidyverse', 'dplyr')
#' dplyr_issues_raw <- get_issues(dplyr, milestone = 1, state = "all")
#' dplyr_issues <- parse_issues(dplyr_issues_raw)
#' dplyr_issues %>%
#'  mutate_at(vars(created_at, closed_at), lubridate::as_date ) %>%
#'  viz_gantt_closed()
#' }

viz_gantt_closed <- function(issues){

  issues %>%
    dplyr::filter(state == "closed") %>%
    dplyr::mutate(id_label = factor(id, levels = id, labels = title)) %>%
    ggplot(aes(
      x = created_at, xend = closed_at,
      y = id_label, yend = id_label,
      col = -1*as.integer(difftime(closed_at, created_at, "days"))
    )) +
    geom_segment(size = 8) +
    geom_point(aes(x = created_at), size = 2) +
    geom_point(aes(x = closed_at), size = 2) +
    labs(
      title = "Closed Issues",
      x = "", y = ""
    ) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(col = FALSE)
}

#' Visualized open issues by opened/closed date
#'
#' Creates Gannt-style chart showing var for length of time each issue was opened.
#' Issues are colored by duration of time open to present.
#'
#' @inherit viz_gantt_closed params return
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{viz_gantt_open(issues)}

viz_gantt_open <- function(issues){
  issues %>%
    dplyr::filter(state == "open") %>%
    dplyr::mutate(id_label = factor(id, levels = id, labels = title),
           closed_at = as.character(Sys.Date())) %>%
    ggplot(aes(
      x = created_at, xend = closed_at,
      y = id_label, yend = id_label,
      col = -1*as.integer(difftime(closed_at, created_at, "days"))
    )) +
    geom_segment(size = 8) +
    labs(
      title = "Open Issues",
      subtitle = paste0("As of ",Sys.Date()),
      x = "", y = ""
    ) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(col = FALSE)
}

#' Visualize issues by opened/closed date
#'
#' Creates Gannt-style chart showing var for length of time each issue was opened.
#' Closed issues are colored by duration of time open. Open issues are grey.
#'
#' @inherit viz_gantt_closed params return
#' @import ggplot2
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{viz_gantt(issues)}
viz_gantt <- function(issues){

  issues_loc <-
    issues %>%
    dplyr::mutate(id_label = factor(id, levels = id, labels = title)) %>%
    dplyr::mutate(closed_at = dplyr::case_when(
      closed_at == created_at ~ as.character(as.Date(closed_at)+1),
      is.na(closed_at) ~ as.character(Sys.Date()),
      TRUE ~ closed_at
    ))

  ggplot(
    issues_loc,
    aes(
      x = as.Date(created_at), xend = as.Date(closed_at),
      y = id_label, yend = id_label,
      col = -1*as.integer(difftime(closed_at, created_at, "days"))
    )) +
    geom_segment(size = 8) +
    geom_segment(
      data = dplyr::filter(issues_loc, state == "open"),
      col = "grey",
      size = 8) +
    labs(
      title = "Issues Tracker",
      x = "", y = ""
    ) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(col = FALSE)

}
