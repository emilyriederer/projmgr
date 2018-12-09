#' Visualize Gannt-style chart of closed issues
#'
#' Produces plot of all closed issues from start date to end date, based on output of
#' issues from \code{tidytracker::get_issues} and \code{tidytracker::parse_issues}. Plot
#' has one row per issues spanning from \code{start} to \code{end}.
#'
#' By default, the start date is the issue's \code{created_at} date, and the end date is
#' the issue's \code{closed_at} date. However, either of these can be altered via the
#' \code{start} and \code{end} parameters since these dates might not be reflective of the
#' true timeframe (e.g. if issues are posted well in advance of work beginning.)
#'
#' @param issues Issues dataset as produced by \code{parse_issues()}
#' @param start Unquoted variable name denoting issue start date
#' @param end Unquoted variable name denoting issue end date
#'
#' @return ggplot object
#' @export
#' @family issues
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' tidytracker_issues <- get_issues(tidytracker, state = "closed") %>% parse_issues()
#' viz_gantt_closed(tidytracker_issues)
#' }

viz_gantt_closed <- function(issues, start = created_at, end = closed_at){

  # check if curl package installed
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  start_var <- enquo(start)
  end_var <- enquo(end)

  issues %>%
    dplyr::filter(state == "closed") %>%
    dplyr::mutate(id_label = factor(id, levels = id, labels = title)) %>%
    ggplot(aes(
      x = !!start_var, xend = !!end_var,
      y = id_label, yend = id_label,
      col = -1*as.integer(difftime(!!end_var, !!start_var, "days"))
    )) +
    geom_segment(size = 8) +
    geom_point(aes(x = !!start_var), size = 2) +
    geom_point(aes(x = !!end_var), size = 2) +
    labs(
      title = "Closed Issues",
      x = "", y = ""
    ) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(col = FALSE)
}

#' Save SVG of Gantt-style chart of closed issues with links to issues
#'
#' This function creates the same plot as \code{viz_gantt_closed} then edits the
#' underlying XML so that the titles of the issues on the y-axis are linked to the
#' actual issue on GitHub
#'
#' Credit goes to this Stack Overflow answer for figuring out how to do this:
#' https://stackoverflow.com/questions/42259826/hyperlinking-text-in-a-ggplot2-visualization/42262407
#'
#' @param g ggplot2 object returned by \code{viz_gantt_closed()}
#' @param filepath Location to save resulting SVG file of ggplot2
#'
#' @return Writes SVG to file and also returns the body so that is can be easily put
#'     into an RMarkdown (with use of the \code{results = 'asis'}) chunk option
#' @export
#'
#' @family issues
#'
#' @examples
#' \dontrun{
#' # In R:
#' viz_gantt_closed_linkedfile(issues, "my_folder/my_file")
#'
#' # In RMarkdown knitting to HTML:
#' ```{r results = 'asis', echo = FALSE}
#' cat(readLines("my_folder/my_file"), sep = "\n")
#' ````
#' }

viz_gantt_closed_linkedfile <- function(g, filepath){

  if (!requireNamespace("xml2", quietly = TRUE)) {
    message(
      paste0("Package \"xml2\" is needed to edit SVG.",
             "Please install \"xml2\" or use viz_gantt_closed for the non-linked version."),
      call. = FALSE)
  }

  # save current ggplot at svg
  ggsave( paste0(filepath, ".svg"), g )

  # update svg w links
  links <- setNames(g$data$url, g$data$title)

  xml <- xml2::read_xml(paste0(filepath, ".svg"))
  xml %>%
    xml2::xml_find_all(xpath="//d1:text") %>%
    purrr::keep(xml2::xml_text(.) %in% names(links)) %>%
    xml2::xml_add_parent("a", "xlink:href" = links[xml2::xml_text(.)], target = "_blank")
  xml2::write_xml(xml, paste0(filepath, ".svg") )

}

