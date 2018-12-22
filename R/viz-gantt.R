#' Visualize Gannt-style chart of closed issues
#'
#' Produces plot of all closed issues from start date to end date, based on output of
#' issues from \code{get_issues()} and \code{parse_issues()}. Plot
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
#' issues <- get_issues(myrepo, state = "closed") %>% parse_issues()
#' viz_gantt_closed(issues)
#' }

viz_gantt_closed <- function(issues, start = created_at, end = closed_at){

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
#' @param filepath Location to save resulting SVG file of ggplot2, if desired. Leave blank for
#'     function to output message precisely as needed to render in HTML RMarkdown with chunk
#'     option \code{results = 'asis'}
#'
#' @return SVG version of ggplot2 object with links to relevant GitHub issues. Either writes output
#'     to file or to console (to be captured in RMarkdown) depending on existence of \code{filepath} argument
#' @export
#'
#' @family issues
#'
#' @examples
#' \dontrun{
#' # In R, to save to file:
#' viz_gantt_closed_links(issues, "my_folder/my_file.svg")
#'
#' # In RMarkdown chunk, to print as output:
#' ```{r results = 'asis', echo = FALSE}
#' g <- viz_gantt_closed(issues)
#' viz_gantt_closed_links(g)
#' ````
#' }

viz_gantt_closed_links <- function(g, filepath){

  if (!requireNamespace("xml2", quietly = TRUE)) {
    message(
      paste0("Package \"xml2\" is needed to edit SVG.",
             "Please install \"xml2\" or use viz_gantt_closed for the non-linked version."),
      call. = FALSE)
  }

  # save current ggplot at svg
  tf <- tempfile(fileext = ".svg")
  ggsave(tf , g )

  # update svg w links
  links <- tibble::tibble(
    url = g$data$url,
    name =
      g$data$title %>%
      stringr::str_wrap(width = 30) %>%
      stringr::str_split("\\n")
  ) %>%
    tidyr::unnest() %>%
    {stats::setNames(.$url, .$name)}

  xml <- xml2::read_xml(tf)
  xml %>%
    xml2::xml_find_all(xpath="//d1:text") %>%
    purrr::keep(xml2::xml_text(.) %in% names(links)) %>%
    xml2::xml_add_parent("a", "xlink:href" = links[xml2::xml_text(.)], target = "_blank")

  if(missing(filepath)){
    xml2::write_xml(xml, tf)
    cat( readLines(tf), sep = "\n" )
  }
  else{ xml2::write_xml(xml, filepath ) }

  # clean up environment
  unlink(tf)

}

