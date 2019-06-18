#' Add GitHub links to select visualizations
#'
#' This function alters the internal representation of a plot to include links back to the actual
#' GitHub issue. This is currently implemented for \code{viz_taskboard()} and \code{viz_gantt()}
#'
#' Credit goes to this Stack Overflow answer for figuring out how to do this:
#' https://stackoverflow.com/questions/42259826/hyperlinking-text-in-a-ggplot2-visualization/42262407
#'
#' @param g ggplot2 object returned by \code{viz_gantt()} or \code{viz_taskboard()}
#' @param filepath Location to save resulting SVG file of ggplot2, if desired. Leave blank for
#'     function to output message precisely as needed to render in HTML RMarkdown with chunk
#'     option \code{results = 'asis'}
#'
#' @return SVG version of ggplot2 object with links to relevant GitHub issues. Either writes output
#'     to file or to console (to be captured in RMarkdown) depending on existence of \code{filepath} argument
#' @export
#'
#' @examples
#' \dontrun{
#' # In R, to save to file:
#' taskboard <- viz_taskboard(issues)
#' viz_linked(taskboard, "my_folder/my_file.svg")
#'
#' # In RMarkdown chunk, to print as output:
#' ```{r results = 'asis', echo = FALSE}
#' gantt <- viz_gantt(issues)
#' viz_linked(gantt)
#' ````
#' }

viz_linked <- function(g, filepath){

  if (!requireNamespace("xml2", quietly = TRUE)) {
    message(
      paste0("Package \"xml2\" is needed to edit SVG.",
             "Please install \"xml2\" or use the non-linked version."),
      call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message(
      paste0("Package \"ggplot2\" is needed to save the image.",
             "Please install \"ggplot2\" or use the non-linked version."),
      call. = FALSE)
  }

  if (!requireNamespace("purrr", quietly = TRUE)) {
    message(
      paste0("Package \"purrr\" is needed for image conversion.",
             "Please install \"purrr\" or use the non-linked version."),
      call. = FALSE)
  }

  # create text-link mapping
  links <- get_text_link_map(g)

  # save current ggplot at svg
  tf <- tempfile(fileext = ".svg")
  suppressMessages( ggplot2::ggsave(tf , g ) )

  # add links to svg
  xml <- xml2::read_xml(tf)
  xml %>%
    xml2::xml_find_all(xpath="//d1:text") %>%
    purrr::keep(xml2::xml_text(.) %in% names(links)) %>%
    xml2::xml_add_parent("a", "xlink:href" = links[xml2::xml_text(.)], target = "_blank")

  if(missing(filepath)){
    xml2::write_xml(xml, tf)
    cat( readLines(tf), sep = "\n" )
  }
  else{
    xml2::write_xml(xml, filepath )
  }

  # clean up environment
  unlink(tf)

}


# internal functions/methods for deriving links ----

#' @keywords internal
get_text_link_map <- function(g){

  # ensure graph data has preserved links
  if(!("url" %in% names(g$data))){
    stop( paste(
      "url column was not included in dataset passed to viz funcion.",
      "Please remake the plot with this field included before passing to viz_linked.",
      sep = "\n"
    ))
  }

  # throw more readable error message if type unsupported
  supported_plots <- c("gantt", "taskboard")
  if(intersect(class(g), supported_plots) == 0) {
    stop( paste(
      "Object provided does not have an implementation for adding links.",
      "Supported plots types are:",
      paste(supported_plots, collapse = ", "),
      sep = "\n"
    ))
  }

  # dispatch to S3 method
  UseMethod('get_text_link_map', g)
}

#' @keywords internal
get_text_link_map.gantt <- function(g){

  link_text_fmt <- g$data$title
  link_text <- lapply(link_text_fmt, FUN = function(x) strwrap(x, width = g[['str_wrap_width']] ))
  link_length <- vapply(link_text, FUN = length, FUN.VALUE = integer(1))
  url_repeat <- rep(g$data$url, link_length)
  links <- stats::setNames(url_repeat, link_text)
  return(links)

}

#' @keywords internal
get_text_link_map.taskboard <- function(g){

  link_text_fmt <- paste0("#", g$data$number, ": ", g$data$title)
  link_text <- lapply(link_text_fmt, FUN = function(x) strwrap(x, width = g[['str_wrap_width']] ))
  link_length <- vapply(link_text, FUN = length, FUN.VALUE = integer(1))
  url_repeat <- rep(g$data$url, link_length)
  links <- stats::setNames(url_repeat, link_text)
  return(links)

}




