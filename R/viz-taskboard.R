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

viz_taskboard <- function(issues){

  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

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

  return(g)

}

#' Save SVG of Agile-style task board of issue status with links to issues
#'
#' This function creates the same plot as \code{viz_taskboard} then edits the
#' underlying XML so that the "cards" are linked to the corresponding issues on GitHub.
#' It saves a file with the reuslting SVG, which can then be read into an RMarkdown
#' HTML document as shown in the Examples.
#'
#' Credit goes to this Stack Overflow answer for figuring out how to do this:
#' https://stackoverflow.com/questions/42259826/hyperlinking-text-in-a-ggplot2-visualization/42262407
#'
#' @inheritParams viz_gantt_closed_links
#' @inherit viz_gantt_closed_links return
#' @export
#'
#' @family issues
#'
#' @examples
#' \dontrun{
#' # In R, to save to file:
#' viz_taskboard_links(issues, "my_folder/my_file.svg")
#'
#' # In RMarkdown chunk, to print as output:
#' ```{r results = 'asis', echo = FALSE}
#' g <- viz_taskboard(issues)
#' viz_taskboard_links(g)
#' ````
#' }

viz_taskboard_links <- function(g, filepath){

  if (!requireNamespace("xml2", quietly = TRUE)) {
    message(
      paste0("Package \"xml2\" is needed to edit SVG.",
             "Please install \"xml2\" or use viz_taskboard for the non-linked version."),
      call. = FALSE)
  }

  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package \"stringr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # save current ggplot at svg
  tf <- tempfile(fileext = ".svg")
  ggsave(tf , g )

  # update svg w links
  links <- tibble::tibble(
      url = g$data$url,
      name =
        paste0("#", g$data$id, ": ", g$data$title) %>%
        stringr::str_wrap(width = 20) %>%
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
    cat(readLines(tf), sep = "\n")
  }
  else{ xml2::write_xml(xml, filepath ) }

  # clean up environment
  unlink(tf)

}
