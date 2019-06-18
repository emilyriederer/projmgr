#' Pivot list-column elements to indicator variables
#'
#' Some outputs of the \code{get_} and \code{parse_} functions contain list-columns (e.g. the labels column in the issues dataframe).
#' This is an efficient way to represent the provided information, but may make certain information seem slightly inaccessible. This
#' function allows users to "pivot" these list columns and instead create a seperate indicator variable to represent the presence
#' or absence of matches within the list column.
#'
#' For example, if a repository tags issues with "priority:high", "priority:medium", and "priority:low" along with other labels,
#' this function could be used to create separate "high", "medium", and "low" columns to denote different issue severities. This
#' could be done with \code{listcol_pivot(data, "labels_name", "^priority:", function(x) sub("^priority:", ""))}
#'
#' @param data Dataframe containing a list column (e.g. an issues dataframe)
#' @param col_name Character string containing column name of list column (e.g. \code{labels_name} or \code{assignees_login})
#' @param regex Character string of regular expression to identify list items of interest (e.g. \code{"^priority:", "^(bug|feature)$"})
#' @param transform_fx Function to transform label name before converting to column (e.g. \code{sub(":", "_")})
#' @param delete_orig Logical denoting whether or not to delete original list column provided by \code{col_name}
#'
#' @return Dataframe additional logical columns denoting absence / presence of specified list-column elements
#' @export
#'
#' @examples
#' \dontrun{
#' issues <-
#'   get_issues(repo) %>%
#'   parse_issues() %>%
#'   listcol_pivot(col_name = "labels_name",
#'                 regex = "^priority:",
#'                 transform_fx = function(x) paste0("label_",x)),
#'                 delete_orig = TRUE)
#' }

listcol_pivot <- function(data, col_name, regex = ".", transform_fx = identity, delete_orig = FALSE) {

  items <- grep(regex, unlist(data[[col_name]]), value = TRUE)
  for (i in items) {data[[transform_fx(i)]] <- vapply(data[[col_name]],
                                                     FUN = function(x) i %in% x,
                                                     FUN.VALUE = logical(1))}
  if (delete_orig) data[[col_name]] <- NULL
  return(data)

}

#' Filter dataframe by list-column elements
#'
#' Some outputs of the \code{get_} and \code{parse_} functions contain list-columns (e.g. the labels column in the issues dataframe).
#' This is an efficient way to represent the provided information, but may make certain information seem slightly inaccessible. This
#' function allows users to filter list columns by the presence of one or more values or a regular expression.
#'
#' @param matches A character vector containing a regular expression or one or more exact-match values. An observation will be kept
#'   in the returned data if any of the
#' @param is_regex Logical to indicate whether charalcter indicates a regular expression or specific values
#' @param any Logical to indicate whether rows should be kept if list-column contains any of elements in \code{matches}
#'     (defaults to \code{TRUE}) or if it must contain all of the elements in \code{matches} (\code{FALSE}). Only respected
#'     when \code{matches} contains exact
#' @inheritParams listcol_pivot
#'
#' @return Dataframe containing only rows in which list-column contains element matching provided criteria
#' @export
#'
#' @examples
#' \dontrun{
#' # keep observation containing a label of either "bug" or "feature"
#' issues <-
#'   get_issues(repo) %>%
#'   parse_issues() %>%
#'   listcol_filter(col_name = "labels_name", matches = c("bug", "feature"))
#'
#' # keep observation containing a label of both "bug" and "feature"
#' issues <-
#'   get_issues(repo) %>%
#'   parse_issues() %>%
#'   listcol_filter(col_name = "labels_name", matches = c("bug", "feature"), any = FALSE)
#'
#' # keep observation containing a label that starts with "region"
#' issues <-
#'   get_issues(repo) %>%
#'   parse_issues() %>%
#'   listcol_filter(col_name = "labels_name", matches = "^region:", is_regex = TRUE)
#' }

listcol_filter <- function(data, col_name, matches, is_regex = FALSE, any = TRUE) {

  eval_fx <-
    if (is_regex) function(x) {any(grepl(matches, x))}
  else if (any) function(x) any(matches %in% x)
  else function(x) all(matches %in% x)
  rows <- vapply(data[[col_name]],
                 FUN = eval_fx,
                 FUN.VALUE = logical(1))
  return(data[rows,])

}

#' Extract new dataframe column from list-column matching pattern
#'
#' Creaes a new column in your dataframe based on a subset of list-column values following a certain patten. For example,
#' this is useful if you have labels you always apply to a repository with a set structure, e.g. key-value pairs like "priority:high",
#' "priority:medium", and "priority:low" or other structures like "engagement-team", "teaching-team", etc. This function could
#' create a new variable (e.g. "priority", "team") with the values encoded within the labels.
#'
#' This function works only if each observatino contains at most one instance of a given patterns. When multiple labels match the
#' same pattern, one is returned at random.
#'
#' @inheritParams listcol_pivot
#' @param new_col_name Optional name of new column. Otherwise \code{regex} is used, stripped of any leading or trailing punctuation
#' @param sep_matches Optional character to use to separate out (e.g. "," or ";"). If \code{NA}, only one of matches will be kept
#'     arbitrarily (whichever happens to come first in the list columns)
#' @param keep_regex Optional logical denoting whether to keep regex part of matched item in value. Defaults to \code{FALSE}
#'
#' @return Dataframe with new column taking values extracted from list column
#' @export
#'
#' @examples
#' \dontrun{
#' issues <-
#'     get_issues(repo) %>%
#'     parse_issues() %>%
#'     listcol_extract("labels_name", "-team$")
#' }

listcol_extract <- function(data, col_name, regex, new_col_name = NULL, sep_matches = ",", keep_regex = FALSE) {

  new_col_vals <- sapply(data[[col_name]],
                         FUN = function(x) {
                           out <- grep(regex, x, value = TRUE)
                           if (!keep_regex) out <- sub(regex, "", out)
                           if (length(out) == 0) out <- NA_character_
                           if (length(out) > 1) {
                             if(!is.na(sep_matches)) paste(sort(out), collapse = sep_matches)
                             else warning(paste("More than one pattern match in single observation.",
                                                "Results contain one of matches arbitrarily",
                                                collapse = "\n")) }
                           return(out[1])
                         },
                         USE.NAMES = TRUE)

  if (is.null(new_col_name)) {
    new_col_name <- sub("\\^|\\$","", regex)
    new_col_name <- sub("(^[[:punct:]])|([[:punct:]]$)", "", new_col_name)
  }

  data[[new_col_name]] <- new_col_vals
  return(data)

}
