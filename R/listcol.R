#' Pivot list-column elements to indicator variables
#'
#' Some outputs of the `get_` and `parse_` functions contain list-columns (e.g. the labels column in the issues dataframe).
#' This is an efficient way to represent the provided information, but may make certain information seem slightly inaccessible. This
#' function allows users to "pivot" these list columns and instead create a seperate indicator variable to represent the presence
#' or absence of matches within the list column.
#'
#' For example, if a repository tags issues with "priority:high", "priority:medium", and "priority:low" along with other labels,
#' this function could be used to create separate "high", "medium", and "low" columns to denote different issue severities. This
#' could be done with `listcol_pivot(data, "labels_name", "^priority:", function(x) sub("^priority:", ""))`
#'
#' @param data Dataframe containing a list column (e.g. an issues dataframe)
#' @param col_name Character string containing column name of list column (e.g. `labels_name` or `assignees_login`)
#' @param regex Character string of regular expression to identify list items of interest (e.g. `"^priority:", "^(bug|feature)$"`)
#' @param transform_fx Function to transform label name before converting to column (e.g. `sub(":", "_")`)
#' @param delete_orig Logical denoting whether or not to delete original list column provided by `col_name`
#'
#' @return Dataframe additional logical columns denoting absence / presence of specified list-column elements
#' @export
#'
#' @examples
#' \dontrun{
#' issues <- get_issues(repo)
#' issues_df <- parse_issues(issues)
#' listcol_pivot(issues_df,
#'   col_name = "labels_name",
#'   regex = "^priority:",
#'   transform_fx = function(x) paste0("label_",x),
#'   delete_orig = TRUE)
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
#' Some outputs of the `get_` and `parse_` functions contain list-columns (e.g. the labels column in the issues dataframe).
#' This is an efficient way to represent the provided information, but may make certain information seem slightly inaccessible. This
#' function allows users to filter list columns by the presence of one or more values or a regular expression.
#'
#' @param matches A character vector containing a regular expression or one or more exact-match values. An observation will be kept
#'   in the returned data if any of the
#' @param is_regex Logical to indicate whether charalcter indicates a regular expression or specific values
#' @inheritParams listcol_pivot
#'
#' @return Dataframe containing only rows in which list-column contains element matching provided criteria
#' @export
#'
#' @examples
#' \dontrun{
#' issues <- get_issues(repo)
#' issues_df <- parse_issues(issues)
#'
#' # keep observation containing a label of either "bug" or "feature"
#' listcol_filter(issues_df, col_name = "labels_name", matches = c("bug", "feature"))
#'
#' # keep observation containing a label that starts with "region"
#' listcol_filter(issues_df, col_name = "labels_name", matches = "^region:", is_regex = TRUE)
#' }

listcol_filter <- function(data, col_name, matches, is_regex = FALSE) {

 # prep regex ----
  regex <- if (is_regex) matches else paste0("^", matches, "$")
  regex <- paste0(regex, collapse = "|")

  # find rows ----
  rows <- vapply(data[[col_name]], FUN = function(x) any(grepl(regex, x)), FUN.VALUE = logical(1))
  return(data[rows,])

  # eval_fx <-
  # if (is_regex) function(x) {grepl(paste(matches, collapse = "|"), x)}
  # else if (any) function(x) any(matches %in% x)
  # else function(x) all(matches %in% x)
  # rows <- vapply(data[[col_name]],
  #                FUN = eval_fx,
  #                FUN.VALUE = logical(1))
  # return(data[rows,])

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
#' @param new_col_name Optional name of new column. Otherwise `regex` is used, stripped of any leading or trailing punctuation
#' @param keep_regex Optional logical denoting whether to keep regex part of matched item in value. Defaults to `FALSE`
#'
#' @return Dataframe with new column taking values extracted from list column
#' @export
#'
#' @examples
#' \dontrun{
#' issues <- get_issues(repo)
#' issues_df <- parse_issues(issues)
#' listcol_extract(issues_df, "labels_name", "-team$")
#' }

listcol_extract <- function(data, col_name, regex, new_col_name = NULL, keep_regex = FALSE) {

  new_col_vals <- lapply(data[[col_name]], function(x) grep(regex, x, value = TRUE))
  if (!keep_regex) new_col_vals <- lapply(new_col_vals, function(x) sub(regex, "", x))
  new_col_vals <- lapply(new_col_vals, function(x) if (length(x) == 0) NA_character_ else x)
  new_col_vals <-
    if (max(vapply(new_col_vals, length, numeric(1))) > 1) {new_col_vals}
    else as.character(new_col_vals)

  if (is.null(new_col_name)) {
    new_col_name <- gsub("[[:punct:]]", "_", gsub("(^[[:punct:]]*)|([[:punct:]]*$)", "", regex))
  }

  data[[new_col_name]] <- new_col_vals
  return(data)

}
