#' @export
print.repo_ref <- function(x, ...){

  writeLines(
    paste0(
      "+ Repository Owner: ", x$repo_owner, "\n",
      "+ Repository: ", x$repo_name, "\n",
      "+ From URL: ", x$base_url, "\n",
      "+ Authenticating with: ", x$id, "\n"
    )
  )
  invisible(x)

}

#' @export
print.plan <- function(x, ...){

  milestones <- vapply(x, FUN = function(x) x[["title"]], FUN.VALUE = character(1))
  n_issues <- vapply(x, FUN = function(x) length(x[["issue"]]), FUN.VALUE = integer(1))

  # format plan component ----
  format_component <- function(index, milestones, n_issues){
    paste0(index, ". ", milestones, " (", n_issues, " issues) \n")
  }

  out <- mapply(format_component, seq_along(milestones), milestones, n_issues)
  out <- paste(out, collapse = "")
  out <- paste0("Plan: \n", out)

  writeLines(out)
  invisible(x)

}

#' @export
print.todo <- function(x, ...){

  issues <- vapply(x, FUN = function(x) x[["title"]], FUN.VALUE = character(1))

  out <- mapply(function(index, issues) paste0(index, ". ", issues, "\n"),
                seq_along(issues), issues)
  out <- paste(out, collapse = "")
  out <- paste0("To Do: \n", out)

  writeLines(out)
  invisible(x)

}
