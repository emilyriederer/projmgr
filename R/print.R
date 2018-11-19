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

  milestones <- purrr::map(x, "title")
  n_issues <- purrr::map(x, ~length(.[["issue"]]))

  out <- "Plan: \n"
  for(i in 1:length(milestones)){
    out <-
      paste0(out, i, ". ", milestones[i], " (",n_issues[i]," issues) \n")
  }

  writeLines(out)

}

#' @export
print.todo <- function(x, ...){

  issues <- purrr::map(x, "title")

  out <- "To Do: \n"
  for(i in 1:length(issues)){
    out <-
      paste0(out, i, ". ", issues[i], "\n")
  }

  writeLines(out)

}
