#' Core code for all GET calls
#'
#' @param api_endpoint API endpoint
#' @param ref Repository reference (list) created by `create_repo_ref()`
#' @param limit Number of records to return, passed directly to `gh` documentation. Defaults to
#'     `Inf` which requests all records but may required multiple API calls
#' @param ... Additional user-defined query parameters. Use `browse_docs()` to learn more.
#'
#' @keywords internal
#' @return Content of GET request as list

get_engine <- function(api_endpoint, ref, limit = Inf, ...){

  res <- gh::gh(
    endpoint = paste0(ref$base_url, ref$repo_path, api_endpoint),
    ...,
    .token = Sys.getenv(ref$id),
    .method = "GET",
    .send_headers = c("User-Agent" = "https://github.com/emilyriederer/projmgr"),
    .limit = limit
  )

  # handle special case when single item returned ----
  if (!is.null( names(res))) { res <- list(res) }

  # attach fields for the repo owner and name ----
  ## only if results are non-empty
  if (length(res) == 1 & res[1] == "") { return(res) }

  add_fields <- function(elt) {
    elt[["repo_owner"]] <- ref$repo_owner
    elt[["repo_name"]] <- ref$repo_name
    return(elt)
  }
  res <- lapply(res, add_fields)

  return(res)

}

#' Core code for all POST calls
#'
#' @param api_endpoint API endpoint
#' @param ref Repository reference (list) created by `create_repo_ref()`
#' @param ... Additional user-defined body parameters. Use `browse_docs()` to learn more.
#'
#' @keywords internal
#' @return Content of POST request as list

post_engine <- function(api_endpoint, ref, ...){

  gh::gh(
    endpoint = paste0(ref$base_url, ref$repo_path, api_endpoint),
    ...,
    .token = Sys.getenv(ref$id),
    .method = "POST",
    .limit = Inf,
    .send_headers = c("User-Agent" = "https://github.com/emilyriederer/projmgr")
  )

}

#' Core code for all PATCH calls
#'
#' @param api_endpoint API endpoint
#' @param ref Repository reference (list) created by `create_repo_ref()`
#' @param ... Additional user-defined body parameters
#'
#' @keywords internal
#' @return Content of PATCH request as list

patch_engine <- function(api_endpoint, ref, ...){

  gh::gh(
    endpoint = paste0(ref$base_url, ref$repo_path, api_endpoint),
    ...,
    .token = Sys.getenv(ref$id),
    .method = "PATCH",
    .limit = Inf,
    .send_headers = c("User-Agent" = "https://github.com/emilyriederer/projmgr")
  )

}

# internal functions focused on cleaning inputs to reduce common api errors

#' Validate that all user-defined inputs (GET query, POST body, etc.) are valid
#'
#' @param input List of user-provided input parameters
#' @param allowed_vars Character vector of allowed but not required variables
#'
#' @return No return. Throws errors if user-defined inputs are invalid.
#' @keywords internal

validate_inputs <- function(input, allowed_vars){

  extra_vars <- setdiff(names(input), allowed_vars)

  # no disallowed vars exist
  if (length(extra_vars) > 0) {
    stop(
      paste0(
        "The following user-inputted variables are not relevant to this API request: \n + ",
        paste(extra_vars, collapse = ","), "\n",
        "Allowed variables are: \n + ",
        paste(allowed_vars, collapse = ","), "\n",
        "Please remove unallowed fields and try again.", "\n",
        "Use the browse_docs() function or \n",
        "visit https://developer.github.com/v3/ for full API documentation."
      ),
      call. = FALSE
    )
  }

}
