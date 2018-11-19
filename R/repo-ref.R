#' Create reference to a GitHub repository
#'
#' This function constructs a list of needed information to send API calls to a specific
#' GitHub repository. Specifically, it stores information on thhe repository's name and
#' owner, the type (whether or not Enterprise GitHub), and potentially credentials to authenticate.
#'
#' @param repo_owner Repository owner's eid or GitHub Organization name
#' @param repo_name Repository name
#' @param is_enterprise Boolean denoting whether or not working with Enterprise GitHub.Defaults to \code{FALSE}
#' @param hostname Host URL stub for Enterprise repositories (e.g. if URL is "github.abc.com" then set to "abc")
#' @param identifier GitHub username. Ideally should be left blank and then defaults to \code{GITHUB_PAT} or \code{GITHUB_ENT_PAT} environment variables if not provided.
#' @param password GitHub password. Ideally should be left blank and then defaults to empty string which works in conjunction with personal access tokens. Required if supplying username.
#'
#' @return List of repository reference information and credentials
#' @export
#'
#' @examples
#' \dontrun{
#' tidytracker <- create_repo_ref('emilyriederer', 'tidytracker')
#' }
#'

create_repo_ref <-
  function(repo_owner,
           repo_name,
           is_enterprise = FALSE,
           hostname = "",
           identifier = "",
           password = ""){

    # validate valid entries for repo_owner and repo_name ----
    stopifnot(
      is.character(repo_owner),
      grepl("^[a-zA-Z0-9][a-zA-Z0-9-]{0,38}$", repo_owner),
      is.character(repo_name),
      grepl("^[a-zA-Z0-9-]{1,100}$", repo_name)
    )

    # determine authentication strategy ----
    if(identifier != "" & password != ""){

      # save credentials to local environment
      id_sys_var <- paste0("GITHUB_USER_", repo_name)
      pw_sys_var <- paste0("GITHUB_USER_", repo_name)
      eval(parse(text = paste(id_sys_var, "=", identifier)))
      eval(parse(text = paste(pw_sys_var, "=", password)))

      message(
        paste0("Requests will authenticate with supplied username and password.",
               "Please see vignette on Personal Access Tokens for improved security.")
      )
    }
    else if(!is_enterprise & Sys.getenv("GITHUB_PAT") != ""){
      id_sys_var <- "GITHUB_PAT"
      pw_sys_var <- ""
      message("Requests will authenticate with GITHUB_PAT")
    }
    else if(is_enterprise & Sys.getenv("GITHUB_ENT_PAT") != ""){
      id_sys_var <- "GITHUB_ENT_PAT"
      pw_sys_var <- ""
      message("Requests will authenticate with GITHUB_ENT_PAT")
    }
    else{
      stop("Insufficient credentials provided. Please see vignette on Personal Access Tokens or provide login information.",
           .call = FALSE)
    }

    # assign base url ----

    if(!is_enterprise){
      base_url <- "https://api.github.com/"
    }
    else if(is_enterprise & length(hostname)>0){
      base_url <- paste0("https://",hostname,"/api/v3/")
    }
    else if(is_enterprise){
      stop("hostname argument must be provided",
           call. = FALSE)
    }
    else{
      stop("is_enterprise argument must be TRUE or FALSE with hostname provided if TRUE",
           call. = FALSE)
    }

    # create repo reference list ----
    ref <- list(
      repo_owner = repo_owner,
      repo_name = repo_name,
      id = id_sys_var,
      pw = pw_sys_var,
      base_url = base_url,
      repo_path = paste("repos", repo_owner, repo_name, sep = "/")
    )


    # add repo-ref class ----
    class(ref) <- c("repo_ref", "list")

    # return repo reference ----
    return(ref)

  }


#' @export
print.repo_ref <- function(x){

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
