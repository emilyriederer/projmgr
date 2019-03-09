#' Create reference to a GitHub repository
#'
#' This function constructs a list of needed information to send API calls to a specific
#' GitHub repository. Specifically, it stores information on the repository's name and
#' owner, the type (whether or not Enterprise GitHub), and potentially credentials to authenticate.
#'
#' Note that this package can be used for GET requests on public repositories without any authentication
#' (resulting in a lower rate limit.) To do this, simply pass any string into \code{identifier} that is not
#' an environment variable already defined for your system (e.g. accessible through \code{Sys.getenv("MY_VAR")})
#'
#' @param repo_owner Repository owner's username or GitHub Organization name
#' @param repo_name Repository name
#' @param is_enterprise Boolean denoting whether or not working with Enterprise GitHub.Defaults to \code{FALSE}
#' @param hostname Host URL stub for Enterprise repositories (e.g. "mycorp.github.com")
#' @param identifier Ideally should be left blank and defaults to using \code{GITHUB_PAT} or \code{GITHUB_ENT_PAT}
#'     environment variables as Personal Access Tokens. If \code{identifier} has a value and \code{password} does not,
#'     this is assumed to be an alternative name of the environment variable to use for your Personal Access Token. If
#'     \code{password} is also provided, this is interpreted as a username.
#' @param password GitHub password. Ideally should be left blank and then defaults to empty string which works in conjunction with personal access tokens. Required if supplying username.
#'
#' @return List of repository reference information and credentials
#' @export
#'
#' @examples
#' \dontrun{
#' myrepo <- create_repo_ref('emilyriederer', 'myrepo')
#' }
#'

create_repo_ref <-
  function(repo_owner,
           repo_name,
           is_enterprise = FALSE,
           hostname = "",
           identifier = "",
           password = ""){

    # determine authentication strategy ----
    if(identifier != "" & password == ""){
      id_sys_var <- identifier
      pw_sys_var <- ""
      message(paste("Requests will authenticate with", identifier))
    }
    else if(identifier != "" & password != ""){

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
      stop(paste("Insufficient credentials provided.",
                 "Please see vignette on Personal Access Tokens or provide login information.",
                 "Otherwise, enter a meaningless string into the identifier field to attempt to proceed without credentials.",
                 collapse = "\n"),
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
    class(ref) <- c("repo_ref", class(ref))

    # return repo reference ----
    return(ref)

  }
