% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_milestones}
\alias{get_milestones}
\title{Get milestones from GitHub repository}
\usage{
get_milestones(ref, ...)
}
\arguments{
\item{ref}{Repository reference (list) created by \code{create_repo_ref()}}

\item{...}{Additional user-defined query parameters. Use \code{browse_docs()} to learn more.}
}
\value{
Content of GET request as list
}
\description{
A single milestone can be obtained by identification number of \code{number} is passed through \code{...}s.
In this case, all other query parameters will be ignored.
}
\examples{
\dontrun{
myrepo <- create_repo_ref("emilyriederer", "myrepo")
milestones_res <- get_milestones(myrepo)
milestones <- parse_milestones(milestones_res)
}
}
\seealso{
Other milestones: 
\code{\link{parse_milestones}()},
\code{\link{post_milestone}()}
}
\concept{milestones}
