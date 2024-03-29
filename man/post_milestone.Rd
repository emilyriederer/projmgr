% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post.R
\name{post_milestone}
\alias{post_milestone}
\title{Post milestone to GitHub repository}
\usage{
post_milestone(ref, title, ...)
}
\arguments{
\item{ref}{Repository reference (list) created by \code{create_repo_ref()}}

\item{title}{Milestone title (required)}

\item{...}{Additional user-defined body parameters. Use \code{browse_docs()} to learn more.}
}
\value{
Number (identifier) of posted milestone
}
\description{
Post milestone to GitHub repository
}
\examples{
\dontrun{
myrepo <- create_repo_ref('emilyriederer', 'myrepo')
post_milestone(myrepo,
  title = 'this is the title of the milestone',
  description = 'this is the long and detailed description',
  due_on = '2018-12-31T12:59:59z')
}
}
\seealso{
Other milestones: 
\code{\link{get_milestones}()},
\code{\link{parse_milestones}()}
}
\concept{milestones}
