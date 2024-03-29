% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taskboard-helpers.R
\name{taskboard_helpers}
\alias{taskboard_helpers}
\alias{is_labeled}
\alias{is_labeled_with}
\alias{is_assigned}
\alias{is_assigned_to}
\alias{is_in_a_milestone}
\alias{is_in_milestone}
\alias{is_created_before}
\alias{is_part_closed}
\alias{is_due}
\alias{is_due_before}
\alias{has_n_commits}
\title{Tag "in-progress" items for taskboard visualization}
\usage{
is_labeled()

is_labeled_with(label, any = TRUE)

is_assigned()

is_assigned_to(login, any = TRUE)

is_in_a_milestone()

is_in_milestone(number)

is_created_before(created_date)

is_part_closed()

is_due()

is_due_before(due_date)

has_n_commits(events, n = 1)
}
\arguments{
\item{label}{Label name(s) as character vector}

\item{any}{When the supplied vector has more than one value, should the result return TRUE if
any of those values are present in the dataset (logical OR)}

\item{login}{User login(s) as character vector}

\item{number}{Milestone number}

\item{created_date}{Date as character in "YYYY-MM-DD" format}

\item{due_date}{Date as character in "YYYY-MM-DD" format}

\item{events}{Dataframe containing events for each issue in data}

\item{n}{Minimum of commits required to be considered in progress}
}
\value{
Function to be passed as \code{in_progress_when} argument in \code{viz_taskboard()}
}
\description{
The \code{viz_taskboard()} function creates a three-column layout of entities that are
not started, in progress, or done. Objects are classified as done when they have a
\code{state} of "closed". Object are classified as "To-Do" when they are neither "Closed"
or "In Progress". However, what constistutes "In Progress" is user and project dependent.
Thus, these functions let users specify what they mean.
}
\details{
General options:

\itemize{
\item is_created_before: Was created before a user-specified data (as "YYYY-MM-DD" character string)
}

Issue-specific options:

\itemize{
\item is_labeled_with: User-specified label (as character string) exists
\item is_assigned: Has been assigned to anyone
\item is_assigned_to: Has been assigned to specific user-specified login (as character string)
\item is_in_a_milestone: Has been put into any milestone
\item is_in_milestone: Has been put into a specific milestone
}

Milestone-specific options:

\itemize{
\item is_part_closed: Has any of its issues closed
\item is_due: Has a due date
\item is_due_before: Has a due data by or before a user-specified date (as "YYYY-MM-DD" character string)
}
}
\examples{
\dontrun{
viz_taskboard(issues, in_progress_when = is_labeled_with('in-progress'))
viz_taskboard(milestones, in_progress_when = is_created_before('2018-12-31'))
viz_taskboard(issues, in_progress_when = is_in_milestone())
report_taskboard(issues, in_progress_when = is_labeled_with('in-progress'))
report_taskboard(milestones, in_progress_when = is_created_before('2018-12-31'))
report_taskboard(issues, in_progress_when = is_in_milestone())
}
}
