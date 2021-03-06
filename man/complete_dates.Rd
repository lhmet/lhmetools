% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates-manipulation.R
\name{complete_dates}
\alias{complete_dates}
\title{Complete a data frame with missing combination of variables (date and group)}
\usage{
complete_dates(x, group = "id", time_step = "days")
}
\arguments{
\item{x}{data.frame with a column date}

\item{group}{character vector of group variables}

\item{time_step}{character scalar (e.g., "hours", "days", "months")}
}
\value{
tibble with a regular and constant time step
}
\description{
Complete a data frame with missing combination of variables (date and group)
}
\examples{
if(TRUE){
 dates_comp <- complete_dates(
   x = dates_miss,
   group = c("id", "g"),
   time_step = "months"
 )
dates_comp
}
}
\seealso{
Other date manipulation: 
\code{\link{dates_from_files}()}
}
\concept{date manipulation}
