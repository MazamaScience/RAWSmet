% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rawsList_utils.R
\name{rawsList_removeEmpty}
\alias{rawsList_removeEmpty}
\title{Remove \emph{raws_timeseries} objects from a \emph{rawsList}.}
\usage{
rawsList_removeEmpty(rawsList = NULL)
}
\arguments{
\item{rawsList}{\emph{rawsList} object}
}
\value{
\emph{rawsList} with empty stations removed.
}
\description{
Removes all empty \emph{raws_timeseries} objects from a
\emph{rawsList}. This may be necessary after using \code{rawsList_filterDate()}
so that downstream functions are guaranteed to have only stations with data.
}
