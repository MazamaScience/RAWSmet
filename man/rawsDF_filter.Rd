% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rawsDF_filter.R
\name{rawsDF_filter}
\alias{rawsDF_filter}
\title{General purpose data filtering for rawsDF objects}
\usage{
rawsDF_filter(rawsDF = NULL, ...)
}
\arguments{
\item{rawsDF}{\emph{rawsDF} object.}

\item{...}{Logical predicates defined in terms of the variables in the 
\code{rawsDF}.}
}
\value{
A subset of the incoming \code{rawsDF}.
}
\description{
A generalized data filter for \emph{rawsDF} objects to 
choose rows/cases where conditions are true.  Multiple conditions are 
combined with \code{&} or separated by a comma. Only rows where the condition 
evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
are dropped.
}
\examples{
\donttest{
library(RAWSmet)

rawsObject <- example_cefa_Saddle_Mountain
rawsDF <- rawsObject \%>\% raws_toRawsDF()

daytime <- rawsDF_filter(rawsDF, solarRadiation > 0)
head(daytime)
}

}
\seealso{
\link{rawsDF_filterDate}
}
