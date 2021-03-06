% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-string.R
\name{strip_empty_lines}
\alias{strip_empty_lines}
\title{Remove empty lines}
\usage{
strip_empty_lines(str)
}
\arguments{
\item{str}{A character string}
}
\value{
The character string stripped from empty lines
}
\description{
Remove empty lines from a string, such as "" or "  "
}
\examples{
\dontrun{
strip_empty_lines(c("test","", "  "))
}
}
\keyword{internal}
