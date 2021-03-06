% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_stats.R
\name{read_stats}
\alias{read_stats}
\title{Read a .json file that was produced with \code{write_stats}}
\usage{
read_stats(file)
}
\arguments{
\item{file}{Path to the tidy stats data file}
}
\description{
\code{read_stats} can read in a .json file containing the statistical output
that was produced with \code{write_stats}. It returns a list containing the 
results, with the identifier as the name for each list element.
}
\examples{
results <- read_stats(system.file("results.json", package = "tidystats"))

}
