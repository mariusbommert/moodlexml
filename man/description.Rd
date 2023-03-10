% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quiz_types.R
\name{description}
\alias{description}
\title{Question of type description.}
\usage{
description(name, text, file = NULL, hidden = 0)
}
\arguments{
\item{name}{[\code{character}]\cr Name for description.}

\item{text}{[\code{character}]\cr Question text for description.}

\item{file}{[\code{character}]\cr Optional file encoded as base64. Default is
NULL for no file.}

\item{hidden}{[\code{choice}]\cr 0 for show and 1 for hidden.}
}
\value{
Question of type description as character.
}
\description{
Generate a question of type description.
}
\examples{
description(name = "Description", text = "text")

}
