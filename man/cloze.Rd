% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quiz_types.R
\name{cloze}
\alias{cloze}
\title{Question of type cloze.}
\usage{
cloze(name, text, file = NULL, grade = 1, penalty = 1, hidden = 0)
}
\arguments{
\item{name}{[\code{character}]\cr Name for cloze.}

\item{text}{[\code{character}]\cr Question text for cloze.}

\item{file}{[\code{character}]\cr Optional file encoded as base64. Default is
NULL for no file.}

\item{grade}{[\code{number}]\cr Grade for the question. Default is 1.}

\item{penalty}{[\code{number}]\cr Penalty for the question. Default is 1.}

\item{hidden}{[\code{choice}]\cr 0 for show and 1 for hidden.}
}
\value{
Question of type cloze as character.
}
\description{
Generate a question of type cloze.
}
\examples{
cloze(name = "Cloze", text = cloze_mc_multi(choices = 1:5, pos_true = 3))

}
