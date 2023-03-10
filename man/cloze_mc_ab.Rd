% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quiz_types.R
\name{cloze_mc_ab}
\alias{cloze_mc_ab}
\title{Single cloze multichoice question with 2 possible answers.}
\usage{
cloze_mc_ab(a, b, true, point = 1)
}
\arguments{
\item{a}{[\code{character}]\cr First answer.}

\item{b}{[\code{character}]\cr Second answer.}

\item{true}{[\code{choice}]\cr "a" for a TRUE and "b" for b TRUE.}

\item{point}{[\code{number}]\cr Point for correct answer. Default is 1.}
}
\value{
Cloze multichoice question as character.
}
\description{
Single cloze multichoice question with 2 possible answers.
}
\examples{
cloze_mc_ab(a = "Yes", b = "No", true = "a")

}
