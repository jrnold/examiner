\name{problem}
\alias{problem}
\title{Create a problem object}
\usage{
problem(text = "", answers = character(), solution = "", correct = 1L,
  first = 0L, last = 0L, randomizable = TRUE)
}
\arguments{
  \item{text}{\code{character} vector with the prompt for
  the problem.}

  \item{answers}{A \code{list} of \code{problem} or
  \code{problemset} objects.}

  \item{correct}{Indices of answers which are correct.}

  \item{first}{Number of observations at the beginning of
  \code{answers} which will not be shuffled.}

  \item{last}{Number of observations at the end of
  \code{answers} which will not be shuffled.}

  \item{randomizable}{Can the answers be shuffled?}
}
\value{
An object of class \code{problem}.
}
\description{
Create a problem object
}
\keyword{internal}

