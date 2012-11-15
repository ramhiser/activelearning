\name{query_oracle}
\alias{query_oracle}
\title{Automatic query of an oracle.}
\usage{
  query_oracle(i, y_truth)
}
\arguments{
  \item{i}{a vector of the queried observation indices}

  \item{y_truth}{the true classification labels for the
  data}
}
\value{
  a vector containing the classifications of observations
  x[i]
}
\description{
  This function reports the true classification for a set
  of observations.
}

