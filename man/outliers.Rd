\name{outliers}
\alias{outliers}
\title{Find outliers in currency rate quotes}
\usage{
  outliers(x, max_diff = 0.004)
}
\arguments{
  \item{x}{A numeric vector of currency rates.}

  \item{max_diff}{Maximum difference beyond which an
  element of x is marked as an outlier. Care should be
  taken when working with currency rates larger than 10
  (e.g. when the Japanese Yen is the quote currency) as
  these will require a larger maximum difference.}
}
\value{
  Returns a vector of indexes of outliers.
}
\description{
  Find outliers in a vector of currency rates. The outliers
  are detected with an algorithm based on medians.
}
\details{
  The algorithm to detect outliers is backward and forward
  looking. It employs medians to come up with a range of
  non-outlier values.
}
\examples{
data(forex_quotes)
outliers(forex_quotes[, 2])
}

