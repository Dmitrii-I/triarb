\name{lookback_medians}
\alias{lookback_medians}
\title{Compute lookback medians}
\usage{
  lookback_medians(x, window)
}
\arguments{
  \item{x}{A numeric vector.}

  \item{window}{The rolling window over which a median is
  computed. Should be odd.}
}
\value{
  Returns a vector of medians having same length as 'x'.
}
\description{
  Compute lookback medians.
}
\examples{
data(AUDCAD)
medians <- lookback_medians(AUDCAD[, 2])
head(medians)
}

