\name{lookahead_medians}
\alias{lookahead_medians}
\title{Compute lookahead medians}
\usage{
  lookahead_medians(x, window)
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
  Compute lookahead medians.
}
\examples{
data(AUDCAD)
medians <- lookahead_medians(AUDCAD[, 2])
head(medians)
}

