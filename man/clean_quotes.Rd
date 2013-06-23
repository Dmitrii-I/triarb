\name{clean_quotes}
\alias{clean_quotes}
\title{A wrapper function to clean the currency rate quotes}
\usage{
  clean_quotes(x, verbose = TRUE)
}
\arguments{
  \item{x}{A data frame with first column being the
  date-time stamp of POSIXct class}

  \item{verbose}{logical TRUE or FALSE specifying whether
  function should output a summary as a side-effect.}
}
\value{
  A data frame with 3 columns: timestamp, first rate
  product, and second rate product
}
\description{
  Remove duplicate quotes, remove outliers, remove NAs, and
  arrange according to timestamp.
}
\examples{
data(AUDCAD)
AUDCAD <- clean_quotes(AUDCAD)
}
\seealso{
  \code{\link{arb_plots}}
}

