\name{clean_quotes}
\alias{clean_quotes}
\title{A wrapper function to clean the currency rate quotes}
\usage{
  clean_quotes(x)
}
\arguments{
  \item{x}{A data frame with first column being the
  date-time stamp of POSIXct class}
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
data(forex_quotes)
rate_products <- rate_prod(ticks, "EURUSD", "GBPUSD", "EURGBP")
head(rate_products)
}
\seealso{
  \code{\link{arb_plots}}
}

