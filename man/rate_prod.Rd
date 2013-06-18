\name{rate_prod}
\alias{rate_prod}
\title{Computes currency rate products}
\usage{
  rate_prod(x, rate1, rate2, rate3)
}
\arguments{
  \item{x}{A data frame with first column being the
  date-time stamp and columns 2 through 7 being the bid and
  ask quotes of the three currencies.}

  \item{rate1}{Specifies the first currency rate in the
  form of a 6-character currency rate identifier. For
  example, the eurodollar rate is specified by "EURUSD",
  the pound-dollar rate by "GBPUSD", and similarly for
  other currencies. Any identifier is accepted as long as
  the first three characters refer to the base currency and
  last three to the quote currency. Therefore "FOOBAR" is
  accepted, and it is up to the user to deal with
  semantics.}

  \item{rate2}{See 'rate1'}

  \item{rate3}{See 'rate1'}
}
\value{
  A data frame with 3 columns: timestamp, first rate
  product, and second rate product
}
\description{
  The function returns the rate product of three
  currencies. Rate products larger than one signal
  arbitrage opportunity.
}
\examples{
data(forex_quotes)
rate_products <- rate_prod(ticks, "EURUSD", "GBPUSD", "EURGBP")
head(rate_products)
}
\seealso{
  \code{\link{arb_plots}}
}

