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

  \item{rate1}{character. A 6-letter currency rate
  identifier. First three letters should identify the base
  currency. The last three should identify the quote
  currency. The three-letter currency identifier should
  preferrebly adhere to the currenct codes as specified by
  ISO 4217 standard (e.g. EUR for the euro, USD for the
  dollar, GBP for the pound). Any identifier is accepted
  though as long as the first three letters refer to the
  base currency and last three to the quote currency.
  Therefore "FOOBAR" is accepted, and it is up to the user
  to deal with the semantics.}

  \item{rate2}{See 'rate1'}

  \item{rate3}{See 'rate1'}
}
\value{
  An object of the class `RateProduct`. This object is a
  data frame with 3 columns: timestamp, first rate product,
  and second rate product.
}
\description{
  The function returns the rate product of three
  currencies. A rate product larger than one indicates
  arbitrage opportunity.
}
\examples{
data(AUDCAD, AUDCHF, CADCHF)
AUDCAD <- clean_quotes(AUDCAD)
AUDCHF <- clean_quotes(AUDCHF)
CADCHF <- clean_quotes(CADCHF)
x <- align(list(AUDCAD, AUDCHF, CADCHF))
rp <- rate_prod(x, "AUDCAD", "AUDCHF", "CADCHF")
}
\seealso{
  \code{\link{arb_plots}}
}

