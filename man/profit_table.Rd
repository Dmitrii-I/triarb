\name{profit_table}
\alias{profit_table}
\title{Triangular arbitrage profit table}
\usage{
  profit_table(x, curr_ids)

  profit_table(x, curr_ids)
}
\arguments{
  \item{x}{A data frame with 7 columns. Timestamp in the
  first column, and bid & ask quotes of three currencies in
  columns 2-7.}

  \item{curr_ids}{character vector with 3 elements. Each
  element is a currency pair identifier of 6 letters. First
  three letters refer to base currency, the last three to
  quote currency. The three-letter currency identifiers
  should preferrably use ISO 4217 standard (e.g. EUR for
  the euro, USD for the dollar, GBP for the pound).}

  \item{x}{A single data frame or a list of data frames.

  If a single data frame is provided, then it should be in
  the "aligned" format: 7 columns, with column 1 having the
  timestamp, and columns 2-7 the bid-ask quotes of three
  currencies.

  If a list of data frames is provided, it is assumed each
  data frame contains bid-ask quotes for a single currency.
  Alignment will then be done inside the 'profit_table'
  function using 'align_quotes' function.}

  \item{curr_ids}{character vector with 3 elements. Each
  element is a currency pair identifier of 6 letters. First
  three letters refer to base currency, the last three to
  quote currency. The three-letter currency identifiers
  should preferrably use ISO 4217 standard (e.g. EUR for
  the euro, USD for the dollar, GBP for the pound).}
}
\value{
  An object of the class `TriArbProfitTable`. This is just
  a fancy data frame with the columns: timestamp, age,
  profit of first roundtrip, and profit of second
  roundtrip. The age is the time difference with the next
  tick, i.e. how long the arbitrage opportunity (if any)
  was available.

  An object of the class `TriArbProfitTable`. This is just
  a fancy data frame with the columns: timestamp, age,
  profit of first roundtrip, and profit of second
  roundtrip. The age is the time difference in seconds,
  with the next tick, i.e. how long the arbitrage
  opportunity (if any) was available.
}
\description{
  This function computes triangular arbitrage profit for
  each tick occurring in either one of the three
  currencies.

  This function computes triangular arbitrage profit (in
  pips) for each tick occurring in either one of the three
  currencies.
}
\examples{
data(AUDCAD, AUDCHF, CADCHF)
AUDCAD <- clean_quotes(AUDCAD)
AUDCHF <- clean_quotes(AUDCHF)
CADCHF <- clean_quotes(CADCHF)
x <- align(list(AUDCAD, AUDCHF, CADCHF))
rp <- rate_prod(x, c("AUDCAD", "AUDCHF", "CADCHF"))
data(AUDCAD, AUDCHF, CADCHF)
AUDCAD <- clean_quotes(AUDCAD)
AUDCHF <- clean_quotes(AUDCHF)
CADCHF <- clean_quotes(CADCHF)
x <- align_quotes(list(AUDCAD, AUDCHF, CADCHF))
rp <- profit_table(x, c("AUDCAD", "AUDCHF", "CADCHF"))
}
\seealso{
  \code{\link{arb_plots}}

  \code{\link{arb_plots}}
}

