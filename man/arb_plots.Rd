\name{arb_plots}
\alias{arb_plots}
\title{Visualize triangular arbitrage opportunities}
\usage{
  arb_plots(x)
}
\arguments{
  \item{x}{A data frame returned by 'rprod'}
}
\value{
  Two plots, showing arbitrage opportunities for each of
  two possible roundtrips
}
\description{
  This function plots a bar chart of arbitrage
  opportunities found in a currencies triple.
}
\examples{
data(forex_quotes)
rate_products <- rprod(ticks, "EURUSD", "GBPUSD", "EURGBP")
arb_plots(rate_products)
}
\seealso{
  \code{\link{rprod}}
}

