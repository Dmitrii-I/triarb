\name{arb_plots}
\alias{arb_plots}
\title{Visualize triangular arbitrage opportunities}
\usage{
  arb_plots(rp)
}
\arguments{
  \item{rp}{A data frame returned by 'rate_prod' function}
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
arb_plots(rate_products)
}
\seealso{
  \code{\link{rate_prod}}
}

