\name{plot_profit}
\alias{plot_profit}
\title{Bar plot of arbitrage profit for each tick}
\usage{
  plot_profit(x, separate_plots = FALSE, ...)
}
\arguments{
  \item{x}{'TriArbProfitTable' object or a data frame with
  4 columns: timestamp, difference between timestamp,
  profit of first roundtrip, and profit of second
  roundtrip.}

  \item{separate}{logical. If FALSE then one device window
  will show two plots of the two possible triangular
  arbitrage roundtrips. If TRUE, each plot will be opened
  in new window.}

  \item{...}{further arguments for the generic 'plot'
  function.}
}
\description{
  This function produces a bar plots of triangular
  arbitrage profit in pips for each tick. The bars are
  arranged on the x-axis according to their timestamp.
}

