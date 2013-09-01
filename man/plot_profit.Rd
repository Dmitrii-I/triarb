\name{plot_profit}
\alias{plot_profit}
\title{Bar plot of arbitrage profit}
\usage{
  plot_profit(x, separate = FALSE, ...)
}
\arguments{
  \item{x}{A data frame or 'TriArbProfitTable' object.}

  \item{separate}{logical. If FALSE then one device window
  will show two plots of the two possible triangular
  arbitrage roundtrips. If TRUE, each plot will be opened
  in new window.}

  \item{...}{further arguments for the generic 'plot'
  function.}

  \item{x}{A data frame or 'TriArbProfitTable' object.}
}
\description{
  This function produces a bar plot of profit in pips, with
  bars sorted by timestamp.
}

