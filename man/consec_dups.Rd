\name{consec_dups}
\alias{consec_dups}
\title{Find consecutive duplicated quotes}
\usage{
  consec_dups(x)
}
\arguments{
  \item{x}{A data frame with first column being a timestamp
  of POSIXct class}
}
\value{
  A vector of row indexes of duplicated quotes
}
\description{
  Find consecutive (when sorted by timestamp) duplicated
  quotes. #'
}
\examples{
data(forex_quotes)
dups <- consec_dups(forex_quotes)
forex_quotes <- forex_quotes[-dups, ] # removes the duplicates
}

