\name{align}
\alias{align}
\title{Align different currency rates quotes}
\usage{
  align(quotes_list)
}
\arguments{
  \item{quotes_list}{A list of currency rates data frames.
  Each data frame has at least a timestamp, a bid price,
  and an ask price columns.}
}
\value{
  A data frame of currency rates in wide format.
}
\description{
  The 'rate_prod' function requires currency rates quotes
  to be in the wide format. This function returns such data
  frame.
}
\examples{
data(AUDCAD, AUDCHF, CADCHF)
AUDCAD <- clean_quotes(AUDCAD)
AUDCHF <- clean_quotes(AUDCHF)
CADCHF <- clean_quotes(CADCHF)
combined <- align(list(AUDCAD, AUDCHF, CADCHF)) # this may take some time to complete
}

