\name{align_quotes}
\alias{align_quotes}
\title{Align multiple currency rates quotes}
\usage{
  align_quotes(quotes_list)

  align_quotes(quotes_list)
}
\arguments{
  \item{quotes_list}{A list of currency rates data frames.
  Each data frame has at least a timestamp, a bid price,
  and an ask price columns.}

  \item{quotes_list}{A list of currency rates data frames.
  Each data frame has at least a timestamp, a bid price,
  and an ask price columns.}
}
\value{
  A data frame of aligned bid and ask quotes of multiple
  currencies.

  A data frame of aligned bid and ask quotes of multiple
  currencies.
}
\description{
  This function takes in bid and ask quotes of multiple
  currencies, and returns a single data frame. In this data
  frame, each row has a timestamp and bid and ask quote of
  each currency at that particular time.

  This function takes in bid and ask quotes of multiple
  currencies, and returns a single data frame. In this data
  frame, each row has a timestamp and bid and ask quote of
  each currency at that particular time.
}
\details{
  Almost always, the ticks of individual currencies will
  not be synchronized. This is normal as the bid and as
  quotes for currencies are updated individually by market
  makers.

  To produce aligned (i.e. synchronized) ticks, the missing
  ticks will be forward propagated using 'na.locf' (NA last
  observation carry forward) function from the 'zoo'
  package.

  The following minimal example clarifies the quotes
  alignment process. Suppose you have two ticks of EURUSD:
  2010-12-31 14:44:56.944 1.5001 1.5002 2010-12-31
  14:44:56.994 1.5002 1.5003 and two ticks of GBPUSD:
  2010-12-31 14:44:56.455 1.0011 1.0012 2010-12-31
  14:44:56.998 1.0011 1.0013 the result of 'align_quotes'
  will be: 2010-12-31 14:44:56.944 1.5001 1.5002 1.0011
  1.0012 2010-12-31 14:44:56.994 1.5002 1.5003 1.0011
  1.0012 2010-12-31 14:44:56.998 1.5002 1.5003 1.0011
  1.0013 Note: the first GBPUSD tick is dropped because
  there was no EURUSD quote to carry forward.

  Almost always, the ticks of individual currencies will
  not be synchronized. This is normal as the bid and as
  quotes for currencies are updated individually by market
  makers.

  To produce aligned (i.e. synchronized) ticks, the missing
  ticks will be forward propagated using 'na.locf' (NA last
  observation carry forward) function from the 'zoo'
  package.

  The following minimal example clarifies the quotes
  alignment function. Suppose you have two ticks of EURUSD:
  2010-12-31 14:44:56.944 1.5001 1.5002 2010-12-31
  14:44:56.994 1.5002 1.5003 and two ticks of GBPUSD:
  2010-12-31 14:44:56.455 1.0011 1.0012 2010-12-31
  14:44:56.998 1.0011 1.0013 the result of 'align_quotes'
  will be: 2010-12-31 14:44:56.944 1.5001 1.5002 1.0011
  1.0012 2010-12-31 14:44:56.994 1.5002 1.5003 1.0011
  1.0012 2010-12-31 14:44:56.998 1.5002 1.5003 1.0011
  1.0013 Note: the first GBPUSD tick is dropped because
  there was no EURUSD quote to carry forward.
}
\examples{
data(AUDCAD, AUDCHF, CADCHF)
AUDCAD <- clean_quotes(AUDCAD)
AUDCHF <- clean_quotes(AUDCHF)
CADCHF <- clean_quotes(CADCHF)
combined <- align(list(AUDCAD, AUDCHF, CADCHF)) # this may take some time to complete
data(AUDCAD, AUDCHF, CADCHF)
AUDCAD <- clean_quotes(AUDCAD)
AUDCHF <- clean_quotes(AUDCHF)
CADCHF <- clean_quotes(CADCHF)
combined <- align(list(AUDCAD, AUDCHF, CADCHF)) # this may take some time to complete
}

