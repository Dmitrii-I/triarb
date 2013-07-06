\name{load_from_csv}
\alias{load_from_csv}
\title{Loads quotes from a csv file}
\usage{
  load_from_csv(csv_file_path, header = NULL)
}
\arguments{
  \item{csv_file_path}{Full path to a csv file containing
  the currency rate quotes.}

  \item{header}{logical TRUE or FALSE, indicating whether
  the csv file has a header in the first row.}
}
\value{
  A data frame with POSIXct class timestamp in the first
  column.
}
\description{
  This function is useful for loading quotes in a csv file
  into a data frame.
}
\examples{
# quotes <- load_from_csv("~/EURUSD.csv", header=TRUE)
}

