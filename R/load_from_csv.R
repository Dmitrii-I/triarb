#' Loads quotes from a csv file
#' 
#' This function is useful for loading quotes in a csv file into a data frame.
#' 
#' @param csv_file_path Full path to a csv file containing the currency rate quotes.
#' 
#' @param header logical TRUE or FALSE, indicating whether the csv file has a header in the first row.
#' 
#' @export
#' 
#' @return A data frame with POSIXct class timestamp in the first column. 
#' 
#' @examples
#' # quotes <- load_from_csv("~/EURUSD.csv", header=TRUE)

load_from_csv <- function(csv_file_path, header=NULL) {
    
    if (is.null(header) || header != TRUE || header != FALSE) header <- has_header(csv_file_path)

    quotes <- read.csv(csv_file_path, header)
    
    # convert character timestamp into POSIXct timestamp
    quotes[, 1] <- as.POSIXct(quotes[, 1])
    
    return(quotes)
}