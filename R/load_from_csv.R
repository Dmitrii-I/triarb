#' Load quotes from a csv file
#' 
#' Load quotes stored in a csv file into a data frame. The csv file must have a specific format:
#' the first three values in each row should be the datetime-stamp, the bid price, and the ask price,
#' respectively.
#' 
#' @param csv_file_path Full path to a csv file containing the currency rate quotes.
#' @param header logical TRUE or FALSE, indicating whether the csv file has a header in the first row.
#' @param verbose logical TRUE or FALSE. If TRUE, shows a summary of the loaded quotes.
#' 
#' @export
#' 
#' @return A data frame with POSIXct class timestamp in the first column. 
#' 
#' @examples
#' # quotes <- load_from_csv("~/EURUSD.csv", header=TRUE, verbose=TRUE)

load_from_csv <- function(csv_file_path, header=NULL, verbose=TRUE) {
    if (!file.exists(csv_file_path)) 
        stop("Cannot load data. File ", csv_file_path, " does not exist.")

    # If header argument not set, determine if file has a header
    if (is.null(header) || header != TRUE || header != FALSE) header <- has_header(csv_file_path)

    quotes <- read.csv(csv_file_path, header)
    quotes[, 1] <- as.POSIXct(quotes[, 1]) # convert char timestamp to POSIXct timestamp
   
    if (verbose == TRUE) {
        cat("Loaded", format(nrow(quotes), big.mark=","), "quotes from", csv_file_path, "\n")
        cat("First quote:\n")
        print(quotes[1, ])
        cat("Last quote:\n") 
        print(quotes[nrow(quotes), ])
    }
 
    return(quotes)
}
