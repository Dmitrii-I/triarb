#' Find consecutive duplicated quotes
#' 
#' Find consecutive (when sorted by timestamp) duplicated quotes. 
#' #' 
#' @param x A data frame with first column being a timestamp of POSIXct class
#' 
#' @export
#' 
#' @return A vector of row indexes of duplicated quotes 
#' 
#' @examples
#' 
#' data(forex_quotes)
#' dups <- consec_dups(forex_quotes)
#' forex_quotes <- forex_quotes[-dups, ] # removes the duplicates 
#' 
consec_dups <- function(x) {
    # Returns indexes of two or more consecutive duplicate rows.
    # x is a numeric data frame
    
    x <- data.matrix(x) # coerce to numeric because apply() requires numeric
    # (apply coerces POSIXct into character)
    
    diffs <- apply(x, 2, diff) # to each column of x, apply diff() 
    diffs <- abs(diffs) 
    diffs <- apply(diffs, 1, sum)
    
    indexes <- which(diffs == 0)
    return(indexes)
}