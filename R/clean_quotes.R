#' A wrapper function to clean the currency rate quotes
#' 
#' Remove duplicate quotes, remove outliers, remove NAs, and arrange according to timestamp.
#' 
#' @param x A data frame with first column being the date-time stamp of POSIXct class
#' 
#' @export
#' @seealso \code{\link{arb_plots}}
#' @return A data frame with 3 columns: timestamp, first rate product, and second rate product
#' @examples
#' data(forex_quotes)
#' rate_products <- rate_prod(ticks, "EURUSD", "GBPUSD", "EURGBP")
#' head(rate_products)
#' 
#'
clean_quotes <- function(x) {
    
    # remove NAs
    x <- na.omit(x)
    
    # order according to timestamp
    x <- x[order(x[, 1], ), ]
        
    # remove duplicates
    x <- x[-consec_dups(x), ]
    
    # remove outliers
    
    
    return(x)  
    
}