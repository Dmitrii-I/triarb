#' A wrapper function to clean the currency rate quotes
#' 
#' Remove duplicate quotes, remove outliers, remove NAs, and arrange according to timestamp.
#' 
#' @param x A data frame with three columns: timestamp, bid, ask. 
#' Second and third columns are bids and asks.
#' @param verbose logical TRUE or FALSE specifying whether function should output a summary
#' as a side-effect.
#' @export
#' @seealso \code{\link{arb_plots}}
#' @return A data frame with 3 columns: timestamp, first rate product, and second rate product
#' @examples
#' data(AUDCAD)
#' AUDCAD <- clean_quotes(AUDCAD)
#' 
#'
clean_quotes <- function(x, verbose=TRUE) {
    n <- nrow(x)
    # remove NAs
    x <- na.omit(x)
    
    # order according to timestamp
    x <- x[order(x[, 1]), ]
        
    # remove duplicates
    x <- x[-consec_dups(x), ]
    
    # remove outliers
    ind <- outliers(x[, 2]) # bid quotes are in second column
    if (length(ind)>0) x <- x[-ind, ]
    
    ind2 <- outliers(x[, 3]) # ask quotes in third column
    if (length(ind2)>0) x <- x[-ind2, ]
        
    if (verbose==TRUE) cat("Removed", n - nrow(x), "rows.\n")
    
    return(x)  
    
}
