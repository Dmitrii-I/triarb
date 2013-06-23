#' Find outliers in currency rate quotes
#' 
#' Find outliers in a vector of currency rates. The outliers are detected with 
#' an algorithm based on medians.
#' 
#' @param x A numeric vector of currency rates.
#' @param max_diff Maximum difference beyond which an element of x is marked as an outlier. 
#' Care should be taken when working with currency rates larger than 10 (e.g. when the Japanese Yen is the quote currency) as
#' these will require a larger maximum difference.
#' @export
#' @return Returns a vector of indexes of outliers. 
#' @details
#' The algorithm to detect outliers is backward and forward looking. It employs medians 
#' to come up with a range of non-outlier values.
#' @examples
#' data(AUDCAD)
#' outliers(AUDCAD[, 2])
#'

outliers <- function (x, max_diff=0.0040) {
    n <- length(x) 
    
    # Should be odd. Set as low as possible, while still making it 
    # impossible for the median to be an outlier itself.
    window <- 11 
    
    # compute lookback and lookahead medians
    lookback_medians <- lookback_medians(x, window)
    lookahead_medians <- lookahead_medians(x, window) 
    
    # Compute the difference of suspected x and the medians 
    diffs_lookback_medians <- abs(x - lookback_medians) 
    diffs_lookahead_medians <- abs(x - lookahead_medians) 
    
    # Identify outliers and store their indexes 
    outliers <- which((diffs_lookback_medians > max_diff) & 
                          (diffs_lookahead_medians > max_diff))
    
    return(outliers)
}