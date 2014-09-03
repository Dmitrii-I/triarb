#' Compute lookback medians
#' 
#' Compute lookback medians.
#' 
#' @param x A numeric vector.
#' 
#' @param window The rolling window over which a median is computed. Should be odd.
#' @export
#' @return Returns a vector of medians having same length as 'x'. 
#' @examples
#' data(AUDCAD)
#' medians <- lookback_medians(AUDCAD[, 2])
#' head(medians)
#'

lookback_medians <- function (x, window=11) {
    if (window %% 2 == 0) window <- window + 1 
    
    # runmed() centers the window around current observation. 
    # since we are computing look back median from current observation,
    # we need to offset the center. compute this offset:
    offset <- (window - 1) / 2 + 1
    
    medians <- runmed(x, window)
    
    # rep() is used to copy into first (window - 1) lookback medians
    # the first properly computed lookbacl median
    lookback_medians <- c(rep(medians[offset], window - 1), 
                          medians[offset : (length(medians) - offset + 1)])
    
    return(lookback_medians)
}