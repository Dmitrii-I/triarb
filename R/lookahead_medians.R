#' Compute lookahead medians
#' 
#' Compute lookahead medians.
#' 
#' @param x A numeric vector.
#' 
#' @param window The rolling window over which a median is computed. Should be odd.
#' @export
#' @return Returns a vector of medians having same length as 'x'. 
#' @examples
#' data(forex_quotes)
#' medians <- lookahead_medians(forex_quotes[, 2])
#' head(medians)
#'

lookahead_medians <- function (x, window) {
    # window should be odd (runmed() requires that), if not, make it odd
    if (window %% 2 == 0) window <- window + 1 
    
    # runmed() centers the window around current observation. 
    # since we are computing look ahead median from current observation,
    # we need to offset the center. compute this offset:
    offset <- (window - 1) / 2 + 1
    
    medians <- runmed(x, window)
    
    # rep() is used to copy last properly computes lookahead median into last
    # (window - 1) lookahead medians
    lookahead_medians <- c(medians[offset : (length(medians) - offset + 1)],
                           rep(medians[length(medians) - offset + 1], window -1))
    
    return(lookahead_medians)
}