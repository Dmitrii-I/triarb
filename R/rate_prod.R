#' Computes currency rate products
#' 
#' The function returns the rate product of three currencies. A rate product
#' larger than one indicates arbitrage opportunity.
#' 
#' @param x A data frame with first column being the date-time stamp and 
#' columns 2 through 7 being the bid and ask quotes of the three currencies.
#'
#' @param rate1 character. A 6-letter currency rate identifier. First three
#' letters should identify the base currency. The last three should identify 
#' the quote currency. The three-letter currency identifier should preferrebly
#' adhere to the currenct codes as specified by ISO 4217 standard (e.g. EUR for
#' the euro, USD for the dollar, GBP for the pound). Any identifier is accepted
#' though as long as the first three letters refer to the base currency and 
#' last three to the quote currency. Therefore "FOOBAR" is accepted, and
#' it is up to the user to deal with the semantics. 
#' @param rate2 See 'rate1'
#' @param rate3 See 'rate1' 
#' @export
#' @seealso \code{\link{arb_plots}}
#' @return An object of the class `RateProduct`. This object is a  data frame 
#' with 3 columns: timestamp, first rate product, and second rate product.
#' @examples
#' data(AUDCAD, AUDCHF, CADCHF)
#' AUDCAD <- clean_quotes(AUDCAD)
#' AUDCHF <- clean_quotes(AUDCHF)
#' CADCHF <- clean_quotes(CADCHF)
#' x <- align(list(AUDCAD, AUDCHF, CADCHF))
#' rp <- rate_prod(x, "AUDCAD", "AUDCHF", "CADCHF")
#' 
#'
rate_prod <- function(x, rate1, rate2, rate3) {
    # to compute rate product, the rates must have proper form
    # get the base and quote currencies of the rates first:
    rates <- c(rate1, rate2, rate3)
    base_currs <- sapply(rates, function(x) substr(x, 1, 3))
    quote_currs <- sapply(rates, function(x) substr(x, 4, 6))
    # each of the three currencies must appear exactly twice,
    # either as base or quote, otherwise no triangular arbitrage possible
    if (!all(table(c(base_currs, quote_currs)) == 2)) 
        stop("No triangular arbitrage possible given the specified currencies")
    
    # computing the rate product
    if (quote_currs[1] == base_currs[2]) { # case AB, BC, XX
        if (base_currs[1] == quote_currs[3]) { 
            # case AB, BC, CA 
            rp1 <- x[, 2] * x[, 4] * x[, 6]
            rp2 <- 1 / x[, 7] / x[, 5] / x[, 3]
        } 
        else {  # case AB, BC, AC
                rp1 <- x[, 2] * x[, 4] / x[, 7]
                rp2 <- x[, 6] / x[, 5] / x[, 3]
        }
    } 
    
    else if (quote_currs[1] == quote_currs[2]) { # case AB, CB, XX
        if (base_currs[1] == base_currs[3]) {
            # case AB, CB, AC
            rp1 <- x[, 2] / x[, 5] / x[, 7]
            rp2 <- x[, 6] * x[, 4] / x[, 3]
        } 
        else {# case AB, CB, CA
            rp1 <- x[, 2] / x[, 5] * x[, 6]
            rp2 <- 1 / x[, 7] * x[, 4] / x[, 3]
        }
    } 
    
    else if (base_currs[1] == base_currs[2]) {# case AB, AC, XX
        if (quote_currs[1] == base_currs[3]) {
            # case AB, AC, BC
            rp1 <- x[, 2] * x[, 6] / x[, 5]
            rp2 <- x[, 4] / x[, 7] / x[, 3]
        } 
        else {
            # case AB, AC, CB
            rp1 <- x[, 2] / x[, 5] / x[, 7]
            rp2 <- x[, 6] * x[, 4] / x[, 3]
        }
    }
    else { # case AB, CA, XX
        if (quote_currs[1] == base_currs[3]) { 
            # case AB, CA, BC
            rp1 <- x[, 2] * x[, 4] * x[, 6]
            rp2 <- 1 / x[, 7] / x[, 5] / x[, 3]
        }
        else {# case AB, CA, CB
            rp1 <- x[, 2] / x[, 5] * x[, 6]
            rp2 <- 1 / x[, 7] * x[, 4] / x[, 3]    
        }     
    }
  
    result <- data.frame(x$timestamp, rp1, rp2)

    # construct column names of the returned data frame
    currs <- unique(c(base_currs, quote_currs))
    rp1_name <- paste(base_currs[1], "-", quote_currs[1], "-", sep="")
    rp1_name <- paste(rp1_name, currs[!(currs %in% c(base_currs[1], quote_currs[1]))], "-", base_currs[1], sep="")
    rp2_name <- paste(base_currs[1], "-", currs[!(currs %in% c(base_currs[1], quote_currs[1]))], sep="")
    rp2_name <- paste(rp2_name, "-", quote_currs[1], "-", base_currs[1], sep="")

    names(result) <- c("timestamp", rp1_name, rp2_name)

    class(result) <- "RateProduct"
    return(result)  
}
