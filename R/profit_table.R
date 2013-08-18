#' Triangular arbitrage profit table
#' 
#' This function computes triangular arbitrage profit (in pips) 
#' for each tick occurring in either one of the three currencies.
#' 
#' @param x A single data frame or a list of data frames. 
#' 
#' If a single data frame is provided, then it should be in the "aligned" 
#' format: 7 columns, with column 1 having the timestamp, and columns 2-7 
#' the bid-ask quotes of three currencies.  
#'
#' If a list of data frames is provided, it is assumed each data frame
#' contains bid-ask quotes for a single currency. Alignment will then be done
#' inside the 'profit_table' function using 'align_quotes' function.
#' 
#' @param curr_ids character vector with 3 elements. Each element is a 
#' currency pair identifier of 6 letters. First three letters refer to base
#' currency, the last three to quote currency. The three-letter currency 
#' identifiers should preferrably use ISO 4217 standard (e.g. EUR for
#' the euro, USD for the dollar, GBP for the pound). 
#'
#' @export
#'
#' @return An object of the class `TriArbProfitTable`. This is just a fancy 
#' data frame with the columns: timestamp, age, profit of first roundtrip, and
#' profit of second roundtrip. The age is the time in seconds until next 
#' tick, i.e. how long the arbitrage opportunity (if any) was available. 
#'
#' @examples
#' data(AUDCAD, AUDCHF, CADCHF)
#' AUDCAD <- clean_quotes(AUDCAD)
#' AUDCHF <- clean_quotes(AUDCHF)
#' CADCHF <- clean_quotes(CADCHF)
#' x <- align_quotes(list(AUDCAD, AUDCHF, CADCHF))
#' rp <- profit_table(x, c("AUDCAD", "AUDCHF", "CADCHF"))

profit_table <- function(x, curr_ids) {
    pips_mult = 10000L # will be replaced by a function in future
    if (class(x) == 'list') x <- align_quotes(x)

    # get the base and quote currencies of the rates first:
    base_currs <- sapply(curr_ids, function(z) substr(z, 1, 3))
    quote_currs <- sapply(curr_ids, function(z) substr(z, 4, 6))
    # each of the three currencies must appear exactly twice,
    # either as base or quote, otherwise no triangular arbitrage possible
    if (!all(table(c(base_currs, quote_currs)) == 2)) 
        stop("No triangular arbitrage possible with specified currencies")
    
    # given 3 currencies A, B and C, the formula for profit depends on
    # where each of these currencies appear: either as base or quotes currency
    if (quote_currs[1] == base_currs[2]) { # case AB, BC, XX
        if (base_currs[1] == quote_currs[3]) { 
            # case AB, BC, CA 
            profit1 <- x[, 2] * x[, 4] * x[, 6] -1
            profit2 <- 1 / x[, 7] / x[, 5] / x[, 3] - 1
        } 
        else {  # case AB, BC, AC
                profit1 <- x[, 2] * x[, 4] / x[, 7] - 1
                profit2 <- x[, 6] / x[, 5] / x[, 3] - 1
        }
    } 
    
    else if (quote_currs[1] == quote_currs[2]) { # case AB, CB, XX
        if (base_currs[1] == base_currs[3]) {
            # case AB, CB, AC
            profit1 <- x[, 2] / x[, 5] / x[, 7] - 1
            profit2 <- x[, 6] * x[, 4] / x[, 3] - 1
        } 
        else {# case AB, CB, CA
            profit1 <- x[, 2] / x[, 5] * x[, 6] - 1
            profit2 <- 1 / x[, 7] * x[, 4] / x[, 3] - 1
        }
    } 
    
    else if (base_currs[1] == base_currs[2]) {# case AB, AC, XX
        if (quote_currs[1] == base_currs[3]) {
            # case AB, AC, BC
            profit1 <- x[, 2] * x[, 6] / x[, 5] - 1
            profit2 <- x[, 4] / x[, 7] / x[, 3] - 1
        } 
        else {
            # case AB, AC, CB
            profit1 <- x[, 2] / x[, 5] / x[, 7] - 1
            profit2 <- x[, 6] * x[, 4] / x[, 3] - 1
        }
    }
    else { # case AB, CA, XX
        if (quote_currs[1] == base_currs[3]) { 
            # case AB, CA, BC
            profit1 <- x[, 2] * x[, 4] * x[, 6] - 1
            profit2 <- 1 / x[, 7] / x[, 5] / x[, 3] - 1
        }
        else {# case AB, CA, CB
            profit1 <- x[, 2] / x[, 5] * x[, 6] - 1
            profit2 <- 1 / x[, 7] * x[, 4] / x[, 3] - 1    
        }     
    }
  
    age <- c(difftime(x$timestamp[-1], x$timestamp[-length(x$timestamp)], units='secs'), 0)
    result <- data.frame(x$timestamp, age, profit1*pips_mult, profit2*pips_mult)

    # construct column names
    currs <- unique(c(base_currs, quote_currs))
    colname1 <- paste(base_currs[1], quote_currs[1], sep="_")
    colname1 <- paste(colname1, currs[!(currs %in% c(base_currs[1], quote_currs[1]))], base_currs[1], sep="_")
    colname2 <- paste(base_currs[1], currs[!(currs %in% c(base_currs[1], quote_currs[1]))], sep="_")
    colname2 <- paste(colname2, quote_currs[1], base_currs[1], sep="_")

    names(result) <- c("timestamp", "age_in_secs", colname1, colname2)

    class(result) <- c('TriArbProfitTable', 'data.frame') # inherits from data.frame too
    return(result)  
}
