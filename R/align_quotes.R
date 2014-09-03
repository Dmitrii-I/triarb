#' Align multiple currency rates quotes 
#' 
#' This function takes in bid and ask quotes of multiple currencies, and
#' returns a single data frame. In this data frame, each row has a timestamp
#' and bid and ask quote of each currency at that particular time.
#' 
#' Almost always, the ticks of individual currencies will not be synchronized.
#' This is normal as the bid and as quotes for currencies are updated 
#' individually by market makers.
#' 
#' To produce aligned (i.e. synchronized) ticks, the missing ticks will be
#' forward propagated using 'na.locf' (NA last observation carry forward) 
#' function from the 'zoo' package.
#' 
#' The following minimal example clarifies the quotes alignment function.
#' Suppose you have two ticks of EURUSD:
#' 2010-12-31 14:44:56.944 1.5001 1.5002
#' 2010-12-31 14:44:56.994 1.5002 1.5003
#' and two ticks of GBPUSD:
#' 2010-12-31 14:44:56.455 1.0011 1.0012
#' 2010-12-31 14:44:56.998 1.0011 1.0013
#' the result of 'align_quotes' will be:
#' 2010-12-31 14:44:56.944 1.5001 1.5002 1.0011 1.0012
#' 2010-12-31 14:44:56.994 1.5002 1.5003 1.0011 1.0012
#' 2010-12-31 14:44:56.998 1.5002 1.5003 1.0011 1.0013
#' Note: the first GBPUSD tick is dropped because there was no EURUSD quote
#' to carry forward. 
#' 
#' @param quotes_list A list of currency rates data frames. Each data frame has at least
#' a timestamp, a bid price, and an ask price columns.
#' @export
#' @return A data frame of aligned bid and ask quotes of multiple currencies.
#' 
#' @examples
#' data(AUDCAD, AUDCHF, CADCHF)
#' AUDCAD <- clean_quotes(AUDCAD)
#' AUDCHF <- clean_quotes(AUDCHF)
#' CADCHF <- clean_quotes(CADCHF)
#' combined <- align_quotes(list(AUDCAD, AUDCHF, CADCHF)) # this may take some time to complete

align_quotes <- function(quotes_list) {
    # Returns a single data frame with aligned bids and asks of multiple
    # instruments. The columns of this data frame are: timestamp, bid1, ask1, 
    # bid2, ask2, bid3, ask3, ...
    #
    # The input is a list of data frames of ticks, where each data frame
    # contains bid/ask prices of a single instrument.
    # 
    # The merging into one data frame is done according to this rule: 
    # if an instrument has a bid/ask price at a particular time while the
    # others do not, the last observed bid/ask of these will be carried
    # forward to fill the gap.  
    
    require(zoo, quietly = TRUE) # to load the na.locf function 
    
    headers <- "timestamp"
    
    timestamps <- NULL
    for (quotes in quotes_list) { 
        headers <- c(headers, names(quotes[,2:3]))
        timestamps <- append(timestamps, quotes[,1])
    } 
    # Some timestamps will have duplicates as several instruments may have
    # ticked at the same time. Remove these duplicates.
    timestamps <- unique(timestamps)
    
    quotes_aligned <- data.frame(timestamps)
    NAs <- data.frame(timestamps, NA, NA) # the skeleton of combined data frame
    
    for (quotes in quotes_list) {
        names(NAs) <- names(quotes) # so that we can use rbind()
        
        # pad with missing timestamps and NAs
        quotes <- rbind(quotes, NAs[!is.element(NAs[,1], quotes[,1]),]) 
        
        # sort by timestamp, increasing
        quotes <- quotes[order(quotes[,1]),]
        
        # Substitute all NAs except leading ones (na.rm = TRUE does not remove entire row, like we need)
        quotes[, 2:3] <- na.locf(quotes[, 2:3], na.rm = FALSE)  
        quotes_aligned <- merge(quotes_aligned, quotes, by = 1)
    }
    
    names(quotes_aligned) <- headers # set the headers
    
    # Remove rows that have at least one NA. Some NAs are still here because
    # there was no last observation that na.locf could use to substitute them.
    while (sum(is.na(quotes_aligned[1,])) > 0) 
        quotes_aligned <- quotes_aligned[-1,] 
    
    return(quotes_aligned)
}
