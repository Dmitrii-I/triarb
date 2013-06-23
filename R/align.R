#' Align different currency rates quotes 
#' 
#' The 'rate_prod' function requires currency rates quotes to be in the wide format.
#' This function returns such data frame.
#' 
#' @param quotes_list A list of currency rates data frames. Each data frame has at least
#' a timestamp, a bid price, and an ask price columns.
#' @export
#' @return A data frame of currency rates in wide format. 
#' @examples
#' data(AUDCAD, AUDCHF, CADCHF)
#' AUDCAD <- clean_quotes(AUDCAD)
#' AUDCHF <- clean_quotes(AUDCHF)
#' CADCHF <- clean_quotes(CADCHF)
#' combined <- align(list(AUDCAD, AUDCHF, CADCHF)) # this may take some time to complete

align <- function(quotes_list) {
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
    
    quotes_combined <- data.frame(timestamps)
    NAs <- data.frame(timestamps, NA, NA) # the skeleton of combined data frame
    
    for (quotes in quotes_list) {
        names(NAs) <- names(quotes) # so that we can use rbind()
        
        # pad with missing timestamps and NAs
        quotes <- rbind(quotes, NAs[!is.element(NAs[,1], quotes[,1]),]) 
        
        # sort by timestamp, increasing
        quotes <- quotes[order(quotes[,1]),]
        
        # Substitute all NAs except leading ones (na.rm = TRUE does not remove entire row, like we need)
        quotes[, 2:3] <- na.locf(quotes[, 2:3], na.rm = FALSE)  
        quotes_combined <- merge(quotes_combined, quotes, by = 1)
    }
    
    names(quotes_combined) <- headers # set the headers
    
    # Remove rows that have at least one NA. Some NAs are still here because
    # there was no last observation that na.locf could use to substitute them.
    while (sum(is.na(quotes_combined[1,])) > 0) 
        quotes_combined <- quotes_combined[-1,] 
    
    return(quotes_combined)
}