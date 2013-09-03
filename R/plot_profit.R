#' Bar plot of arbitrage profit for each tick
#' 
#' This function produces a bar plot of profit in pips for each tick. The bars 
#' are arranged on the x-axis according to their timestamp.
#' 
#' @param x 'TriArbProfitTable' object or a data frame with similar properties.
#' @param separate logical. If FALSE then one device window will show two plots of the two possible 
#' triangular arbitrage roundtrips. If TRUE, each plot will be opened in new window. 
#' @param ... further arguments for the generic 'plot' function. 
#' @export
#'
plot_profit <- function(x, separate=FALSE, ...) {
    single_plot <- function(col, title="", sub="", ...) {
        par(mai=c(1, 1, 1.5, 0.6), bty="l")
        # instead of passing x, thereby copying it unnecessarily, we just past column names
        plot(x$timestamp, x[, col], main=title, las=1, type='h', xlab='', ylab='', ...)
        mtext(build_subtitle(col), line=2, cex=0.9)
    }


    build_subtitle <- function(col) {
        sub <- paste("Ticks: ", format(nrow(x), big.mark=","), ". ", sep="")
        num_pos_profit <- length(x[x[, col] > 0, col])
        sub <- paste(sub, "Positive arbitrages: ", num_pos_profit, ". ", sep="")
        start_date <- format(x$timestamp[1], "%Y-%m-%d %H:%M:%S") 
        end_date <- format(rev(x$timestamp)[1], "%Y-%m-%d %H:%M:%S")
        period <- paste(start_date, " - ", end_date, sep="")
        sub <- paste(sub, "Period: ", period, ". ", sep="")
    }

    title_1 <- paste("Profit in pips for roundtrip ", gsub('_', ' - ', names(x)[3]))
    title_2 <- paste("Profit in pips for roundtrip ", gsub('_', ' - ', names(x)[4]))
    
    if (separate==FALSE) {
        dev.new(width=11, height=10, xpos=100, ypos=100)
        par(mfrow=c(2, 1), bty='l')
        single_plot(col=3, title=title_1, sub_1, ...)        
        single_plot(col=4, title=title_2, sub_2, ...)        
    } else {
        dev.new(width=11, height=6, xpos=100, ypos=100)
        single_plot(col=3, title=title_1, sub_1, ...)        
        dev.new(width=11, height=6, xpos=150, ypos=150)
        single_plot(col=4, title=title_2, sub_2, ...)        
    }

}
