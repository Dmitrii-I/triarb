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

    single_plot <- function(title="", ...) {
        par(mai=c(1, 1, 1.5, 0.6))
        par(bty='l')
        plot(x[, 1], x[, 4], main=title_2, las=1, type='h', xlab='', ylab='', ...)
    }

    title_1 <- paste("Profit in pips for roundtrip ", gsub('_', ' - ', names(profit)[3]))
    title_2 <- paste("Profit in pips for roundtrip ", gsub('_', ' - ', names(profit)[4]))
    
    if (separate==FALSE) {
        dev.new(width=11, height=10, xpos=100, ypos=100)
        par(mfrow=c(2, 1), bty='l')
        single_plot(title=title_1, ...)        
        single_plot(title=title_2, ...)        
    } else {
        dev.new(width=11, height=6, xpos=100, ypos=100)
        single_plot(title=title_1, ...)        
        dev.new(width=11, height=6, xpos=150, ypos=150)
        single_plot(title=title_2, ...)        
    }

}
