#' Visualize triangular arbitrage opportunities
#' 
#' This function plots a bar chart of arbitrage opportunities found in a currencies triple. 
#' @param x A data frame returned by 'rate_prod'
#' @export
#'
#'
#'@seealso \code{\link{rate_prod}}
#' @return Two plots, showing arbitrage opportunities for each of two possible roundtrips
#' @examples
#' arb_plots(rate_products)
#' 
arb_plots <- function(x) {
    par(mfrow=c(2, 1))
	pips_mult <- 10000L # multiplier to obtain pips from currency rate

	x_axis_lab <- "Arbitrage opportunities (sorted by magnitude)"
	y_axis_lab <- "Profit in pips"
    
	# arbitrage opportunities for the first roundtrip
    arb1 <- x[x[, 2] > 1, c(1, 2)]
	names(arb1) <- c("timestamp", "profit")
	arb1$profit <- (arb1$profit - 1) * pips_mult

	# build plot title
	n <- nrow(arb1)
	roundtrip <- names(x)[2]
	title <- (paste(n, " arbitrage opportunities in ", roundtrip, " roundtrip")) 
	
    barplot(sort(arb1$profit, decreasing=TRUE), xlab=x_axis_lab, ylab=y_axis_lab, main=title)
   
 
	# arbitrage opportunities for the second roundtrip
    arb2 <- x[x[, 3] > 1, c(1, 2)]
	names(arb2) <- c("timestamp", "profit")
	arb2$profit <- (arb2$profit - 1) * pips_mult

	# build plot title
	n <- nrow(arb2)
	roundtrip <- names(x)[3]
	title <- (paste(n, " arbitrage opportunities in ", roundtrip, " roundtrip")) 
	
    barplot(sort(arb1$profit, decreasing=TRUE), xlab=x_axis_lab, ylab=y_axis_lab, main=title)
    
}
