(1) 
Add 'verbose' argument to function 'load_from_csv' so that when the function is executed
a short status message is printed to the console. I.e. 
    my_data <-load_from_csv("~/EURUSD.csv", verbose=TRUE)
should output either "Could not find file" or "Loaded 250435 quotes"

(2)
'arb_plots' function should include a plot of arbitrage opportunity size vs 
duration in milliseconds

(3) 
Make sure the package works with JPY as quote currency (and other currencies).
When JPY is the quote currency, the exchange rate is almost always larger than 1.
This could lead to problems, if we for example coded in the package the pip to be equal
the exchange rate divided by 10,000 (which si the same as the fourth decimal of a rate). 
If JPY is the quote currency, on pip WILL NOT equal to the fourth decimal. Thus, this issues, 
if there are any at all, need to be addressed in the package.

(4)
Add a report on hypothetical trading system(s). Such report could include statistics
such as number of trades, profit in pips, max. drawdown, and so forth. Examples
of such trading systems:
- if there is a triangular arbitrage opportunity arbitrage opportunity, perform the three trades
at the market prices. This is quite a risky strategy though.
- if there is a triangular arbitrage opportunity, perform the three trades at limit prices
- and similar

(5)
Add a summary function 'arb_summary'

(6)
Optimize "align" function, if possible. It is slow now. Maybe re-write in C/C++.

(7)
Write proper examples for the documentation. The scenarios of the examples need to be
executable using the included dataset.

(8)
Add a vignette

(9)
Post to CRAN

(10)
Make sure the package can deal with several formats of forex quotes. (what formats are there?)

(11)
'clean_quotes' function documentation for return value is incorrect. It also needs
to describe properly that it is a wrapper functions and what other
functions it calls

(10) 
Add 'very_verbose' argument to some functions.

(11)
Make sure the DESCRIPTION contains all the dependencies. Right now we need 'zoo' package 
for our package.

(12)
In the documentation, the example for 'arb_plots' should says something descriptive
like 'arb_plots(rate_products)' instead of 'arb_plots(x)'

