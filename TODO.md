# To do

- Address scenario when first column of csv file does not contain valid timestamp.

- Prettify the info printed by `load_from_csv`

- `arb_plots` function should include a plot of arbitrage opportunity size vs 
duration in milliseconds

- Make sure the package works with JPY as quote currency (and other currencies).
When JPY is the quote currency, the exchange rate is almost always larger than 1.
This could lead to problems, if we for example coded in the package the pip to be equal
the exchange rate divided by 10,000 (which si the same as the fourth decimal of a rate). 
If JPY is the quote currency, then one pip is not the digit in the fourth decimal.
Thus, these issues, if there are any at all, need to be addressed.

- Add a report on hypothetical trading system(s). Such report could include statistics
such as number of trades, profit in pips, max. drawdown, and so forth. Examples
of such trading systems:
    - if there is a triangular arbitrage opportunity arbitrage opportunity, perform the three trades
at the market prices. This is quite a risky strategy though.
    - if there is a triangular arbitrage opportunity, perform the three trades at limit prices
    - and similar

- allow for bid- and ask-sizes to be incorporate into the analysis

- Add a summary function `arb_summary`.

- Optimize `align` function, if possible. It is slow now. Maybe re-write in C/C++.

- Write proper examples for the documentation. The scenarios of the examples need to be
executable using the included dataset.

- Add a vignette.

- Post to CRAN.

- Make sure the package can deal with several formats of forex quotes (what formats are there?).

- `clean_quotes` function documentation for return value is incorrect. It also needs
to describe properly that it is a wrapper functions and what other functions it calls.

- Add `very_verbose` argument to some functions.

- Make sure the DESCRIPTION file mentions all packages that this package depends on. 
Right now we need `zoo` package for our package.

- In the documentation, the example for `arb_plots` should says something descriptive
like `arb_plots(rate_products)` instead of `arb_plots(x)`

