#Bitcoin - GBTC Discount Arbitrage

library(quantmod)
library(PerformanceAnalytics)
library(jsonlite)


tickers = c("GBTC", "BTC-USD")

tickers |> 
	getSymbols(from = "2019-01-15", to = "2022-12-13")

prices = merge(GBTC[,6],`BTC-USD`[,4]) |>
	na.omit()

asset_rets = prices |>
	Return.calculate()

wt = c(.5,.5)

strategy_return = asset_rets |> 
	Return.portfolio(wt, rebalance_on = "days")




