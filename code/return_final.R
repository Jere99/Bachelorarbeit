# calculate the log-returns

library(quantmod)
# use 4 years to have enough data for the models to converge
startDate = as.Date("2018-01-01")
endDate = as.Date("2021-12-31")

## Bitcoin
btc_data <- getSymbols("BTC-USD", from = startDate, to = endDate, auto.assign=FALSE)
btc_price <- btc_data[, 6]
bitcoin_return <- dailyReturn(btc_price, type = "log")

## Ethereum
eth_data <- getSymbols("ETH-USD", from = startDate, to = endDate, auto.assign=FALSE)
eth_price <- eth_data[, 6]
ethereum_return <- dailyReturn(eth_price, type = "log")

## Tether
usdt_data <- getSymbols("USDT-USD", from = startDate, to = endDate, auto.assign=FALSE)
usdt_price <- usdt_data[, 6]
tether_return <- dailyReturn(usdt_price, type = "log")

## BNB
bnb_data <- getSymbols("BNB-USD", from = startDate, to = endDate, auto.assign=FALSE)
bnb_price <- bnb_data[, 6]
binance_return <- dailyReturn(bnb_price, type = "log")

## XRP (Ripple)
xrp_data <- getSymbols("XRP-USD", from = startDate, to = endDate, auto.assign=FALSE)
xrp_price <- xrp_data[, 6]
ripple_return <- dailyReturn(xrp_price, type = "log")

# ADA (Cardano)
ada_data <- getSymbols("ADA-USD", from = startDate, to = endDate, auto.assign=FALSE)
ada_price <- ada_data[, 6]
cardano_return <- dailyReturn(ada_price, type = "log")

# ADA (Dogecoin)
doge_data <- getSymbols("DoGE-USD", from = startDate, to = endDate, auto.assign=FALSE)
doge_price <- doge_data[, 6]
dogecoin_return <- dailyReturn(doge_price, type = "log")

