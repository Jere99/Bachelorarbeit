######################################################################################################
### Stylized facts ###
######################################################################################################


options(java.parameters = "-Xmx8000m") # increase java memory to save large df to excel

library(quantmod)
library(tidyverse)
library(coinmarketcapr)
library(xlsx)
library(moments)
library(MTS) # archTest()
library(xts)
library(tseries)

#rm(list = ls())


### Overview over cryptocurrencies
coinmarketcapr::setup(api_key = "16f8a2df-16af-4a30-aec2-7f3edc46203b")
all_cryptos <- get_crypto_listings(currency = "USD", latest = TRUE)
all_cryptos <- all_cryptos[order(-all_cryptos$USD_market_cap),]
# View(all_cryptos)

crypto_df <- all_cryptos[c(1:25),] %>% 
  select(name, symbol, date_added, USD_market_cap, USD_market_cap_dominance) %>% 
  mutate(rank = c(1:25))


crypto_df[1:15]
cumsum(crypto_df$USD_market_cap_dominance)

crypto_df %>% 
  filter(date_added < "2018-01-01")

sum(crypto_df$USD_market_cap[c(1:25)])/sum(all_cryptos$USD_market_cap)


### downloedsa price data and calculate returns
# use 4 years to have enough data for the models to converge
startDate = as.Date("2018-01-01")
endDate = as.Date("2021-12-31")

## Bitcoin
btc_data <- getSymbols("BTC-USD", from = startDate, to = endDate, auto.assign=FALSE)
btc_price <- btc_data[, 6]
btc_return <- dailyReturn(btc_price, type = "log")

## Ethereum
eth_data <- getSymbols("ETH-USD", from = startDate, to = endDate, auto.assign=FALSE)
eth_price <- eth_data[, 6]
eth_return <- dailyReturn(eth_price, type = "log")

## Tether
usdt_data <- getSymbols("USDT-USD", from = startDate, to = endDate, auto.assign=FALSE)
usdt_price <- usdt_data[, 6]
usdt_return <- dailyReturn(usdt_price, type = "log")

## BNB
bnb_data <- getSymbols("BNB-USD", from = startDate, to = endDate, auto.assign=FALSE)
bnb_price <- bnb_data[, 6]
bnb_return <- dailyReturn(bnb_price, type = "log")

## XRP (Ripple)
xrp_data <- getSymbols("XRP-USD", from = startDate, to = endDate, auto.assign=FALSE)
xrp_price <- xrp_data[, 6]
xrp_return <- dailyReturn(xrp_price, type = "log")

# ADA (Cardano)
ada_data <- getSymbols("ADA-USD", from = startDate, to = endDate, auto.assign=FALSE)
ada_price <- ada_data[, 6]
ada_return <- dailyReturn(ada_price, type = "log")

# ADA (Dogecoin)
doge_data <- getSymbols("DoGE-USD", from = startDate, to = endDate, auto.assign=FALSE)
doge_price <- doge_data[, 6]
doge_return <- dailyReturn(doge_price, type = "log")


## Stable Coins
# USDC (USD Coin)
usdc_data <- getSymbols("USDC-USD", auto.assign=FALSE)
usdc_price <- usdc_data[, 6] %>% na.omit()
usdc_return <- dailyReturn(usdc_price, type = "log")
# BUSD (Binance USD)
busd_data <- getSymbols("BUSD-USD", auto.assign=FALSE)
busd_price <- busd_data[, 6] %>% na.omit()
busd_return <- dailyReturn(busd_price, type = "log")
# ust (USD Terra)
ust_data <- getSymbols("UST-USD", auto.assign=FALSE)
ust_price <- ust_data[, 6] %>% na.omit()
ust_return <- dailyReturn(ust_price, type = "log")


# SMI
smi_data <- getSymbols("^SSMI", from = startDate, to = endDate,  auto.assign=FALSE)
smi_price <- smi_data[, 6] %>% na.omit()
smi_return <- dailyReturn(smi_price, type = "log")

# CHF/USD
chfusd_data <- getSymbols("CHFUSD=X", from = startDate, to = endDate,  auto.assign=FALSE)
chfusd_price <- chfusd_data[, 6] %>% na.omit()
chfusd_return <- dailyReturn(chfusd_price, type = "log")

# Apple
apple_data <- getSymbols("AAPL", from = startDate, to = endDate, auto.assign=FALSE)
apple_price <- apple_data[, 6] %>% na.omit()
apple_return <- dailyReturn(apple_price, type = "log")



namevect <- c("SMI", "S&P500", "CHF/USD", "AAPL", "BTC", "ETH", "USDT", "BNB", "XRP", "ADA", "DOGE")
retvect <- list(smi_return, chfusd_return, apple_return, btc_return, eth_return,
                usdt_return, bnb_return, xrp_return, ada_return, doge_return)

####################################################################################################
## 1) absence of autocorrelation:
####################################################################################################

# par(mfrow = c(2,2))
acf(smi_return, main = "SMI - tägliche Rendite")
acf(apple_return, main = "Apple - tägliche Rendite")
acf(chfusd_return, main = "CHF/USD - tägliche Rendite")

# par(mfrow = c(3,3))
acf(btc_return, main = "Bitcoin - tägliche Rendite")
acf(eth_return, main = "Ethereum - tägliche Rendite")
acf(usdt_return, main = "Tether - tägliche Rendite")
acf(bnb_return, main = "BNB - tägliche Rendite")
acf(xrp_return, main = "Ripple (XRP) - tägliche Rendite")
acf(ada_return, main = "Cardano (ADA) - tägliche Rendite")
acf(doge_return, main = "Dogecoin - tägliche Rendite")


# stable coins
# par(mfrow = c(1,3))
acf(usdc_return, main = "USD Coin - tägliche Rendite")
acf(busd_return, main = "Binance USD - tägliche Rendite")
acf(ust_return, main = "TerraUSD - tägliche Rendite")


######################################################################################################
## 2) volatility clustering:
####################################################################################################

plot(smi_return, lwd = 1, main = "SMI - tägliche Rendite") # picture with 550 when exporting
plot(chfusd_return, lwd = 1, main = "CHF/USD - tägliche Rendite")
plot(apple_return, lwd = 1, main = "Apple - tägliche Rendite")

plot(btc_return, lwd = 1, main = "Bitcoin - tägliche Rendite")
plot(eth_return, lwd = 1, main = "Ethereum - tägliche Rendite")
plot(usdt_return, lwd = 1, main = "Tether - tägliche Rendite")
plot(bnb_return, lwd = 1, main = "BNB - tägliche Rendite")
plot(xrp_return, lwd = 1, main = "Riplle (XRP) - tägliche Rendite")
plot(ada_return, lwd = 1, main = "Cardano (ADA) - tägliche Rendite")
plot(doge_return, lwd = 1, main = "Dogecoin - tägliche Rendite")

######################################################################################################
## 3) Slow decay of autocorrleation in absolute returns
####################################################################################################

# par(mfrow = c(2,2))
acf(abs(smi_return), main = "SMI - \ntägliche absolute Rendite")
acf(abs(chfusd_return), main = "CHF/USD - \ntägliche absolute Rendite")
acf(abs(apple_return), main = "Apple - \ntägliche absolute Rendite")

# par(mfrow = c(3,3))
acf(abs(btc_return), main = "Bitcoin - \ntägliche absolute Rendite")
acf(abs(eth_return), main = "Ethereum - \ntägliche absolute Rendite")
acf(abs(usdt_return), main = "Tether - \ntägliche absolute Rendite")
acf(abs(bnb_return), main = "BNB - \ntägliche absolute Rendite")
acf(abs(xrp_return), main = "Ripple (XRP) - \ntägliche absolute Rendite")
acf(abs(ada_return), main = "Cardano (ADA) - \ntägliche absolute Rendite")
acf(abs(doge_return), main = "Dogecoin - \ntägliche absolute Rendite")

# par(mfrow = c(2,2))
acf(abs(smi_return)**2, main = "SMI - \ntägliche quadrierte Rendite")
acf(abs(chfusd_return)**2, main = "CHF/USD - \ntägliche quadrierte Rendite")
acf(abs(apple_return)**2, main = "Apple - \ntägliche quadrierte Rendite")

# par(mfrow = c(3,3))
acf(abs(btc_return)**2, main = "Bitcoin - \ntägliche quadrierte Rendite")
acf(abs(eth_return)**2, main = "Ethereum - \ntägliche quadrierte Rendite")
acf(abs(usdt_return)**2, main = "Tether - \ntägliche quadrierte Rendite")
acf(abs(bnb_return)**2, main = "BNB - \ntägliche quadrierte Rendite")
acf(abs(xrp_return)**2, main = "Ripple (XRP) - \ntägliche quadrierte Rendite")
acf(abs(ada_return)**2, main = "Cardano (ADA) - \ntägliche quadrierte Rendite")
acf(abs(doge_return)**2, main = "Dogecoin - \ntägliche quadrierte Rendite")


for(i in c(1:length(retvect))){
  print(namevect[i])
  print(Box.test((retvect[[i]])**2, lag = 20, type = c("Ljung-Box")))
  
}


retvect.2 <- list(btc_return, eth_return,
                  usdt_return, bnb_return, xrp_return, ada_return, doge_return)

ljung.box.test <- function(n, obs, fitdf = 0){
  lag = length(obs)
  STATISTIC <- n * (n + 2) * sum(1/seq.int(n - 1, n - lag) * 
                                   obs^2)
  PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
  print(PVAL)
}

for(i in retvect.2){
  acf.temp <- as.vector(acf(i**2)$acf[2:21])
  ljung.box.test(1461, acf.temp)
}


######################################################################################################
## 4) heavy tails:
############################

x<-seq(-4,+4,by=0.02)

# par(mfrow = c(2,2))
hist(smi_return, main = "SMI", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(smi_return),  sd = sd(smi_return)), add=TRUE, col = "red")


hist(snp_return, main = "S&p 500", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(snp_return), sd = sd(snp_return)), add=TRUE, col = "red")

hist(chfusd_return, main = "CHF/USD", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(chfusd_return), sd = sd(chfusd_return)), add=TRUE, col = "red")

hist(apple_return, main = "Apple", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(apple_return), sd = sd(apple_return)), add=TRUE, col = "red")


# par(mfrow = c(3,3))
hist(btc_return, main = "Bitcoin", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(btc_return), sd = sd(btc_return)), add=TRUE, col = "red")
hist(eth_return, main = "Ethereum", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(eth_return), sd = sd(eth_return)), add=TRUE, col = "red")
hist(usdt_return, main = "Tether", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(usdt_return), sd = sd(usdt_return)), add=TRUE, col = "red")
hist(bnb_return, main = "BNB", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(bnb_return), sd = sd(bnb_return)), add=TRUE, col = "red")
hist(xrp_return, main = "Ripple (XRP)", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(xrp_return), sd = sd(xrp_return)), add=TRUE, col = "red")
hist(ada_return, main = "Cardano (ADA)", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(ada_return), sd = sd(ada_return)), add=TRUE, col = "red")
hist(doge_return, main = "Dogecoin", freq = F, breaks = 100, 
     xlab = "tägliche Rendite", ylab = "relative Häufigkeit")
curve(dnorm(x, mean = mean(doge_return), sd = sd(doge_return)), add=TRUE, col = "red")


distr_facts <- function(data, symbol){
  data <- as.vector(data)
  summary <- data.frame(
    Symbol = symbol,
    Min = min(data),
    Median = median(data),
    Mean = mean(data),
    Max = max(data),
    Skewness = skewness(data),
    Kurtosis = kurtosis(data),
    JB = jarque.test(data)$statistic[[1]],
    'p-Value(JB)' = jarque.test(data)[["p.value"]]
  )
  return(summary)
}
fact_df <- distr_facts(btc_return, "BTC")

for (i in c(1:11)){
  temp_row <- distr_facts(retvect[[i]], namevect[i])
  fact_df <- rbind(fact_df, temp_row)
}

fact_df

######################################################################################################
## 5)	Leverage effect
######################################################################################################

lag <- 4
df.leverage <- data.frame(lags = c(1:4))
for(i in c(1:length(namevect))){
  vect_temp <- c()
  for(j in c(1:lag)){
    n <- length(retvect[[i]])
    retmax <- n-j
    absmin <- 1 + j
    cor_temp <- cor(retvect[[i]][1:retmax], abs(retvect[[i]][absmin:n]))
    vect_temp <- c(vect_temp, cor_temp)
  }
  df.leverage[,namevect[i]] <- vect_temp
}
df.leverage
