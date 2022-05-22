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


coinmarketcapr::setup(api_key = "16f8a2df-16af-4a30-aec2-7f3edc46203b")
all_cryptos <- get_crypto_listings(currency = "USD", latest = TRUE)
all_cryptos <- all_cryptos[order(-all_cryptos$USD_market_cap),]
# View(all_cryptos)


crypto_df <- all_cryptos[c(1:25),] %>% 
  select(name, symbol, date_added, USD_market_cap, USD_market_cap_dominance) %>% 
  mutate(rank = c(1:25))



# path <- "~/R/projects/Bachelorarbeit/results/Crypto_facts.xlsx"
# sheet_name = format(Sys.time(), "%Y-%b-%d")
# 
# 
# if(sheet_name %in% names(getSheets(loadWorkbook(path)))){
#   wb <- loadWorkbook(path)
#   removeSheet(wb, sheetName = sheet_name)
#   saveWorkbook(wb, path)
# }
# 
# write.xlsx(crypto_df, path, sheetName = sheet_name, 
#            col.names = TRUE, row.names = FALSE, append = TRUE)



crypto_df[1:15]
cumsum(crypto_df$USD_market_cap_dominance)

crypto_df %>% 
  filter(date_added < "2018-01-01")

sum(crypto_df$USD_market_cap[c(1:25)])/sum(all_cryptos$USD_market_cap)


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


### Stable Coins
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


# S&P 500
snp_data <- getSymbols("^GSPC", from = startDate, to = endDate,   auto.assign=FALSE)
snp_price <- snp_data[, 6] %>% na.omit()
snp_return <- dailyReturn(snp_price, type = "log")

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

# CHF/USD
usdchf_price <- 1/ chfusd_price
usdchf_return <- dailyReturn(usdchf_price, type = "log")


# data_vect <- list(btc_data, eth_data, usdt_data, bnb_data, xrp_data, ada_data, snp_data, smi_data)
# name_vect <- c("Bitcoin", "Ethereum", "Tether", "Binance Coin", "Ripple", 
#                "Cardano", "S&P500", "SMI")
# 
# for (i in c(1:length(name_vect))){
#   
#   path <- "~/R/projects/Bachelorarbeit/results/Crypto_data.xlsx"
#   sheet_name = name_vect[i]
#   
#   
#   if(sheet_name %in% names(getSheets(loadWorkbook(path)))){
#     wb <- loadWorkbook(path)
#     removeSheet(wb, sheetName = sheet_name)
#     saveWorkbook(wb, path)
#   }
#   
#   write.xlsx(data_vect[i], path, sheetName = sheet_name, 
#              col.names = TRUE, row.names = TRUE, append = TRUE)
#   
# }

namevect <- c("SMI", "S&P500", "CHF/USD", "AAPL", "BTC", "ETH", "USDT", "BNB", "XRP", "ADA", "DOGE")
retvect <- list(smi_return, snp_return, chfusd_return, apple_return, btc_return, eth_return,
                usdt_return, bnb_return, xrp_return, ada_return, doge_return)
pricevect <- list(smi_price, snp_price, chfusd_price, apple_price, btc_price, eth_price,
                usdt_price, bnb_price, xrp_price, ada_price, doge_price)

######################################################################################################
## 0) random walk

# par(mfrow = c(2,2))
plot.xts(smi_price, main = "SMI - Preis in (USD)", lwd = 1)
plot.xts(snp_price, main = "S&p 500 - Preis in (USD)", lwd = 1)
plot.xts(apple_price, main = "Apple - Preis in (USD)", lwd = 1)
plot.xts(chfusd_price, main = "CHF/USD - Preis in (USD)", lwd = 1)

# par(mfrow = c(3,3))
plot.xts(btc_price, main = "Bitcoin - Preis in (USD)", lwd = 1)
plot.xts(eth_price, main = "Ethereum - Preis in (USD)", lwd = 1)
plot.xts(usdt_price, main = "Tether - Preis in (USD)", lwd = 1)
plot.xts(bnb_price, main = "BNB - Preis in (USD)", lwd = 1)
plot.xts(xrp_price, main = "Ripple - Preis in (USD)", lwd = 1)
plot.xts(ada_price, main = "Cardano - Preis in (USD)", lwd = 1)
plot.xts(doge_price, main = "Dogecoin - Preis in (USD)", lwd = 1)


# par(mfrow = c(2,2))
acf(smi_price, main = "SMI")
acf(snp_price, main = "S&p 500")
acf(apple_price, main = "Apple")
acf(chfusd_price, main = "CHF/USD")

# par(mfrow = c(3,3))
acf(btc_price, main = "Bitcoin")
acf(eth_price, main = "Ethereum")
acf(usdt_price, main = "Tether")
acf(bnb_price, main = "BNB")
acf(xrp_price, main = "Ripple (XRP)")
acf(ada_price, main = "Cardano (ADA)")
acf(doge_price, main = "Dogecoin")

for(i in pricevect){
  print(adf.test(i))
}


####################################################################################################
## 1) absence of autocorrelation:
####################################################################################################

# par(mfrow = c(2,2))
acf(smi_return, main = "SMI - tägliche Rendite")
# acf(snp_return, main = "S&P 500 - tägliche Rendite")
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

for(i in retvect){
  print(adf.test(i))
}

# stable coins
# par(mfrow = c(1,3))
acf(usdc_return, main = "USD Coin - tägliche Rendite")
acf(busd_return, main = "Binance USD - tägliche Rendite")
acf(ust_return, main = "TerraUSD - tägliche Rendite")


# par(mfrow = c(3,3))
pacf(btc_return, main = "Bitcoin") # 1 slightly
pacf(eth_return, main = "Ethereum") # 1, 2, 6, 7
pacf(usdt_return, main = "Tether") # a lot --> MA(1)
pacf(bnb_return, main = "BNB") # 2 slighlty, and 6
pacf(xrp_return, main = "Ripple (XRP)") # 12
pacf(ada_return, main = "Cardano (ADA)") # 26 slightly
pacf(doge_return, main = "Dogecoin") # 2, 3 (a lot), 15, 26, 27

chisq_name <- "Coin Name"
n <- length(btc_return$daily.returns)
chisq.value <- n*sum(pacf(bnb_return, main = chisq_name)[["acf"]]**2)
pchisq(chisq.value, 20, lower.tail = F)

for(i in retvect){
  chisq.value <- n*sum(pacf(i, main = chisq_name)[["acf"]]**2)
  print(pchisq(chisq.value, 20, lower.tail = F))
}

par(mfrow = c(1,1))

for(i in c(1:length(retvect))){
  print(namevect[i])
  print(Box.test(retvect[[i]], lag = 20, type = c("Ljung-Box")))
}

test2 <- arima(doge_return, order = c(1, 0, 0))
res <- test2$residuals

######################################################################################################
## 6) volatility clustering:
####################################################################################################
plot(as.vector(smi_return), type = "l")
plot(abs(smi_return))
plot(smi_return**2)
plot(smi_return, lwd = 1, main = "SMI - tägliche Rendite")

plot(smi_return, lwd = 1, main = "SMI - tägliche Rendite") # picture with 550 when exporting
plot(snp_return, lwd = 1, main = "S&P 500 - tägliche Rendite")
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
## 7) Slow decay of autocorrleation in absolute returns
####################################################################################################

# par(mfrow = c(2,2))
acf(abs(smi_return), main = "SMI - \ntägliche absolute Rendite")
acf(abs(snp_return), main = "S&p 500 - \ntägliche absolute Rendite")
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
acf(abs(snp_return)**2, main = "S&p 500 - \ntägliche quadrierte Rendite")
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



namevect <- c("SMI", "S&P500", "CHF/USD", "AAPL", "BTC", "ETH", "USDT", "BNB", "XRP", "ADA", "DOGE")
retvect <- list(smi_return, snp_return, chfusd_return, apple_return, btc_return, eth_return,
                usdt_return, bnb_return, xrp_return, ada_return, doge_return)


x <- archTest(as.vector(btc_return))


for(i in c(1:length(namevect))){
  print(archTest(as.vector(retvect[[i]])))
}
archTest(rnorm(1400))


for(i in c(1:length(retvect))){
  print(namevect[i])
  print(Box.test(abs(retvect[[i]]), lag = 20, type = c("Ljung-Box")))
}

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

acf.abs.doge <- acf(doge_return**2)$acf[2:21]

for(i in c(1:1)){
  critVals <- try(sum(acf.abs.doge** 2 / (n.ret - c(1:20))))
  temp <- n.ret*(n.ret+2) * critVals
  print(pchisq(temp, 20, lower.tail = F))
  print(1 - pchisq(temp, 20))
}

  

######################################################################################################
## 2) heavy tails:
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

# mtext("Distribution of cryptocurrency returns", side = 3, line = -1.2, outer = TRUE)

# empty plot
plot(NA, NA)


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
namevect <- c("SMI", "S&P500", "CHF/USD", "AAPL", "BTC", "ETH", "USDT", "BNB", "XRP", "ADA", "DOGE")
retvect <- list(smi_return, snp_return, chfusd_return, apple_return, btc_return, eth_return,
             usdt_return, bnb_return, xrp_return, ada_return, doge_return)
for (i in c(1:11)){
  temp_row <- distr_facts(retvect[[i]], namevect[i])
  fact_df <- rbind(fact_df, temp_row)
}
fact_df

# path <- "~/R/projects/Bachelorarbeit/results/Crypto_facts.xlsx"
# sheet_name = "summary"
# 
# 
# if(sheet_name %in% names(getSheets(loadWorkbook(path)))){
#   wb <- loadWorkbook(path)
#   removeSheet(wb, sheetName = sheet_name)
#   saveWorkbook(wb, path)
# }
# 
# write.xlsx(fact_df, path, sheetName = sheet_name, 
#            col.names = TRUE, row.names = FALSE, append = TRUE)


######################################################################################################
## 2b) tail index estimation


######################################################################################################
## 3) gain/loss asymmetry:
# Will not be done


######################################################################################################
## 4) aggregational gaussian:


btc_price

agg_gauss <- function(data){
  weekly <- rep(c(T, rep(F, 6)), floor(nrow(data)/7))
  monthly <- rep(c(T, rep(F, 30)), floor(nrow(data)/30))
  weekly = diff(log(data[weekly])) %>% na.omit()
  monthly = diff(log(data[monthly])) %>% na.omit()
  print(nrow(weekly))
  print(nrow(monthly))
  return(list(weekly, monthly))
}

plot_agg_gauss <- function(list, name){
  hist(list[[1]] %>% na.omit(), breaks = 50, freq = F, main = name)
  curve(dnorm(x, sd = sd(list[[1]] %>% na.omit())), add=TRUE, col = "red")
  hist((list[[1]] %>% na.omit()) / sd(list[[1]]), breaks = 50, freq = F, col = "blue",
       main = name)
  curve(dnorm(x,), add=TRUE, col = "red")
  # hist(list[[2]] %>% na.omit(), breaks = 20, freq = F, main = name)
  # curve(dnorm(x, sd = sd(list[[2]] %>% na.omit())), add=TRUE, col = "red")
}


smi_list <- agg_gauss(smi_price)
plot_agg_gauss(smi_list, "SMI")

snp_list <- agg_gauss(snp_price)
plot_agg_gauss(snp_list, "S&P 500")

chfusd_list <- agg_gauss(chfusd_price)
plot_agg_gauss(chfusd_list, "CHF / USD")



btc_list <- agg_gauss(btc_price)
plot_agg_gauss(btc_list, "Bitcoin")
hist(btc_return / sd(btc_return), main = "Bitcoin", freq = F, breaks = 100, 
     add = T, col=rgb(1,0,0,0.5))
curve(dnorm(x), add=TRUE, col = "black")



eth_list <- agg_gauss(eth_price)
plot_agg_gauss(eth_list, "Ethereum")

usdt_list <- agg_gauss(usdt_price)
plot_agg_gauss(usdt_list, "USDT")

bnb_list <- agg_gauss(bnb_price)
plot_agg_gauss(bnb_list, "BNB")

xrp_list <- agg_gauss(xrp_price)
plot_agg_gauss(xrp_list, "Ripple (XRP)")

ada_list <- agg_gauss(ada_price)
plot_agg_gauss(ada_list, "Cardano (ADA)")

doge_list <- agg_gauss(doge_price)
plot_agg_gauss(doge_list, "Dogecoin")


######################################################################################################
## 5) intermittency:







######################################################################################################
## 8)	Leverage effect
######################################################################################################

lag = 1
btc_log <- btc_return[c(1: (length(btc_return)-lag))] > 0
mean(btc_return$daily.returns[c(rep(FALSE, lag), btc_log)]**2)
mean(btc_return$daily.returns[c(rep(FALSE, lag), !btc_log)]**2)

for (i in c(1:lag)){
  i
  btc_log <- btc_return[c(1: (length(btc_return)-lag))] > 0
  pos = mean(btc_return$daily.returns[c(rep(FALSE, lag), btc_log)]**2)
  neg = mean(btc_return$daily.returns[c(rep(FALSE, lag), !btc_log)]**2)
}

btc_ret <- as.vector(btc_return)

cor(btc_ret[1:length(btc_ret)-1], abs(btc_ret[2:length(btc_ret)]))
lm(abs(btc_ret[2:length(btc_ret)]) ~ btc_ret[1:length(btc_ret)-1])
plot(btc_ret[1:(length(btc_ret)-2)], abs(btc_ret[3:length(btc_ret)]))
lm(abs(btc_ret[3:length(btc_ret)]) ~ btc_ret[1:(length(btc_ret)-2)])

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

test1 <- abs(smi_return[2:(length(smi_return))])
test2 <- smi_return[1:(length(smi_return)-1)]
(mean(test1*test2)-mean(test1)*mean(test2))/sqrt(mean(test1**2)-mean(test1)**2)/sqrt(mean(test2**2)-mean(test2)**2)                                                                                                                     cor(test1, test2)

# View(df.leverage)

# path <- "~/R/projects/Bachelorarbeit/results/copyPaste.xlsx"
# sheet_name = leverage
# 
# 
# if(sheet_name %in% names(getSheets(loadWorkbook(path)))){
#   wb <- loadWorkbook(path)
#   removeSheet(wb, sheetName = sheet_name)
#   saveWorkbook(wb, path)
# }
# 
# write.xlsx(df.leverage, path, sheetName = sheet_name,
#            col.names = TRUE, row.names = TRUE, append = TRUE)


## Sign Bias test

# for(i in c(1:length(namevect))){
#   vect_temp <- retvect[[i]]
#   ret_tmiuns1 <- vect_temp[1:length(vect_temp)-1]
#   retsquared <- vect_temp[2:length(vect_temp)]
#   retlogical <- ret_tmiuns1 < 0
#   
#   retdf <- data.frame(et2 = retsquared, etpast = ret_tmiuns1, isnegativ = retlogical)
#   retlm <- lm(retsquared ~ retlogical + I(retlogical == 1):ret_tmiuns1, data = retdf)
#   print(namevect[[i]])
#   print(summary(retlm))
# }

for(i in retvect){
  print(cor())
}


######################################################################################################
## 9) Volume/volatility
######################################################################################################
plot(as.vector(smi_return$daily.returns)**2, smi_data[,5] %>% na.omit())
cor(as.vector(smi_return$daily.returns)**2, smi_data[,5] %>%na.omit())

plot(as.vector(snp_return$daily.returns)**2, snp_data[,5] %>% na.omit())
cor(as.vector(snp_return$daily.returns)**2, snp_data[,5] %>%na.omit())

# no volume for exchange rrate provided
# plot(as.vector(chfusd_return$daily.returns)**2, chfusd_data[,5] %>% na.omit())
# cor(as.vector(chfusd_return$daily.returns)**2, chfusd_data[,5] %>%na.omit())


plot(as.vector(btc_return$daily.returns)**2, btc_data[,5])
cor(as.vector(btc_return$daily.returns)**2, btc_data[,5])
plot(as.vector(btc_return$daily.returns)**2 ~ as.vector(diff(btc_data[,5])))

plot(as.vector(eth_return$daily.returns)**2, eth_data[,5])
cor(as.vector(eth_return$daily.returns)**2, eth_data[,5])

plot(as.vector(usdt_return$daily.returns)**2, usdt_data[,5])
cor(as.vector(usdt_return$daily.returns)**2, usdt_data[,5])

plot(as.vector(bnb_return$daily.returns)**2, bnb_data[,5])
cor(as.vector(bnb_return$daily.returns)**2, bnb_data[,5])

plot(as.vector(xrp_return$daily.returns)**2, xrp_data[,5])
cor(as.vector(xrp_return$daily.returns)**2, xrp_data[,5])

plot(as.vector(ada_return$daily.returns)**2, ada_data[,5])
cor(as.vector(ada_return$daily.returns)**2, ada_data[,5])


# stable coins
plot(as.vector(usdc_return$daily.returns)**2, usdc_data[,5])
cor(as.vector(usdc_return$daily.returns)**2, usdc_data[,5])

plot(as.vector(busd_return$daily.returns)**2, busd_data[,5])
cor(as.vector(busd_return$daily.returns)**2, busd_data[,5])

plot(as.vector(ust_return$daily.returns)**2, ust_data[,5])
cor(as.vector(ust_return$daily.returns)**2, ust_data[,5])


