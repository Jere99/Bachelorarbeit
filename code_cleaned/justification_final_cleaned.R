
### Here the model are justified. It will be tested if the models are able to
### remoce the conditional heteroscedasticity by applying a Ljung-Box test
### to the squared residuals

options(java.parameters = "-Xmx8000m")

library(tidyverse)
library(reshape)
library(xlsx)
library(xts)

# path <- "~/R/projects/Bachelorarbeit/results/Crypto_GARCH.xlsx"
path <- "~/R/projects/Bachelorarbeit/results/Crypto_GARCH.xlsx"

acf.squ <- read.xlsx(path, "acf_residuals_squ")
acf.abs <- read.xlsx(path, "acf_residuals_abs")
str(acf.squ)


n.max <- which.min(all.analysis$BIC[all.analysis$coin == "Bitcoin"])

{
acf(abs(btc_return), main = "Bitcoin - \ntägliche absolute Rendite")
points(c(0:20), acf.abs[n.max,], col = "red")
legend(x = "topright",
       legend = c("absolute Rendite", "absolute Residuen"),
       lty = c(1, NA),
       pch = c(NA, 1),
       col = c("black", "red"),
       lwd = 1)
}

{
acf(abs(btc_return)**2, main = "Bitcoin - \ntägliche quadrierte Rendite")
points(c(0:20), acf.squ[n.max,], col = "red")
legend(x = "topright",
       legend = c("quadrierte Rendite", "quadrierte Residuen"),
       lty = c(1, NA),
       pch = c(NA, 1),
       col = c("black", "red"),
       lwd = 1)
}

# calculates the estimated parameters in the variance equation
coef.df_just <- read.xlsx(path2, "coef_df_final")
coef.df_just[overall_vect == "AVGARCH",c(15:22)] <- NA
n.ret <- nrow(btc_return) 
df.lost.varaince <- coef.df_just[, c(2:23)]
df.lost <- length(df.lost.varaince) - apply(df.lost.varaince, 1, function(x) sum(is.na(x)))

crit.vect.squ <- rep(NA, nrow(acf.squ))
for(i in c(1:nrow(acf.squ))){
  acf.vals <- try(as.numeric(acf.squ[i,])[2:21])
  critVals <- try(sum(acf.vals** 2 / (n.ret - c(1:20))))
  crit.vect.squ[i] <- try(n.ret*(n.ret+1) * critVals)
  print(i)
}

pVal1 <- pchisq(crit.vect.squ, 20-df.lost, lower.tail = F) 
boxplot(pVal1)
hist(pVal1, breaks = 100, main = "Ljung-Box Test", xlab = "p-Value")
abline(v = 0.05, col = "red")


log.vect1 <- pVal1 < 0.05
log.vect1[is.na(log.vect1)] <- FALSE
justification.df <- rbind(spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final)
justification.df$coin <- c(rep("Bitcoin", l), rep("Ethereum", l), rep("Tether", l), rep("BNB", l), rep("Ripple", l), rep("Cardano", l), rep("Dogecoin", l))
final.just <- justification.df[log.vect1,c(3:7)]


hist(pVal1[justification.df$p == 0], breaks = 100, main = "Ljung-Box Test (ARCH)", xlab = "p-Value")
abline(v = 0.05, col = "red")
hist(pVal1[justification.df$p != 0], breaks = 100, main = "Ljung-Box Test (GARCH + Erweiterungen)", xlab = "p-Value")
abline(v = 0.05, col = "red")

mean(pVal1 %>% na.omit() < 0.05)
mean(pVal1[justification.df$p == 0] %>% na.omit() < 0.05)
mean(pVal1[justification.df$p != 0] %>% na.omit() < 0.05)


summariser <- function(data){
  data.final <- data
  data.final[,"name"] <- as.factor(data[,"name"])
  data.final[,"distr"] <- as.factor(data[,"distr"])
  data.final[,"p"] <- as.factor(data[,"p"])
  data.final[,"q"] <- as.factor(data[,"q"])
  data.final[,"coin"] <- as.factor(data[,"coin"])
  print(summary(data.final[,c("coin", "name", "distr", "p", "q")], maxsum = 10))
  return <- summary(data.final[,c("coin", "name", "distr", "p", "q")], maxsum = 10)
}

summariser(final.just)
summariser(final.just %>% 
             filter(p == "0"))
summariser(final.just %>% 
             filter(p != "0"))
