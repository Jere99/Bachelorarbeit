
#### Total analysis

options(java.parameters = "-Xmx8000m")

library(tidyverse)
library(reshape)
library(xlsx)
library(xts)
library(rugarch)


dataToNumeric <- function(data){
  data[,"p"] <- as.numeric(data[,"p"])
  data[,"q"] <- as.numeric(data[,"q"])
  data[,"LLH"] <- as.numeric(data[,"LLH"])
  data[,"AIC"] <- as.numeric(data[,"AIC"])
  data[,"BIC"] <- as.numeric(data[,"BIC"])
  return(data)
}

path <- "~/R/projects/Bachelorarbeit/results/Crypto_GARCH.xlsx"

all.analysis <- read.xlsx(path, "final_FINAL", stringsAsFactors=T)

error_table2 <- function(data_unmod){
  data <- data_unmod
  error_vect <- (data$LLH < 0)
  error_vect[is.na(error_vect)] <- TRUE
  data <- data[error_vect,]
  data.final <- data
  data.final[,"name"] <- as.factor(data[,"name"])
  data.final[,"distr"] <- as.factor(data[,"distr"])
  data.final[,"p"] <- as.factor(data[,"p"])
  data.final[,"q"] <- as.factor(data[,"q"])
  data.final[,"coin"] <- as.factor(data[,"coin"])
  print(summary(data.final[,c("coin", "name", "distr", "p", "q")], maxsum = 10))
  return <- error_vect
}


### Percentage plots
percentage_data_full <- function(data, vari, value){
  data <- data %>% select(name, p, q, distr, coin, value) %>% 
    pivot_wider(names_from = vari, values_from = value)
  return(data)
}
percentage_graph_full <- function(percentage_data, vari, value){
  
  min <- 5
  max <- length(percentage_data)
  mean_vect <- c()
  for(i in c(min:max)){
    for(j in c(min:max)){
      temp_df <- percentage_data[,c(i, j)] %>% na.omit()
      if(value == "LLH"){
        mean_vect <- c(mean_vect, round(mean(temp_df[,1] > temp_df[,2]),2))
      } else {
        mean_vect <- c(mean_vect, round(mean(temp_df[,1] < temp_df[,2]),2))
      }
      
    }
  }
  
  domination_mat <- matrix(mean_vect, ncol = sqrt(length(mean_vect)))
  
  colnames(domination_mat) <- names(percentage_data[5:max])
  rownames(domination_mat) <- names(percentage_data[5:max])
  for(i in c(1:nrow(domination_mat))){
    domination_mat[i, i] <- NA
  }
  mat_melted <- melt(domination_mat, na.rm = TRUE)
  
  if(value == "LLH"){
    crit <- "höheren LLH"
  } else if(value == "AIC"){
    crit <- "tieferen AIC"
  } else {
    crit <- "tieferen BIC"
  }
  
  if(vari == "name"){
    title2 <- "Modeltyp"
  } else if(vari == "distr"){
    title2 <- "Verteilung"
  } else if(vari == "p"){
    title2 <- "GARCH Parameter (p)"
  } else if(vari == "q"){
    title2 <- "ARCH Parameter (q)"
  }
  
  if(vari == "name"){
    varname <- "von Typ"
    axisname <- "Typ"
  } else if(vari == "distr"){
    varname <- "mit Verteilung"
    axisname <- "Verteilung"
  } else if(vari == "p"){
    varname <- "mit Parameterzah p"
    axisname <- "p"
  } else if(vari == "q"){
    varname <- "mit Parameterzahl q"
    axisname <- "q"
  }
  
  
  
  
  ggplot(data = mat_melted, aes(X1, X2, fill = value))+
    geom_tile(color = "white") +
    
    scale_fill_gradient2(low = "white", high = "dark red", 
                         limit = c(0,1), space = "Lab", 
                         name="relative Häufigkeit") +
    
    geom_text(aes(X1, X2,label=value), col = "black") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 60, vjust = 1,  hjust = 1))+
    coord_fixed() +
    labs(x = paste0(axisname, 2), y = paste0(axisname, 1), 
         title = paste("Vergleich nach: " ,title2),
         subtitle = paste0("Realtive Häufigkeit: Modelle ", varname,"1,\nwelche einen", crit, 
                           " haben, als Modelle\n", varname, "2, ceteris paribus."))
}
percentage_total_full <- function(data, vari, value){
  data <- percentage_data_full(data, vari, value)
  percentage_graph_full(data, vari, value)
}

# the same functino for all plots
percentage_total_full.coin <- function(data, vari, value, coin){
  data <- percentage_data_full(data, vari, value)
  if(vari == "name"){
    title2 <- "Modeltyp"
  } else if(vari == "distr"){
    title2 <- "Verteilung"
  } else if(vari == "p"){
    title2 <- "GARCH Parameter (p)"
  } else if(vari == "q"){
    title2 <- "ARCH Parameter (q)"
  }
  percentage_graph_full(data, vari, value) + 
    ggtitle(paste0("Vergleich nach: ", title2, "\n(Währung: ", coin, ")"))
}
percentage.allplots <- function(temp, coin){
  
  archvsgarch.temp <- temp %>% 
    filter(name == "sGARCH")
  
  print(percentage_total_full.coin(archvsgarch.temp, "p", "LLH", coin))
  print(percentage_total_full.coin(archvsgarch.temp, "p", "AIC", coin))
  print(percentage_total_full.coin(archvsgarch.temp, "p", "BIC", coin))
  
  onlygarch.temp <- temp %>%
    filter(p != 0)
  
  print(percentage_total_full.coin(onlygarch.temp, "name", "LLH", coin))
  print(percentage_total_full.coin(onlygarch.temp, "name", "AIC", coin))
  print(percentage_total_full.coin(onlygarch.temp, "name", "BIC", coin))
  
  print(percentage_total_full.coin(onlygarch.temp, "distr", "LLH", coin))
  print(percentage_total_full.coin(onlygarch.temp, "distr", "AIC", coin))
  print(percentage_total_full.coin(onlygarch.temp, "distr", "BIC", coin))
  
  print(percentage_total_full.coin(onlygarch.temp, "p", "LLH", coin))
  print(percentage_total_full.coin(onlygarch.temp, "p", "AIC", coin))
  print(percentage_total_full.coin(onlygarch.temp, "p", "BIC", coin))
  
  print(percentage_total_full.coin(onlygarch.temp, "q", "LLH", coin))
  print(percentage_total_full.coin(onlygarch.temp, "q", "AIC", coin))
  print(percentage_total_full.coin(onlygarch.temp, "q", "BIC", coin))
  
}


boxplotter_full <- function(data, vari, argi){
  
  if(vari == "name"){
    title2 <- "Modeltyp"
  } else if(vari == "distr"){
    title2 <- "Verteilung"
  } else if(vari == "p"){
    title2 <- "GARCH Parameter (p)"
  } else if(vari == "q"){
    title2 <- "ARCH Parameter (q)"
  }
  
  data[,"p"] <- as.factor(data[,"p"])
  data[,"q"] <- as.factor(data[,"q"])
  
  plot1 <- data %>% 
    ggplot() +
    geom_boxplot(aes_string(argi, vari)) +
    facet_wrap(~coin, scales = "free") +
    labs(title = paste("Vergleich nach: ", title2)) +
    xlab(argi) + ylab(title2) +
    theme_light()
  
  if(argi == "LLH"){
    plot1
  } else {
    plot1 +
      scale_x_reverse() +
      xlab(paste(argi, "(axis inverted)"))
  }
}


########################################

sum.error <- error_table2(all.analysis)
sum(sum.error)
nrow(all.analysis)
sum(sum.error) / nrow(all.analysis)

error_vect <- (all.analysis$LLH < 0)
error_vect[is.na(error_vect)] <- TRUE

final_all.analysis <- all.analysis
final_all.analysis <- all.analysis[!error_vect,]
sum(final_all.analysis$LLH < 0)


########################################
archvsgarch <- final_all.analysis %>% 
  filter(name == "sGARCH")

percentage_total_full(archvsgarch, "p", "LLH")
percentage_total_full(archvsgarch, "p", "AIC")
percentage_total_full(archvsgarch, "p", "BIC")

boxplotter_full(archvsgarch, "p", "LLH")
boxplotter_full(archvsgarch, "p", "AIC")
boxplotter_full(archvsgarch, "p", "BIC")

arch4 <- archvsgarch %>% 
  filter((p == 0) & (q == 4))

garch11 <- archvsgarch %>% 
  filter((p == 1) & (q == 1))

archgarch <- merge(garch11, arch4, by = c("name", "distr", "coin")) %>% 
  select(name, distr, coin, LLH.x, AIC.x, BIC.x, LLH.y, AIC.y, BIC.y)

archgarch %>% 
  na.omit() %>% 
  summarise(LLH = mean(LLH.x > LLH.y),
            AIC = mean(AIC.x < AIC.y),
            BIC = mean(BIC.x < BIC.y))


onlygarch <- final_all.analysis %>% 
  filter(p != 0)

levels(onlygarch$name) <- c("AVGARCH", "csGARCH", "eGARCH", "gjrGARCH", "iGARCH", "NGARCH", "sGARCH", "TGARCH", "GARCH")
onlygarch[onlygarch$name=="sGARCH", "name"] <- "GARCH"

percentage_total_full(onlygarch, "name", "LLH") # width 510
percentage_total_full(onlygarch, "name", "AIC")
percentage_total_full(onlygarch, "name", "BIC")

boxplotter_full(onlygarch, "name", "LLH")
boxplotter_full(onlygarch, "name", "AIC")
boxplotter_full(onlygarch, "name", "BIC")

lev <- onlygarch %>% 
  filter(name %in% c("eGARCH", "TGARCH", "gjrGARCH", "GARCH"))
percentage_total_full(lev, "name", "LLH")
percentage_total_full(lev, "name", "AIC")
percentage_total_full(lev, "name", "BIC")

levels(lev$coin)
for(i in levels(lev$coin)){
  data_temp <- lev %>% 
    filter(coin == i) %>% 
    filter(name %in% c("eGARCH","TGARCH", "gjrGARCH"))
  print(percentage_total_full(data_temp, "name", "AIC") + 
          ggtitle(paste0("Vergleich nach: Modelltyp\n(Währung: ", i, ")")))
}


nonlin <- onlygarch %>% 
  filter(name %in% c("AVGARCH", "NGARCH", "GARCH"))
percentage_total_full(nonlin, "name", "LLH")
percentage_total_full(nonlin, "name", "AIC")
percentage_total_full(nonlin, "name", "BIC")

levels(nonlin$coin)
for(i in levels(nonlin$coin)){
  data_temp <- nonlin %>% 
    filter(coin == i) %>% 
    filter(name %in% c("AVGARCH", "NGARCH", "sGARCH"))
  print(percentage_total_full(data_temp, "name", "LLH") + 
          ggtitle(paste0("Vergleich nach: Modelltyp \n(Währung: ", i, ")")))
}

onlygarch %>% 
  na.omit() %>% 
  filter(name %in% c("AVGARCH", "sGARCH")) %>% 
  pivot_wider(names_from = name, values_from = c(LLH, AIC, BIC)) %>% 
  mutate(sLLH = as.numeric(LLH_sGARCH > LLH_AVGARCH)) %>% 
  group_by(coin) %>% 
  na.omit() %>% 
  summarize(mean = mean(sLLH))



igarch <- onlygarch %>% 
  filter(name %in% c("iGARCH", "sGARCH"))
percentage_total_full(igarch, "name", "LLH")
percentage_total_full(igarch, "name", "AIC")
percentage_total_full(igarch, "name", "BIC")


for(i in levels(igarch$coin)){
  data_temp <- igarch %>% 
    filter(coin == i)
  print(percentage_total_full(data_temp, "name", "BIC") + 
          ggtitle(paste0("Vergleich nach: Modelltyp \n(Währung: ", i, ")")))
}

csgarch <- onlygarch %>% 
  filter(name %in% c("csGARCH", "sGARCH"))
percentage_total_full(csgarch, "name", "LLH")
percentage_total_full(csgarch, "name", "AIC")
percentage_total_full(csgarch, "name", "BIC")

for(i in levels(csgarch$coin)){
  data_temp <- csgarch %>% 
    filter(coin == i)
  print(percentage_total_full(data_temp, "name", "BIC") + 
          ggtitle(paste0("Vergleich nach: Modelltyp \n(Währung: ", i, ")")))
}



percentage_total_full(onlygarch, "distr", "LLH")
percentage_total_full(onlygarch, "distr", "AIC")
percentage_total_full(onlygarch, "distr", "BIC")

boxplotter_full(onlygarch, "distr", "LLH")
boxplotter_full(onlygarch, "distr", "AIC")
boxplotter_full(onlygarch, "distr", "BIC")

distr <- onlygarch %>% 
  filter(distr %in% c("std", "nig", "ghyp", "jsu", "sstd"))
percentage_total_full(distr, "distr", "LLH")
percentage_total_full(distr, "distr", "AIC")
percentage_total_full(distr, "distr", "BIC")


percentage_total_full(onlygarch, "q", "LLH")
percentage_total_full(onlygarch, "q", "AIC")
percentage_total_full(onlygarch, "q", "BIC")

boxplotter_full(onlygarch, "q", "LLH")
boxplotter_full(onlygarch, "q", "AIC")
boxplotter_full(onlygarch, "q", "BIC")


percentage_total_full(onlygarch, "p", "LLH")
percentage_total_full(onlygarch, "p", "AIC")
percentage_total_full(onlygarch, "p", "BIC")

boxplotter_full(onlygarch, "p", "LLH")
boxplotter_full(onlygarch, "p", "AIC")
boxplotter_full(onlygarch, "p", "BIC")


# add all plots for all coins
for(coin.name in levels(final_all.analysis$coin)){
  temp <- final_all.analysis[final_all.analysis$coin == coin.name,]
  plot(percentage.allplots(temp, coin.name))
}



#################################################################################################
## Coefficient analysis

coef.df <- read.xlsx(path, "coef_df_final")
stdE.df <- read.xlsx(path, "stdErr_df_final")

df.lost.unmod <- length(coef.df) - apply(coef.df, 1, function(x) sum(is.na(x)))
nret <- nrow(btc_return)


### lambda (NGARCH) ###
coef.df$coin <- all.analysis$coin
coef.df.lambda <- coef.df[all.analysis$name == "NGARCH",]

tVal2 <- ((coef.df$lambda - 2) / stdE.df$lambda)
pVal2 <- 2*pt(abs(tVal2), df = nret-df.lost.unmod, lower.tail = F)
Val2_df <- data.frame(tVal2 = tVal2, pVal2 = pVal2, coin = coef.df$coin) %>% na.omit()


ggplot(coef.df.lambda) +
  geom_boxplot(aes(lambda)) +
  ggtitle("Lambda Koeffizienten des Modelltyps NGARCH") +
  facet_wrap(~coin) +
  geom_vline(xintercept = 2, col = "orange", lwd = 0.1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

Val2_df %>% 
  ggplot() +
  geom_boxplot(aes(pVal2)) +
  geom_vline(xintercept = 0.05, col = "red") +
  ggtitle("Ist lambda signifikant unterschiedlich von 2?") +
  labs(x = "p-Value") +
  facet_wrap(~coin) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))


### sum of alpha and beta (sGARCH) ###
logvectsgarch <- all.analysis$name == "sGARCH"
logvectsgarch[all.analysis$p == 0] <- FALSE # removes all ARCH Models

coef.df.garch <- coef.df[logvectsgarch,c(3:10)]
coef.df.garch[is.na(coef.df.garch)] = 0
rowsumgarch <- data.frame(rowsum = rowSums(coef.df.garch), 
                          coin = all.analysis$coin[logvectsgarch])

ggplot(rowsumgarch) +
  geom_boxplot(aes(rowsum)) +
  ggtitle("Summe der ARCH und GARCH Parameter") +
  facet_wrap(~coin) +
  geom_vline(xintercept = 1, col = "orange", lwd = 0.1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab(expression(~ Sigma*alpha + Sigma*beta))


rowsumgarch %>% 
  group_by(coin) %>% 
  summarize(mean = mean(rowsum))



### std Verteilung ###

shape.std <- coef.df[all.analysis$distr == "std", c("shape", "coin")]

ggplot(shape.std) +
  geom_boxplot(aes(shape)) +
  facet_wrap(~coin) +
  ggtitle("Verteilung des Shape-Parameters (t-Verteilung)")

shapebycoin <- shape.std %>% 
  group_by(coin) %>% 
  summarize(mean = mean(shape))

mean(shape.std$shape)
shapebycoin

{
seq1 <- seq(-4, 4, 0.1)
colvect <- c("light green", "blue", "green", "orange", "black", "grey", "brown")
plot(seq1, 
     ddist(distribution = "std", seq1, mu = 0, sigma = 1, shape = shapebycoin[4, 2])
     , type = "l", col = colvect[4], ylab = "f(x)", xlab = "x", main = "Dichtefunktion (t-Verteilung)")
for(i in c(1:7)){
  points(seq1, 
         ddist(distribution = "std", seq1, mu = 0, sigma = 1, shape = shapebycoin[i, 2])
         , type = "l", col = colvect[i])
}
#plot(seq1, ddist(distribution = "std", seq1, mu = 0, sigma = 1, shape = 3), type = "l")
lines(seq1, ddist(distribution = "norm", seq1, mu = 0, sigma = 1), col = "red", lty = 2)

legend("topright", legend= c(as.vector(shapebycoin$coin), "N(0, 1)"), col = c(colvect, "red"), lty=c(rep(1, 7), 2), cex=0.8)
}
