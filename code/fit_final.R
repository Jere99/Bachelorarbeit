

options(java.parameters = "-Xmx8000m") 
library(quantmod)
library(rugarch)
library(tidyverse)
library(xlsx)


all_return <- list(bitcoin_return, ethereum_return, tether_return, binance_return, 
                   ripple_return, cardano_return, dogecoin_return)

# spec_vect_final <- list(ugarchspec(mean.model = list(armaOrder = c(0, 0)),
#                               variance.model = list(model = "sGARCH",
#                                                     garchOrder = c(1, 1)),
#                               distribution.model = "std"),
# ugarchspec(mean.model = list(armaOrder = c(0, 0)),
#                               variance.model = list(model = "sGARCH",
#                                                     garchOrder = c(1, 1)),
#                               distribution.model = "std"))

# all_fit_final <- c()
n <- length(spec_vect_final)
all_fit_final <- as.list(rep(NA, 7*n))


coincounter <- 1

for(coin in all_return){
  counter <- 1
  for (i in spec_vect_final){ 
    
    try(all_fit_final[[(coincounter-1)*n+counter]] <- ugarchfit(data = coin, spec = i, solver ='hybrid'))
    
    print(paste(coincounter,"-", counter))
    counter <- counter + 1
    
  } 
  coincounter <- coincounter + 1
}

print("Number of fits:") # should be 1188
counter - 1

sum(is.na(all_fit_final))
spec_dataframe_final[is.na(all_fit_final),]




LLH <- rep(NA, length(all_fit_final))
AIC <- rep(NA, length(all_fit_final))
BIC <- rep(NA, length(all_fit_final))

for (j in c(1:length(all_fit_final))){
  
  try(LLH[j] <- likelihood(all_fit_final[[j]])) # Likelihood
  try(AIC[j] <- infocriteria(all_fit_final[[j]])[1])
  try(BIC[j] <- infocriteria(all_fit_final[[j]])[2])
  
}

########################################################################################
l <- nrow(spec_dataframe_final)
fit_allfinal_df <- rbind(spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final, spec_dataframe_final)
fit_allfinal_df$coin <- c(rep("Bitcoin", l), rep("Ethereum", l), rep("Tether", l), rep("BNB", l), rep("Ripple", l), rep("Cardano", l), rep("Dogecoin", l))
fit_allfinal_df$LLH <- LLH
fit_allfinal_df$AIC <- AIC
fit_allfinal_df$BIC <- BIC
names(fit_allfinal_df)
head(fit_allfinal_df)


path <- "~/R/projects/Bachelorarbeit/results/Crypto_GARCH.xlsx"
sheet_name = "final_FINAL"


if(sheet_name %in% names(getSheets(loadWorkbook(path)))){
  wb <- loadWorkbook(path)
  removeSheet(wb, sheetName = sheet_name)
  saveWorkbook(wb, path)
}

write.xlsx(fit_allfinal_df, path, sheetName = sheet_name, 
           col.names = TRUE, row.names = FALSE, append = TRUE)

# extracts all coefficientsw
coefToDf <- function(data){
  n <- length(data)
  coefDf <- data.frame(mu = rep(NA, n),
                       omega = rep(NA, n),
                       alpha1 = rep(NA, n),
                       alpha2 = rep(NA, n),
                       alpha3 = rep(NA, n),
                       alpha4 = rep(NA, n),
                       beta1 = rep(NA, n),
                       beta2 = rep(NA, n),
                       beta3 = rep(NA, n),
                       beta4 = rep(NA, n),
                       gamma1 = rep(NA, n),
                       gamma2 = rep(NA, n),
                       gamma3 = rep(NA, n),
                       gamma4 = rep(NA, n),
                       eta11 = rep(NA, n),
                       eta12 = rep(NA, n),
                       eta13 = rep(NA, n),
                       eta14 = rep(NA, n),
                       eta21 = rep(NA, n),
                       eta22 = rep(NA, n),
                       eta23 = rep(NA, n),
                       eta24 = rep(NA, n),
                       lambda = rep(NA, n),
                       skew = rep(NA, n),
                       shape = rep(NA, n),
                       ghlambda = rep(NA, n)
                       
  )
  coefNA <- coefDf[1,]
  for(i in c(1:length(data))){
    coef_temp <- try(round(coef(data[[i]]),10))
    
    if(class(coef_temp) == "try-error"){
      coef_temp <- coefNA
    }
    
    coefDf_temp <- coefDf[1,]
    
    coefDf_temp <- bind_rows(coefDf_temp, coef_temp)
    coefDf[i,] <- coefDf_temp[2,]
    #print(i)
  }
  return(coefDf)
  
}
stdErrToDf <- function(data){
  n <- length(data)
  coefDf <- data.frame(mu = rep(NA, n),
                       omega = rep(NA, n),
                       alpha1 = rep(NA, n),
                       alpha2 = rep(NA, n),
                       alpha3 = rep(NA, n),
                       alpha4 = rep(NA, n),
                       beta1 = rep(NA, n),
                       beta2 = rep(NA, n),
                       beta3 = rep(NA, n),
                       beta4 = rep(NA, n),
                       gamma1 = rep(NA, n),
                       gamma2 = rep(NA, n),
                       gamma3 = rep(NA, n),
                       gamma4 = rep(NA, n),
                       eta11 = rep(NA, n),
                       eta12 = rep(NA, n),
                       eta13 = rep(NA, n),
                       eta14 = rep(NA, n),
                       eta21 = rep(NA, n),
                       eta22 = rep(NA, n),
                       eta23 = rep(NA, n),
                       eta24 = rep(NA, n),
                       lambda = rep(NA, n),
                       skew = rep(NA, n),
                       shape = rep(NA, n),
                       ghlambda = rep(NA, n)
                       
  )
  coefNA <- coefDf[1,]
  for(i in c(1:length(data))){
    coef_temp <- try(round(data[[i]]@fit$robust.matcoef[," Std. Error"],10))
    
    if(class(coef_temp) == "try-error"){
      coef_temp <- coefNA
    }
    
    coefDf_temp <- coefDf[1,]
    
    coefDf_temp <- bind_rows(coefDf_temp, coef_temp)
    coefDf[i,] <- coefDf_temp[2,]
    #print(i)
  }
  return(coefDf)
  
}
pValsToDf <- function(data){
  n <- length(data)
  coefDf <- data.frame(mu = rep(NA, n),
                       omega = rep(NA, n),
                       alpha1 = rep(NA, n),
                       alpha2 = rep(NA, n),
                       alpha3 = rep(NA, n),
                       alpha4 = rep(NA, n),
                       beta1 = rep(NA, n),
                       beta2 = rep(NA, n),
                       beta3 = rep(NA, n),
                       beta4 = rep(NA, n),
                       gamma1 = rep(NA, n),
                       gamma2 = rep(NA, n),
                       gamma3 = rep(NA, n),
                       gamma4 = rep(NA, n),
                       eta11 = rep(NA, n),
                       eta12 = rep(NA, n),
                       eta13 = rep(NA, n),
                       eta14 = rep(NA, n),
                       eta21 = rep(NA, n),
                       eta22 = rep(NA, n),
                       eta23 = rep(NA, n),
                       eta24 = rep(NA, n),
                       lambda = rep(NA, n),
                       skew = rep(NA, n),
                       shape = rep(NA, n),
                       ghlambda = rep(NA, n)
                       
  )
  coefNA <- coefDf[1,]
  for(i in c(1:length(data))){
    coef_temp <- try(round(data[[i]]@fit$robust.matcoef[,"Pr(>|t|)"],10))
    
    if(class(coef_temp) == "try-error"){
      coef_temp <- coefNA
    }
    
    coefDf_temp <- coefDf[1,]
    
    coefDf_temp <- bind_rows(coefDf_temp, coef_temp)
    coefDf[i,] <- coefDf_temp[2,]
    #print(i)
  }
  return(coefDf)
  
}
tValsToDf <- function(data){
  n <- length(data)
  coefDf <- data.frame(mu = rep(NA, n),
                       omega = rep(NA, n),
                       alpha1 = rep(NA, n),
                       alpha2 = rep(NA, n),
                       alpha3 = rep(NA, n),
                       alpha4 = rep(NA, n),
                       beta1 = rep(NA, n),
                       beta2 = rep(NA, n),
                       beta3 = rep(NA, n),
                       beta4 = rep(NA, n),
                       gamma1 = rep(NA, n),
                       gamma2 = rep(NA, n),
                       gamma3 = rep(NA, n),
                       gamma4 = rep(NA, n),
                       eta11 = rep(NA, n),
                       eta12 = rep(NA, n),
                       eta13 = rep(NA, n),
                       eta14 = rep(NA, n),
                       eta21 = rep(NA, n),
                       eta22 = rep(NA, n),
                       eta23 = rep(NA, n),
                       eta24 = rep(NA, n),
                       lambda = rep(NA, n),
                       skew = rep(NA, n),
                       shape = rep(NA, n),
                       ghlambda = rep(NA, n)
                       
  )
  coefNA <- coefDf[1,]
  for(i in c(1:length(data))){
    coef_temp <- try(round(data[[i]]@fit$robust.matcoef[," t value"],10))

    if(class(coef_temp) == "try-error"){
      coef_temp <- coefNA
    }
    
    coefDf_temp <- coefDf[1,]

    coefDf_temp <- bind_rows(coefDf_temp, coef_temp)
    coefDf[i,] <- coefDf_temp[2,]
    #print(i)
  }
  return(coefDf)
  
}


coef_df_final <- coefToDf(all_fit_final)
pVal_df_final <- pValsToDf(all_fit_final)
stdErr_df_final <- stdErrToDf(all_fit_final)
tVal_df_final <- tValsToDf(all_fit_final)



df_saver <- function(df, path, sheet_name){
  
  if(sheet_name %in% names(getSheets(loadWorkbook(path)))){
    wb <- loadWorkbook(path)
    removeSheet(wb, sheetName = sheet_name)
    saveWorkbook(wb, path)
  }
  
  write.xlsx(df, path, sheetName = sheet_name,
             col.names = TRUE, row.names = FALSE, append = TRUE)
  
}

path2 <- "~/R/projects/Bachelorarbeit/results/Coefs_GARCH.xlsx"

df_saver(coef_df_final, path2, "coef_df_final")
df_saver(pVal_df_final, path2, "pVal_df_final")
df_saver(stdErr_df_final, path2, "stdErr_df_final")
df_saver(tVal_df_final, path2, "tVal_df_final")



n.acf <- length(all_fit_final)

acf_residuals.abs <- matrix(rep(NA, n.acf*21), ncol = 21)
counter_acf <- 1

for(i in all_fit_final){
  res_temp <- try(residuals(i, standardize = T))
  acf_temp <- try(acf(abs(res_temp), plot = F)$acf[c(0:21)])
  acf_residuals.abs[counter_acf, ] <- try(acf_temp)
  counter_acf <- counter_acf + 1
}

df_saver(acf_residuals.abs, path2, "acf_residuals_abs")


acf_residuals.squ <- matrix(rep(NA, n.acf*21), ncol = 21)
counter_acf <- 1

for(i in all_fit_final){
  res_temp <- try(residuals(i, standardize = T))
  acf_temp <- try(acf(res_temp**2, plot = F)$acf[c(0:21)])
  acf_residuals.squ[counter_acf, ] <- try(acf_temp)
  counter_acf <- counter_acf + 1
}

df_saver(acf_residuals.squ, path2, "acf_residuals_squ")


