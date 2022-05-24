##############################################################################################
##############################################################################################
##############################################################################################

# Here all models are specified and added to a list

# load all packages
library(rugarch)
library(tidyverse)


# specifies all models and adds them to a df
distr_vect <- c()
model_name_vect <- c()
submodel_name_vect <- c()
p_vect <- c()
q_vect <- c()
spec_vect_final <- c()

model_distr <- c("norm",
                 "std",
                 "ged",
                 "nig",
                 "ghyp",
                 "jsu")

model_distr <- c(model_distr, "snorm", "sstd", "sged") # added skewed for norm, std & ged
# nig can already be skewed, depending on the parameters


garch_model_names <- c("sGARCH",
                       "eGARCH",
                       "iGARCH",
                       "gjrGARCH",
                       "csGARCH"
)

garch_submodel_names <- c("TGARCH",
                          "NGARCH")

garch_model_names_sub <- rep(c("fGARCH"), length(garch_submodel_names))

garch_model_names <- c(garch_model_names, garch_model_names_sub)


submodel_func <- function(model_vect, submodel_vect, index){
  if (model_vect[index] == "fGARCH"){
    sub_i <- index - (length(model_vect) - length(submodel_vect))
    return(submodel_vect[sub_i])
  } else {
    return(NULL) 
  }
}

### ATTENTION: the specification is garchOrder = c(q, p) 
### first ARCH, then GARCH parameter --> do not confuse with GARCH(p, q)
# garch
for (i in c(1:length(garch_model_names))){
  for (j in model_distr){
    for (p in c(1:4)){
      for (q in c(1:4)){
        
        submodel <- submodel_func(garch_model_names,
                                  garch_submodel_names, i)
        
        spec_temp <- ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                variance.model = list(model = garch_model_names[i],
                                                      submodel = submodel,
                                                      garchOrder = c(q, p)),
                                distribution.model = j)
        spec_vect_final <- c(spec_vect_final, spec_temp)
        model_name_vect <- c(model_name_vect, garch_model_names[i])
        if (is.null(submodel)){
          submodel = NA
        }
        submodel_name_vect <- c(submodel_name_vect, submodel)
        distr_vect <- c(distr_vect, j)
        p_vect <- c(p_vect, p)
        q_vect <- c(q_vect, q)
        
      }
    }
  }
}


# ARCH
arch_model_names <- c("sGARCH")


for (i in c(1:length(arch_model_names))){
  for (j in model_distr){
    for (q in c(1:4)){
      
      submodel <- submodel_func(arch_model_names, arch_submodel_names, i)
      
      spec_temp <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                              variance.model = list(model = arch_model_names[i],
                                                    garchOrder = c(q, 0)),
                              distribution.model = j)
      spec_vect_final <- c(spec_vect_final, spec_temp)
      model_name_vect <- c(model_name_vect, arch_model_names[i])
      if (is.null(submodel)){
        submodel = NA
      }
      submodel_name_vect <- c(submodel_name_vect, NA)
      distr_vect <- c(distr_vect, j)
      p_vect <- c(p_vect, 0)
      q_vect <- c(q_vect, q)
      
    }
  }
}



### ATTENTION: the specification is garchOrder = c(q, p) 
### first ARCH, then GARCH parameter --> do not confuse with GARCH(p, q)
# garch
for (i in c(1:length(garch_model_names_fix))){
  for (j in model_distr){
    for (p in c(1:4)){
      for (q in c(1:4)){
        
        submodel <- submodel_func(garch_model_names,
                                  garch_submodel_names, i)
        
        spec_temp <- ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                variance.model = list(model = "fGARCH",
                                                      submodel = "AVGARCH",
                                                      garchOrder = c(q, p)),
                                distribution.model = j)
        
        setfixed(spec_temp) <- list(eta11 = 0, eta12 = 0, eta13 = 0, eta14 = 0, 
                                    eta21 = 0, eta22 = 0, eta23 = 0, eta24 = 0)
        
        spec_vect_final <- c(spec_vect_final, spec_temp)
        model_name_vect <- c(model_name_vect, "fGARCH")
        submodel_name_vect <- c(submodel_name_vect, "AVGARCH")
        distr_vect <- c(distr_vect, j)
        p_vect <- c(p_vect, p)
        q_vect <- c(q_vect, q)
        
      }
    }
  }
}

#####
overall_vect <- submodel_name_vect
logical_vect <- is.na(submodel_name_vect)
overall_vect[logical_vect] <- model_name_vect[logical_vect]

spec_dataframe_final <- data.frame(model = model_name_vect,
                                   submodel = submodel_name_vect,
                                   name = overall_vect,
                                   distr = distr_vect,
                                   p = p_vect,
                                   q = q_vect)

print("Number of models:")
length(spec_vect_final)


