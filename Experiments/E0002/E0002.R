# --------------------------------------------------------------------

# ----------- APPLY DATA IMPUTATION TECHNIQUE CONTINUOUS VARIABLES

# --------------------------------------------------------------------

# This experiment presents preliminary results on the convergence of imputation

rm(list=ls())
# setwd("C:/Users/u0106869/Google Drive/Independent work/JULVAL")
getwd()
# --- Libraries and sources

source("./Methodology/imputation_functions.R")

# library(MASS)


# --- Apply Function

# -- Matrix

N <- 150
K <- 4
Xcomplete <- as.matrix(iris[1:4])
missing <- matrix(rbinom(N*K,1,0.1),nrow = N, ncol = K)
Xmissing <- Xcomplete * (ifelse(missing==1,NA,1))
Xmeans <- matrix(rep(colMeans(Xcomplete), nrow(Xcomplete)), nrow(Xcomplete), byrow = TRUE)


# -- Loop

iter_vector <- seq(1,50,1)
rmse <- matrix(0,nrow = length(iter_vector), ncol=1)
rsquared <- matrix(0,nrow = length(iter_vector), ncol=1)

kk <- 1

for(iit in iter_vector){
  
  # -- Imputed matrix
  
  Ximputed <- impute_matrix(X0 = Xmissing, max_iter = iit, X_initial = ifelse(is.na(Xmissing),Xmeans,Xmissing))
  rmse[kk,1] <- sqrt(mean((Xcomplete - Ximputed)^2))
  rsquared[kk,1] <- 1 - sum((Xcomplete - Ximputed)^2)/sum((Xcomplete - ifelse(is.na(Xmissing),Xmeans,Xmissing))^2)
  kk <- kk + 1
}


plot(iter_vector, rmse[,1], type = "l")
plot(iter_vector, rsquared[,1], type = "l")


## Multiple imputation chained equations

library(mice)
mice(Xmissing) -> Xtmp
complete(Xtmp) -> Ximputed

1 - sum((Xcomplete - Ximputed)^2)/sum((Xcomplete - ifelse(is.na(Xmissing),Xmeans,Xmissing))^2)

