# --------------------------------------------------------------------

# ----------- APPLY DATA IMPUTATION TECHNIQUE CONTINUOUS VARIABLES

# --------------------------------------------------------------------

# This experiment presents preliminary results on the convergence of imputation

rm(list=ls())
# setwd("C:/Users/u0106869/Google Drive/Independent work/JULVAL")
getwd()
# --- Libraries and sources

source("Methodology/imputation_functions.R")

library(MASS)


# --- Apply Function

# -- Matrix

Sigma <- matrix(c(10,3,3,2),2,2)
N <- 1000
K <- 2
Xcomplete <- mvrnorm(n = N, rep(0, K), Sigma)
missing <- matrix(rbinom(N*K,1,0.1),nrow = N, ncol = K)
Xmissing <- Xcomplete * (ifelse(missing==1,NA,1))
  

# -- Loop

iter_vector <- seq(2,50,1)
rmse <- matrix(0,nrow = length(iter_vector), ncol=1)
rsquared <- matrix(0,nrow = length(iter_vector), ncol=1)

kk <- 1

for(iit in iter_vector){
  
  # -- Imputed matrix
  
  result <- run_imputation(X0 = Xmissing, max_iter = iit)
  Ximputed <- get_imputed_matrix(result)
  rmse[kk,1] <- sqrt(mean((Xcomplete - Ximputed)^2))
  rsquared[kk,1] <- 1 - sum((Xcomplete - Ximputed)^2)/sum((Xcomplete - ifelse(is.na(Xmissing),0,Xmissing))^2)
  kk <- kk + 1
}
  

plot(iter_vector, rmse[,1], type = "l")
plot(iter_vector, rsquared[,1], type = "l")

   