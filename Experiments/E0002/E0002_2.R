# --------------------------------------------------------------------

# ----------- APPLY DATA IMPUTATION TECHNIQUE CONTINUOUS VARIABLES

# --------------------------------------------------------------------

# This experiment presents preliminary results on the convergence of imputation

rm(list=ls())
# setwd("C:/Users/u0106869/Google Drive/Independent work/JULVAL")
getwd()
# --- Libraries and sources

source("Methodology/imputation_functions.R")

# library(MASS)

# Data

library(readxl)
read_excel("Experiments/E0002/Boston_Housing.xls") -> boston_housing_dataset
# --- Apply Function


# -- Matrix

N <- nrow(boston_housing_dataset)
K <- ncol(boston_housing_dataset)
Xcomplete <- as.matrix(boston_housing_dataset)
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
as.matrix(complete(Xtmp)) -> Ximputed_mice

sqrt(mean((Xcomplete - Ximputed_mice)^2))
1 - sum((Xcomplete - Ximputed_mice)^2)/sum((Xcomplete - ifelse(is.na(Xmissing),Xmeans,Xmissing))^2)

## Data resilience

resilience(boston_housing_dataset) -> boston_resiliance
plot(rsq~mr, boston_resiliance, ylim = c(0,1))

