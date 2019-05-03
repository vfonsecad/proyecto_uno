# --------------------------------------------------------------------

# ----------- E0001_3 simulations to test data imputation -------------

# --------------------------------------------------------------------

# This experiment presents results on the convergence of 
# imputation and the more insights on the accuracy

rm(list=ls())
dev.off()
setwd("/home/valeria/vfonsecad/INDEPENDENT_WORK/JULVAL/")

# --- Libraries and sources

source("./proyecto_uno/Methodology/imputation_functions.R")

library(MASS)
library(StatMatch)

set.seed(3452)

# --- Apply Function

# -- Matrix

Sigma <- matrix(c(10,3,3,2)*0.5985,2,2)
N <- 3
K <- 2
Xcomplete <- mvrnorm(n = N, rep(0, K), Sigma)
missing <- matrix(rbinom(N*K,1,0.3),nrow = N, ncol = K)
Xmissing <- Xcomplete * (ifelse(missing==1,NA,1))

# -- Initialize imputation

X <- Xcomplete + (missing * mvrnorm(n = N, rep(0, K), diag(K)*20))

# -- Loop

kk <- 1

Ximputed_list <- list()



while(kk <= 10){
  
  #D <- as.matrix(dist(X, "euclidean"))
  D <- mahalanobis.dist(X)
  W <- D/matrix(rep(apply(D,1,sum),N),nrow = N, byrow = FALSE)
  X1 <- (W%*%X)*missing
  X <- ifelse(missing == 1,X1,Xmissing)
  Ximputed_list[[kk]] <- X
  
  kk <- kk + 1
  
}

