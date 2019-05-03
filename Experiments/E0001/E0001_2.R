# --------------------------------------------------------------------

# ----------- E0002 simulations to test data imputation -------------

# --------------------------------------------------------------------

# This experiment presents results on the convergence of 
# imputation and the more insights on the accuracy

rm(list=ls())
dev.off()

# --- Libraries and sources

source("Methodology/imputation_functions.R")


#set.seed(3452)
# --- Apply Function

# -- Matrix

N <- 100
K <- 4
A <- matrix(runif(K^2)*2-1, ncol=K) 
Sigma <- t(A) %*% A
#Sigma <- matrix(c(10,3,3,2),2,2)
Xcomplete <- mvrnorm(n = N, rep(0, K), Sigma)
missing <- matrix(rbinom(N*K,1,0.1),nrow = N, ncol = K)
Xmissing <- Xcomplete * (ifelse(missing==1,NA,1))
Xmeans <- matrix(rep(colMeans(Xcomplete), nrow(Xcomplete)), nrow(Xcomplete), byrow = TRUE)


# -- Initialize imputation

X0 <- NULL #Xcomplete + (missing * mvrnorm(n = N, rep(0, K), diag(K)*2))

# -- Loop

iter_vector <- seq(1,30,1)
rmse <- matrix(0,nrow = length(iter_vector), ncol=1)
rsquared <- matrix(0,nrow = length(iter_vector), ncol=1)

kk <- 1
Ximputed_list <- list()

for(iit in iter_vector){
  
  # -- Imputed matrix
  
  Ximputed <- impute_matrix(X0 = Xmissing, max_iter = iit, X_initial = X0)
  rmse[kk,1] <- sqrt(mean((Xcomplete - Ximputed)^2))
  rsquared[kk,1] <- 1 - sum((Xcomplete - Ximputed)^2)/sum((Xcomplete - ifelse(is.na(Xmissing),Xmeans,Xmissing))^2)
  Ximputed_list[[kk]] <- Ximputed*missing
  kk <- kk + 1
  #plot(as.vector(Xcomplete),as.vector(Ximputed))
  
}


plot(rmse, type="l")
plot(rsquared, type = "l")
plot(as.vector(Ximputed),as.vector(Xcomplete))
rsquared[30]
