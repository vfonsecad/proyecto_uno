# --------------------------------------------------------------------

# ----------- E0001_5 Imputable simulation with new functions --------

# --------------------------------------------------------------------

# This experiment presents results on the convergence of 
# imputation and the more insights on the accuracy

rm(list=ls())
#dev.off()

# --- Libraries and sources

source("./Methodology/imputation_functions.R")
library(MASS)
library(mice)
library(ggplot2)
library(data.table)



simul_iter <- seq(1,30,1)

our_rsquared <- matrix(0,nrow = length(simul_iter), ncol=1)
mice_rsquared <- matrix(0,nrow = length(simul_iter), ncol=1)

for(sit in simul_iter){
  
  print(sit)
  
  # --- Apply Function
  
  # -- Matrix
  
  N <- 100
  K <- 4
  ncp <- 2
  TT <- mvrnorm(n = N, rep(0, ncp), diag(ncp))
  #TT <- mvrnorm(n = N, rep(0, K), diag(K))
  PP <- mvrnorm(n = ncp, rep(0, K), diag(K))
  A <- TT%*%PP + mvrnorm(n = N, rep(0, K), diag(K)*0.5)
  Sigma <- (t(scale(A)) %*% scale(A))/N
  Xcomplete <- mvrnorm(n = N, rep(0, K), Sigma)
  missing <- matrix(rbinom(N*K,1,0.1),nrow = N, ncol = K)
  Xmissing <- Xcomplete * (ifelse(missing==1,NA,1))
  Xmeans <- matrix(rep(colMeans(Xcomplete), nrow(Xcomplete)), nrow(Xcomplete), byrow = TRUE)
  
  
  # -- Initialize imputation
  
  X0 <- NULL #Xcomplete + (missing * mvrnorm(n = N, rep(0, K), diag(K)*2))
  
  # -- Loop
  
  iter_vector <- c(2,seq(2,30,1))
  rmse <- matrix(0,nrow = length(iter_vector), ncol=1)
  rsquared <- matrix(0,nrow = length(iter_vector), ncol=1)
  
  kk <- 1
  
  Ximputed_list <- run_imputation(X0 = Xmissing, max_iter = max(iter_vector), X_initial = X0)
  
  for(Ximputed in Ximputed_list[["steps"]]){
    
    rmse[kk,1] <- sqrt(mean((Xcomplete - Ximputed)^2))
    rsquared[kk,1] <- 1 - min(1,sum((Xcomplete - Ximputed)^2)/sum((Xcomplete - ifelse(is.na(Xmissing),Xmeans,Xmissing))^2))
    kk <- kk + 1
    
  }
  
  #plot(iter_vector, rmse, type = "l")
  #plot(iter_vector, rsquared, type = "l")
  
  
  # --- Multiple imputation chained equations
  
  Xtmp <- mice(Xmissing , printFlag=FALSE)
  Ximputed_mice <- complete(Xtmp)
  mice_rsquared_current <- 1 - min(1,sum((Xcomplete - Ximputed_mice)^2)/sum((Xcomplete - ifelse(is.na(Xmissing),Xmeans,Xmissing))^2))
  
  # --- Comparison
  
  
  our_rsquared[sit,1] <- rsquared[length(iter_vector)]
  mice_rsquared[sit,1] <- ifelse(is.na(mice_rsquared_current),0,mice_rsquared_current)

}

things_to_plot <- rbind(data.table("MEASURE" = "our_rsquare", XVALUE = simul_iter, YVALUE = our_rsquared[,1]),
                        data.table("MEASURE" = "mice_rsquare",XVALUE = simul_iter, YVALUE = mice_rsquared[,1]))


ggplot(data = things_to_plot, aes(x = XVALUE,y = YVALUE, group = MEASURE, color = MEASURE))+
  geom_line()+
  ggtitle("Low covariability")
