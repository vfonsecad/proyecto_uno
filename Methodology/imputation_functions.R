# --------------------------------------------------------------------

# ----------------- DATA IMPUTATION TECHNIQUE CONTINUOUS VARIABLES

# --------------------------------------------------------------------

# This data imputation technique is based on distance of observations. It goes as follows:

# Given a distance measure and X0 data matrix with missing entries:
#   0. Initialize X = X0 with missing entries filled in randomly 
#   1. Calculate D, distance matrix based on X of size NxN containing the distances of each couple of observations
#   2. Calculate W, matrix of weights which is D/sum(D rowwise) so that the rows of W sum to 1
#   3. Calculate X1 = W*X (matrix multiplication)
#   4. Fill in the missing data with corresponding entries in X1.
#   5. Repeat steps 1 to 4 until convergence


impute_matrix <- function(X0, distance_method = "euclidean", X_initial = NULL, max_iter = 30){
  
  
  # - Constants
  
  N <-dim(X0)[1]
  K <- dim(X0)[2]
  missing_matrix <- ifelse(is.na(X0),1,0)
  
  # - Iterations 
  
  kk <- 1
  
  # - Initialize
  
  if(is.null(X_initial)){
    
    X <- ifelse(is.na(X0),rnorm(1),X0)
    
  } else {
    
    X <- X_initial
    
  }
  
  # - Loop
  
  while(kk <= max_iter){
    Xstandard <- scale(X)
    D <- as.matrix(dist(Xstandard, distance_method))
    D <- exp(-D^2)
    diag(D) <- 0
    W <- D/matrix(rep(apply(D,1,sum),N),nrow = N, byrow = FALSE)
    X1 <- (W%*%X)*missing_matrix
    X <- ifelse(missing_matrix == 1,X1,X0)
    
    kk <- kk + 1
    
  }
  
  return(X)
}