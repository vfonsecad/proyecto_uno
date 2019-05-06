# --------------------------------------------------------------------

# ----------------- DATA IMPUTATION TECHNIQUE CONTINUOUS VARIABLES

# --------------------------------------------------------------------

# This data imputation technique is based on distance of observations. It goes as follows:


# Given a distance measure a Xnow data matrix and a 0-1 matrix of missing entries:
#   0. Standardize* the Xnow matrix
#   1. Calculate D, distance matrix based on standardized X of size NxN containing the distances of each couple of observations
#   2. Calculate exp(-D^2) kernel of distances, to Nadaraya Watson approach forecasting(?)
#   3. Set to 0 the diagonal of the kernels because the individual must have not weight in its own imputation
#   4. Calculate W, matrix of weights which is D/sum(D rowwise) so that the rows of W sum to 1, W[i] are the weights of all individuals to one bar it self
#   5. Calculate Ximput = W*X (matrix multiplication)
#   6. Fill in the missing data with corresponding entries in X1.


step_imputation <- function(Xnow, missings, dist_method){
  Xstandard <- scale(Xnow)
  D <- as.matrix(dist(Xstandard, dist_method))
  D <- exp(-D^2)
  diag(D) <- 0
  W <- D/rowSums(D)
  Ximput <- (W%*%Xnow)
  Xnew <- ifelse(missings == 1,Ximput,Xnow)
}


# Given a distance measure and X0 data matrix with missing entries:
#   0. Initialize X = X0 with missing entries filled in randomly (if there is not Xinitial) 
#   1. Set the number of imputation steps 
#   2. Repeat imputation_step until convergence
#   3. Return the result in every step

run_imputation <- function(X0, distance_method = "euclidean", X_initial = NULL, max_iter = 30){
  
  
  # - Constants
  
  N <-dim(X0)[1]
  K <- dim(X0)[2]
  missing_matrix <- ifelse(is.na(X0),1,0)
  
  # - Initialize
  
  if(is.null(X_initial)){
    
    Xmeans <- matrix(rep(colMeans(X0, na.rm = TRUE), nrow(X0)), nrow(X0), byrow = TRUE)
    X <- ifelse(is.na(X0),Xmeans,X0)
    
  } else {
    
    X <- X_initial
    
  }

  steps <- list(X)
  
  # - Loop
  
  for(kk in 2:max_iter){
    steps[[kk]] <- step_imputation(steps[[kk - 1]], missing_matrix, distance_method)  
  }
  
  output <- list(
    initial_matrix = X0,
    missings = missing_matrix,
    steps = steps,
    n_steps = max_iter
  )
  
  return(output)

}

# Given an imputation result object get the imputated matrix
#   0. Know the imputation result lenght
#   1. Get the last matrix in the list

get_imputed_matrix <- function(output_object){
  output_object[["steps"]][[output_object[["n_steps"]]]]
}

#  Given a dataset calculate its resiliance
#  0. Set the parametters of the experiment, 
#  0.1. the missing rates are the proportions of missings from 1% to 25%, 
#  0.2. the complete matrix is the data.frame
#  0.3. the mean matrix is a matrix which columns are the means of the complete matrix
#  1. Repeat along the missing rates
#  1.1  Repeat "repetitions" times
#  1.1.1  simulate the lose of a % of data
#  1.1.2  impute the vunded dataset
#  1.1.3  calculate the rsquared of that imputation
#  1.2  Return the rsqared of these imputations
#  2. Return the rsquared of these imputations
#  3. Organize them in a table and deliver them


resilience <- function(dataset, repetitions = 50){
  
  seq(0.01, 0.30, by = 0.01) -> missing_rate
  paste("Q", missing_rate, sep = "_") -> output_names
  output <- data.frame()
  N <- nrow(dataset)
  K <- ncol(dataset)
  Xcomplete <- as.matrix(dataset)
  Xmeans <- matrix(rep(colMeans(Xcomplete), nrow(Xcomplete)), nrow(Xcomplete), byrow = TRUE)
  
  lapply(
    missing_rate,
    function(prop_of_missings){
      sapply(
        1:repetitions,
        function(i){
          missing <- matrix(rbinom(N*K,1,prop_of_missings),nrow = N, ncol = K)
          Xmissing <- Xcomplete * (ifelse(missing==1,NA,1))
          Xnull <- ifelse(is.na(Xmissing),Xmeans,Xmissing)
          result <- run_imputation(X0 = Xmissing, max_iter = 30)
          Ximputed <- get_imputed_matrix(result)
          1 - sum((Xcomplete - Ximputed)^2)/sum((Xcomplete - Xnull)^2)
        }
      )
    }
  ) -> list_of_results
  
  Map(data.frame, rsq = list_of_results, mr = missing_rate) -> dataframes_of_results

  do.call(rbind, dataframes_of_results)  
}

