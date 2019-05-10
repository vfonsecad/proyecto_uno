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
missing <- matrix(rbinom(N*K,1,0.05),nrow = N, ncol = K)
Xmissing <- Xcomplete * (ifelse(missing==1,NA,1))
Xmeans <- matrix(rep(colMeans(Xcomplete), nrow(Xcomplete)), nrow(Xcomplete), byrow = TRUE)
Xinit <- ifelse(is.na(Xmissing),Xmeans,Xmissing)


# --- Run the algoithm 

algorithm_output <- run_imputation(X0 = Xmissing, X_initial = Xinit)

# --- Some evaluations and playing

library("tibble")
library("magrittr")
library("FactoMineR")
library("purrr")
library("dplyr")
library("gganimate")
library("ggthemes")

algorithm_output %$%
  tibble(
    step_matrices = steps,
    pca = map(step_matrices, PCA, graph = FALSE),
    ind = map(pca, extract2, "ind"),
    coord = map(ind, extract2, "coord"),
    factor_plane = map(coord, as_tibble),
    by_step = Map(mutate, factor_plane, step = as.list(seq_along(factor_plane))),
    to_plot = map(by_step, mutate, missing_ind = as.character(apply(missing, 1, max)))
  ) %$%
  bind_rows(to_plot) -> steps_to_plot

steps_to_plot %>% 
  ggplot +
  aes(x = Dim.1, y = Dim.2, colour = missing_ind) +
  geom_point() + 
  theme_solarized_2(light = FALSE) +
  transition_time(step) +
  labs(title = "Step: {frame_time}")





