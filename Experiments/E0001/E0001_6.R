# --------------------- Test of mutual information for 2 variables 
library(muti)

data(iris)
pairs(iris[,1:4])

x1 <- iris[,3]
y1 <- iris[,4]

cor(x1,y1)
h_x1 <- muti(x1,x1, lags=0)[,2]
h_y1 <- muti(y1,y1, lags=0)[,2]
mi_xy <- muti(x1,y1, lags=0)[,2]
rel_mi_xy <- mi_xy/sqrt(h_x1*h_y1)
