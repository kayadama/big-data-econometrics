### week 4 hw ### matrix completion function ###
### Brian Crist

# Write an R function to perform matrix completion as in Algorithm 12.1. 

# NEED
# 1) keep track of relative error & iteration count
# 2) iterations continue until threshold is reached
# 3) option to print out progress at each iteration

# EXPERIMENT
# 1) Boston data set, scale to have mean = 0 and stdev = 1
# 2) Randomly leave out an increasing (and nested) number of observations
# from 5% to 30% 
# 3) Apply algorithm 12.1 with M = 1,2,...,8
# 4) Display the approximation error as a function of the fraction of 
# observations that are missing, and the value M, averaged over 10 repetitions
# of the experiment


# This function does not have nested randomization when the percentage
# removed is increased. Each iteration of randomization is unique. 

library(MASS)

# turn data frame into a matrix, center and scale each column to have 
# mean zero and variance one

X <- data.matrix(scale(Boston))

# matcomtest() function notes:
# X = matrix
# perc = percentage of data that is removed for test

matcomtest <- function(X, perc = 0.05){
  
# matcom() function notes:
# X = matrix
# M = # of principal components used
# thresh = threshold for relative error
# progrep = T returns printout of each iteration; F does not
# iterlim = iteration limit
# perc = percentage of data missing
  
matcom <- function(X, M = 1, thresh = 1e-7, progrep = TRUE, iterlim = 25){
# step one of algorith 12.1: replace missing values with the column means
# of non-missing values

Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inq]

# code to implement algorithm 12.1
# first, write a function that takes in a matrix and returns an approximation
# to the matrix using the svd() function

fit.svd <- function(X){
  svdob <- svd(X)
  with(svdob
       , u[, 1:M, drop = FALSE] %*% (d[1:M] * t(v[, 1:M, drop = FALSE]))
  )
}

# step one of algorith 12.1: replace missing values with the column means
# of non-missing values

Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inq]

# set ourselves up to measure the progress of the iterations
rel_err <- 1
iter <- 0
ismiss <<- is.na(Xna)
mssold <- mean((scale(Xna, xbar, FALSE)[!ismiss])^2)
mss0 <- mean(Xna[!ismiss]^2)

# step two, approximate Xhat using fit.svd()
while((rel_err > thresh) & (iter < iterlim)) {
  iter <- iter + 1
  # step 2(a)
  Xapp <<- fit.svd(Xhat)
  # step 2(b)
  Xhat[ismiss] <- Xapp[ismiss]
  # step 2(c)
  mss <- mean(((Xna - Xapp)[!ismiss])^2)
  rel_err <- (mssold - mss) / mss0
  mssold <- mss
  if (progrep){
    cat("Iter:", iter, "MSS:", mss
        , "Rel.Err:", rel_err, "\n")
  }
  
}

}


for (i in 1:8){
  # useful lines if you want to see every iteration
  # out <- paste0("Matrix completion for M = ", i)
  # print(out)
  sumcor <<- 0
  for (j in 1:10){
    # run 10 experiments for each M
    # select different 5% each time
    Xna <- X
    inp <- sample(seq(nrow(X)), (perc * nrow(X))) 
    inq <- sample(1:ncol(X), length(inp), replace = TRUE)
    index.na <- cbind(inp, inq)
    Xna[index.na] <- NA
    
    matcom(X, M = i, iterlim = 100, progrep = FALSE)
    
    # keep track of correlation values between original and imputed
    corval <- cor(Xapp[ismiss], X[ismiss])
    sumcor <<- corval + sumcor
    
  }
  
  out2 <- paste0("Average correlation for M = ", i, " is ", sumcor/10)
  print(out2)
}

}


# Experiments

# 5% missing
matcomtest(X)

# 10% missing 
matcomtest(X, 0.1)

# 15% missing
matcomtest(X, 0.15)

# 20% missing
matcomtest(X, 0.2)

# 25% missing
matcomtest(X,0.25)

# 30% missing
matcomtest(X, 0.3)






