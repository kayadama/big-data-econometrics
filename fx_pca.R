# this data was downloaded from factset on 9/21/2021
# it is foreign exchange data, weekly average, over the last 5 years
# my goal is to do a PCA analysis

library(readxl)
library(dplyr)
library(tidyr)

##### set up

# import data
fx <- read_xlsx('~/documents/data/factset_fx/fx_5yr_wkly_avg_transpose.xlsx')

# identify unwanted columns and delete
data.frame(colnames(fx))
fx <- fx[,-c(1:3,14,21,36,53)]

# change data into numeric. This seems roundabout, but it works and every 
# simpler solution I tried did not work

# create empty matrix
efex <- matrix(data = NA, nrow = dim(fx)[1], ncol = dim(fx)[2])
# add colnames
colnames(efex) <- colnames(fx)
# change each cell of fx into numeric and put in new matrix
for (i in 1:dim(fx)[2]) {
  efex[,i] <- c(as.numeric(fx[[i]]))
}

##### pca
pr.out <- prcomp(efex, scale = TRUE)
biplot(pr.out, scale=0)

pr.out$sdev

# proportion of variance explained 
pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var)
pve

# 42.4% variance explained by the first principal component

par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
    ylab = "Proportion of Variance Explained", ylim = c(0, 1),
    type = "b")
plot(cumsum(pve), xlab = "Principal Component",
    ylab = "Cumulative Proportion of Variance Explained",
    ylim = c(0, 1), type = "b")

# you can explain over 81% of the variance in the forex market over the last
# 5 years with the first 3 principal components, and over 86% with 4

sum(pve[1:3])
sum(pve[1:4])
