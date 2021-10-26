# big-data-econometrics
select work from class

**matrix_function.R**

This is a function that will randomly remove a certain percentage of data from a matrix, impute the missing values using a principal components analysis, and then assess the accuracy of the imputed values as compared to the original matrix. (Instead of using prcomp(), this function finds the principal components via a svd(). This is the method used in ISLR2). The function imputes values using M = 1,...,8, and averages the results of 10 trials for each value of M. This is another way to understand the relative importance of each successive principal component. This function could be modified to impute truly missing values. In this scenario, it would be important to first pick of the number of principal components to use from a scree plot, since there is no way to evaluate the accuracy of the replacement values.

**forex pca**

PCA on 5yr weekly averaged forex data. I find that 3 principle components can explain 81% of the variance in 49 currency pairs.
