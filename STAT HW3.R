#Consider the USArrests data. We will now perform hierarchical clustering on the states.
# (a) Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.

library(ISLR)
data("USArrests")
set.seed(1)
hclust.mod = hclust(dist(USArrests), method = 'complete')
plot(hclust.mod) #answer

# (b) Cut the dendrogram at a height that results in three distinct clusters. 
# Which states belong to which clusters?

cut3 = cutree(hclust.mod,3)
table(cut3)
cut3
plot(cut3) #shows number of states per cluster

# (c) Hierarchically cluster the states using complete linkage and Euclidean distance, 
# after scaling the variables to have standard deviation one.

hclust.scale = scale(USArrests)
set.seed(1)
hclust.scale.mod = hclust(dist(hclust.scale), method = 'complete')
plot(hclust.scale.mod, main = 'after scaling to have st. dev. 1')

# (d) What effect does scaling the variables have on the hierarchical clustering 
# obtained? In your opinion, should the variables be scaled before the inter-observation 
# dissimilarities are computed? Provide a justification for your answer.

#checking cutoff between clusters
cut.hclust.scale.mod = cutree(hclust.scale.mod,3)
table(cut.hclust.scale.mod)


# 10. In this problem, you will generate simulated data, and then perform PCA and K-means 
# clustering on the data.

# (a) Generate a simulated data set with 20 observations in each of three classes 
# (i.e. 60 observations total), and 50 variables. Hint: There are a number of functions 
# in R that you can use to generate data. One example is the rnorm() function; runif() is
# another option. Be sure to add a mean shift to the observations in each class so that 
# there are three distinct classes.

set.seed(2)
x = matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20,2] = 1
x[21:40,1] = 2
x[21:40,2] = 2
x[41:60,1] = 1
true.labels = c(rep(1,20), rep(2,20), rep(3,20))

# (b) Perform PCA on the 60 observations and plot the first two principal component 
# score vectors. Use a different color to indicate the observations in each of the 
# three classes. If the three classes appear separated in this plot, then continue 
# on to part (c). If not, then return to part (a) and modify the simulation so that
# there is greater separation between the three classes. Do not continue to part (c) 
# until the three classes show at least some separation in the first two principal component score vectors.

pcr.out = prcomp(x)
plot(pcr.out$x[,1:2], col = 1:3, pch=19)

# (c) Perform K-means clustering of the observations with K = 3. 
# How well do the clusters that you obtained in K-means clustering compare to the true class labels?
# Hint: You can use the table() function in R to compare the true class labels 
# to the class labels obtained by clustering. Be careful how you interpret the 
# results: K-means clustering will arbitrarily number the clusters, so you cannot 
# simply check whether the true class labels and clustering labels are the same.

km.out = kmeans(x,3,nstart = 20)
table(true.labels, km.out$cluster)

# (d) Perform K-means clustering with K = 2. Describe your results.

km.out = kmeans(x,2,nstart=10)
table(true.labels, km.out$cluster)

# (e) Now perform K-means clustering with K = 4, and describe your results.

km.out = kmeans(x,4,nstart=20)
table(true.labels, km.out$cluster)

# (f) Now perform K-means clustering with K = 3 on the first two principal component score 
# vectors, rather than on the raw data. That is, perform K-means clustering on the 60 × 2 matrix of 
# which the first column is the first principal component score vector, and the second 
# column is the second principal component score vector. Comment on the results.

km.out = kmeans(pcr.out$x[,1:2],3,nstart =20)
table(true.labels, km.out$cluster)

# (g) Using the scale() function, perform K-means clustering with K = 3 on the data 
# after scaling each variable to have standard deviation one. How do these 
# results compare to those obtained in (b)? Explain.

km.out = kmeans(scale(x),3,nstart=20)
table(true.labels, km.out$cluster)

