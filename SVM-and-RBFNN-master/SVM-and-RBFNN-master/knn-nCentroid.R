
library(keras)


mnist <- dataset_mnist()
x_train <- mnist$train$x  #matrix 60,000 x 784
y_train <- mnist$train$y  #array 60,000
x_test <- mnist$test$x    #matrix 60,000 x 784
y_test <- mnist$test$y    #array 60,000


#matrices into arrays dim 784
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

#normalization
# Transform values into [0,1] range 0:white 255:Black-> 0:white 1:black
x_train <- x_train / 255
x_test <- x_test / 255


cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

#for kNN and nearest Centroid integer values [0,9] were used as labels
# Convert class vectors to binary class matrices
#y_train <- to_categorical(y_train, num_classes)
#y_test <- to_categorical(y_test, num_classes)

# kNN k=1, Euclidean dist, tie: all dist= to kth vote
#no PCA yet
#full training/test set
library(class)
knn_model= knn(as.matrix(x_train)[1:60000,], as.matrix(x_test)[1:10000,], cl= as.matrix(y_train)[1:60000,] ,k= 1,  prob=TRUE, use.all= TRUE )

library(gmodels)
#confusion matrix
a=CrossTable(as.matrix(y_test)[1:10000,], knn_model, prop.chisq = FALSE)

a
b=0
s=for(i in 1:10){
  b=b + a[[1]][i,i]
}  
b
accuracy=b/10000
accuracy

#kNN k=3, Euclidean dist, tie: all dist= to kth vote
library(gmodels)
knn_model= knn(as.matrix(x_train)[1:60000,], as.matrix(x_test)[1:10000,], cl= as.matrix(y_train)[1:60000,] , k= 3,  prob=TRUE, use.all= TRUE )
dim(as.matrix(x_train))
dim(as.matrix(x_test))
dim(as.matrix(y_train))

#confusion matrix
c=CrossTable(as.matrix(y_test)[1:10000,], knn_model, prop.chisq = FALSE)
c
d=0
s=for(i in 1:10){
  d=d + c[[1]][i,i]
}  
d
accuracy=d/10000
accuracy


#Nearest Centroid
library(lolR)

model=lol.classify.nearestCentroid(as.matrix(x_train)[1:60000,], as.matrix(y_train)[1:60000,])
#output: centroids[K=10,d=784] , ylabs[K=10] , prior probs[K=10] 

predict.nearestCentroid <- function(object, X, ...) {
  K <- length(object$ylabs); n <-  dim(X)[1]
  dists <- array(0, dim=c(n, K))
  for (i in 1:n) {
    for (j in 1:K) {
      dists[i, j] <- sqrt(sum((X[i,] - object$centroids[j,])^2))
    }
  }
  Yass <- apply(dists, c(1), which.min)
  Yhat <- object$ylabs[Yass]
  return(Yhat)
}

pred=predict.nearestCentroid(model, as.matrix(x_test)[1:10000,])

#confusion matrix
tab=table(pred, y_test[1:10000])
tab

k=0
s=for(i in 1:10){
  k=k + tab[i,i]
}  
k
accuracy=k/10000
#model's accuracy
accuracy

