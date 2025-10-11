####################################
##### Abalone Data Preparation #####
####################################

library(class)
library(cluster)
## -------EXERCISE 1-------
# read dataset
abalone.data <- read.csv("C:\\Users\\tony f\\data-anal-lab1\\Lab 3\\abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
#abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"

s.train <- sample(4176,3000) 
s.train

# creat training and testing sets 
abalone.train <-abalone.data[s.train,]
abalone.test <-abalone.data[-s.train,] 

dim(abalone.test)
dim(abalone.train) 

## kNN Model 1: Age group based off length, diameter, and height
#sqrt(total number of observations). Use this as numbers of neighbors as discussed in class
knn.predicted_size <- knn(abalone.train[,2:4], abalone.test[,2:4], abalone.train[,10], k=64)
#kNN Model 2: Age group based off all weight variables
knn.predicted_weight <- knn(abalone.train[,5:8], abalone.test[,5:8], abalone.train[,10], k=64)

##Weight has better accuracy. 
table(knn.predicted_size, abalone.test[,10], dnn=list('size_knn','actual'))
table(knn.predicted_weight, abalone.test[,10], dnn=list('weight_knn','actual'))

##loop over 32-128 for k values
#3through this, it is found that 57 is the best k.
max_k_accuracy <- numeric(96)
for(k in 32:128){
  knn.predicted_weight <- knn(abalone.train[,5:8], abalone.test[,5:8], abalone.train[,10], k=k)
  confusion_mat <- table(knn.predicted_weight, abalone.test[,10], dnn=list('weight_knn','actual'))
  accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
  max_k_accuracy[k-31] <- accuracy
}
print(max_k_accuracy)
print("The best k for knn: ")
print(which.max(max_k_accuracy) + 31)



#take subset of features that give best results in KNN
##-------EXERCISE 2-------

##Below code uses the silhoutte method to find best K. When ran, show's that 2 is the best. Proof in Word doc.
avg_sil <- numeric(10)
for(k in 2:10){
  km <- kmeans(abalone.train[,5:8], centers=k, nstart=25)
  ss <- silhouette(km$cluster, dist(abalone.train[,5:8]))
  avg_sil[k] <- mean(ss[,3])
}
plot(1:10, avg_sil, type="b", pch=19,
     xlab="Number of clusters K",
     ylab="Average silhouette width",
     main = "kMeans version of best K finding")

km <- kmeans(abalone.train[,5:8], centers=2)


#now for PAM
#Best K is also 2
pam(abalone.train[,5:8], k=2, nstart=25)

sil_width <- numeric(10)
for (k in 2:10) {
  pam_fit <- pam(abalone.train[, 5:8], k = k) 
  sil_width[k] <- pam_fit$silinfo$avg.width
}
plot(2:10, sil_width[2:10], type = "b", pch = 19,
     xlab = "Number of clusters K",
     ylab = "Average silhouette width",
     main = "PAM version of best K finding")

pam_fit <- pam(abalone.train[,5:8], k=2)



##SILHUOETE PLOTS:
sil_km <- silhouette(km$cluster, dist(abalone.train[,5:8]))
plot(sil_km, border=NA, main="Silhuoette plot for kMeans (K=2)")

plot(silhouette(pam_fit), border=NA, main="Silhuoette plot for pam (K=2)")







#for PAM, K is number of clusters(?). 


