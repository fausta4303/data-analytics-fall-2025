##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)
library(Metrics)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/tony f/data-anal-lab1/Lab 4")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

### LAB CODE

####### PCA #######

#Compute the PCs and plot the dataset using the 1st and 2nd PCs. Also Identifying them :^D
wine_no_type <- wine[,-1]
mat_wine <- as.matrix(wine_no_type)
scaled_wine <- scale(mat_wine, center = T, scale = T)
principal_components <- princomp(scaled_wine)
summary(principal_components)

#According to the loadings, Flavanoids most influence PC1 and Color Intensity most influences PC2
principal_components$loadings[,1:2]

#Training knn with all variables. F1, precision, recall included
features <- wine[,2:14]
labels <- wine$Type

set.seed(67)
training_index <- sample(nrow(features), size = round(0.8 * nrow(features)))
train.features <- features[training_index,]
test.features <- features[-training_index,]
train.labels <- labels[training_index]
test.labels <- labels[-training_index]

train.features <- scale(train.features)
test.features <- scale(test.features, 
                       center = attr(train.features, 'scaled:center'), 
                       scale = attr(train.features, 'scaled:scale'))

first_predictions <- knn(train.features, test.features, train.labels, k = 13)

#conf_matrix <- table(first_predictions, test.labels, dnn=list('predicted','actual'))
#print(sum(diag(conf_matrix)) / sum(conf_matrix))
#print(conf_matrix)
cm_allVar <- confusionMatrix(first_predictions, test.labels, mode = "prec_recall")
cm_allVar$table                                  
cm_allVar$byClass[, c("Precision","Recall","F1")] 
cm_allVar$overall["Accuracy"]


#Training with PC variabls (Flavanoids and PCI)
features <- as.data.frame(principal_components$scores[,1:2])
colnames(features) <- c("PC1", "PC2")
labels <- wine$Type

set.seed(67)
training_index <- sample(nrow(features), size = round(0.8 * nrow(features)))
train.features <- features[training_index,]
test.features <- features[-training_index,]
train.labels <- labels[training_index]
test.labels <- labels[-training_index]

train.features <- scale(train.features)
test.features <- scale(test.features, 
                       center = attr(train.features, 'scaled:center'), 
                       scale = attr(train.features, 'scaled:scale'))

first_predictions <- knn(train.features, test.features, train.labels, k = 13)

#conf_matrix <- table(first_predictions, test.labels, dnn=list('predicted','actual'))
#print(sum(diag(conf_matrix)) / sum(conf_matrix))
#print(conf_matrix)

cm_pc <- confusionMatrix(first_predictions, test.labels, mode = "prec_recall")
cm_pc$table                                  
cm_pc$byClass[, c("Precision","Recall","F1")] 
cm_pc$overall["Accuracy"]
