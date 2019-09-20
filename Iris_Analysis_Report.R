# Remarks -------------------------------------------------------------
# Description: Iris Dataset Analysis Report
# Author: Manuel Alexis Mena Nieves
# Github: https://github.com/alexismenanieves/Iris_Dataset
# Date: September 17, 2019

# Step 1. Load and understand the data, tidy it if necessary-----------
library(tidyverse)
data(iris)
dim(iris)
str(iris)
summary(iris)
#   As we see, Iris dataset is spread by its variables, let's tidy it

# Step 2. Exploratory analysis ----------------------------------------
#   Let's see measures boxplot
iris %>% gather(Measure,Value, -Species) %>% ggplot(aes(x = Measure, y = Value, fill = Measure)) + 
  geom_boxplot(alpha = 0.4) + labs(title = "Boxplot for each variable in Iris Dataset") + 
  geom_jitter(alpha = 0.1)

#   Let's see the boxplot stratified by specie
iris %>% gather(Measure, Value, -Species) %>% ggplot(aes(x = Measure, y = Value, fill = Species)) + 
  geom_boxplot(alpha = 0.4) + facet_wrap(~ Measure, scales = "free") + 
  theme(axis.text.x = element_blank()) + labs(title = "Stratified boxplot in Iris Dataset")

#   It seems that Petal.Length and Petal.Width can explain the data, let's plot
iris %>% ggplot(aes(x = Petal.Length, y = Petal.Width, colour = Species)) + 
  geom_point(aes(shape = Species))

# Step 3. Modelling the data ------------------------------------------
library(caret)
set.seed(1979)
#   Create a train and test set
index <- createDataPartition(iris$Species, times = 1, p = 0.3, list = FALSE )
train_set <- iris[-index,]
test_set <- iris[index,]

#   Let's try a machine learning algorithm, in this case a decision tree
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
train_rpart <- train(Species ~ ., data = train_set, method = "rpart", trControl = fitControl) 
y_hat_tree <- predict(train_rpart,test_set)
confusionMatrix(y_hat_tree, test_set$Species)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

#   Now let's try the KNN algorithm
train_knn <- train(Species ~., data = train_set, method = "knn", trControl = fitControl, 
                   tuneGrid = data.frame(k = seq(5,20,1)))
y_hat_knn <- predict(train_knn, test_set)
confusionMatrix(y_hat_knn, test_set$Species)
ggplot(train_knn,highlight = TRUE)

#   To complete the modelling, let's try the random forest algorithm
train_rf <- train(Species ~., data = train_set, method ="rf", trControl = fitControl)
y_hat_rf <- predict(train_rf, test_set)
confusionMatrix(y_hat_rf, test_set$Species)
ggplot(varImp(train_rf))

#   Applying PCA
dist <- as.matrix(dist(iris[,1:4]))
as.data.frame(dist) %>% mutate(x = row_number()) %>% 
  gather(key = "y", value = "dist",-x, convert = TRUE) %>% 
  ggplot(aes(x,y,fill = dist)) + geom_raster() + 
  scale_fill_gradientn(colors  = rev(RColorBrewer::brewer.pal(9,"RdBu")))
cor(iris[,1:4])
pca <- prcomp(iris[,1:4])
summary(pca)
data.frame(pca$x[,1:2], Species = iris$Species) %>% 
  ggplot(aes(PC1, PC2, fill = Species)) + geom_point(cex = 3, pch = 21)
qplot(x = dist(iris[,1:4]),y = dist(pca$x[,1:2])) + geom_abline(color = "red") + 
  labs(title = "Distance comparison between Base and PCA ", 
       x = "Base distance", y = "PCA distance")
