##https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

library("caret")

# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris

# create a list of 80% of the rows in the original dataset to create the training dataset for training and testing the model
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
training <- dataset[validation_index,]
# select the remaining 20% of the data for validation
validation <- dataset[-validation_index,]

# dimensions of dataset
dim(training)

# list types for each attribute
sapply(dataset, class)

# list the levels for the class
levels(dataset$Species)

# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# summarize attribute distributions
summary(dataset)

# split input and output
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#We will 10-fold cross validation to estimate accuracy.
#This will split our dataset into 10 parts, train in 9 and test on 1 and release for all combinations of 
#train-test splits. We will also repeat the process 3 times for each algorithm with different splits of 
#the data into 10 groups, in an effort to get a more accurate estimate.
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Build Models- Let's evaluate 5 different algorithms:
# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#select best model
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.lda)

# Make prediction-estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
#We can see that the accuracy is 100%. It was a small validation dataset (20%), but this result is within 
#our expected margin of 97% +/-4% suggesting we may have an accurate and a reliably accurate model.