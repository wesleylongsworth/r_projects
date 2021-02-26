library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

data <- read.csv('data.csv')

names(data)
View(data)
str(data)

ggplot(data, aes(Cohort)) +
  geom_bar(aes(fill=Cohort)) +
  scale_fill_brewer(palette = "Greens") +
  coord_flip()


set.seed(54321)
indexes <- createDataPartition(data$Cohort,
                               times = 1, 
                               p = 0.7,
                               list = FALSE)

data.train <- data[indexes,]
data.test <- data[-indexes,]

prop.table(table(data$Cohort))
prop.table(table(data.train$Cohort))
prop.table(table(data.test$Cohort))




# Set up caret to perform 10-fold cross validation repeated 3 times and use a grid search for optimal model hyperparameter values
# cross validation is a means of estimating how well our model will function in production on brand new data
# down side is that it's computationally intense# build out 5 machine learning models to determine best model to proceed with
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"



# A) linear algorithms
# linear regression
set.seed(7)
fit.lda <- train(Cohort ~., data = data.train, method = "lda", metric=metric, trControl = control)

# B) non-linear algorithms
# Cart
set.seed(7)
fit.cart <- train(Cohort ~., data = data.train, method = "rpart", metric=metric, trControl = control)

# kNN
set.seed(7)
fit.knn <- train(Cohort ~., data = data.train, method = "knn", metric=metric, trControl = control)

# C) Advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Cohort ~., data = data.train, method = "svmRadial", metric=metric, trControl = control)

# Random Forest
set.seed(7)
fit.rf <- train(Cohort ~., data = data.train, method = "rf", metric=metric, trControl = control)

## summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)

dotplot(results)

# summarize best model - rf
print(fit.rf)

# estimate power of rf model on new data - the test set
predictions <- predict(fit.rf, data.test)
confusionMatrix(predictions, data.test$Cohort)


# tune rf model
mtry <- sqrt(ncol(data.train))
control.rf <- trainControl(method ="repeatedcv", number = 10, repeats = 3, search = "grid")
set.seed(7)
tunegrid <- expand.grid(.mtry = c(1:15))
rf_gridsearch <- train(Cohort~., data = data.train, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = control.rf)



varImp(fit.rf)

ggplot(data, aes(Cohort, Pre.trial.Responses)) +
  geom_point() +
  facet_grid(Cohort ~., labeller = label_both)
names(data)
pairs(~Pre.trial.Responses + In.Trial.Responses + Pre.DD + In.DD + Pre.Intro + In.Intro +
        +Cohort,data=data, 
      main="Simple Scatterplot Matrix")
