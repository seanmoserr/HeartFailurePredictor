##PROJECT 1

##load dataset, explore structure
library(readr)
heartFailure <- read_csv("Desktop/FS2023/statisticalAnalysis4720/heartFailure.csv")
View(heartFailure)
str(heartFailure)

##convert "DEATH_EVENT" to factor
heartFailure$DEATH_EVENT <- as.factor(heartFailure$DEATH_EVENT)
is.factor(heartFailure$DEATH_EVENT)
heartFailure$sex <- as.factor(heartFailure$sex)

##pairs command
pairs(heartFailure, col = heartFailure$DEATH_EVENT) 

##K-nearest neighbor classification
set.seed(333)
library(class)
train.set <- sample(1:nrow(heartFailure), 0.7*nrow(heartFailure), replace=F)
heart.train <- heartFailure[train.set, ]
heart.test <- heartFailure[-train.set, ]

heart.knn1 <- knn(train = heart.train[, -13, drop = FALSE],
                  test = heart.test[, -13, drop = FALSE],
                  cl = heart.train$DEATH_EVENT,
                  k = 1)

heart.knn3 <- knn(train = heart.train[, -13, drop = FALSE],
                  test = heart.test[, -13, drop = FALSE],
                  cl = heart.train$DEATH_EVENT,
                  k = 3)

heart.knn4 <- knn(train = heart.train[, -13, drop = FALSE],
                 test = heart.test[, -13, drop = FALSE],
                 cl = heart.train$DEATH_EVENT,
                 k = 4)

heart.knn8 <- knn(train = heart.train[, -13, drop = FALSE],
                  test = heart.test[, -13, drop = FALSE],
                  cl = heart.train$DEATH_EVENT,
                  k = 8) 

heart.knn12 <-  knn(train = heart.train[, -13, drop = FALSE],
                    test = heart.test[, -13, drop = FALSE],
                    cl = heart.train$DEATH_EVENT,
                    k = 12) 

heart.knn16 <- knn(train = heart.train[, -13, drop = FALSE],
                   test = heart.test[, -13, drop = FALSE],
                   cl = heart.train$DEATH_EVENT,
                   k = 16)

heart.knn20 <- knn(train = heart.train[, -13, drop = FALSE],
                   test = heart.test[, -13, drop = FALSE],
                   cl = heart.train$DEATH_EVENT,
                   k = 20)


##view misclassification rates for knn classification with all predictors
mean(heart.test$DEATH_EVENT != heart.knn1)
mean(heart.test$DEATH_EVENT != heart.knn2)
mean(heart.test$DEATH_EVENT != heart.knn3)
mean(heart.test$DEATH_EVENT != heart.knn4)
mean(heart.test$DEATH_EVENT != heart.knn8)
mean(heart.test$DEATH_EVENT != heart.knn12)
mean(heart.test$DEATH_EVENT != heart.knn16)
mean(heart.test$DEATH_EVENT != heart.knn20)


##Tree method
library(tree)
heart.tree = tree(heart.train$DEATH_EVENT ~ ., data = heart.train)
summary(heart.tree)
plot(heart.tree)
text(heart.tree, pretty = 1)


##Support Vector Machines
library(e1071)

heart.SVM <- svm(DEATH_EVENT ~., data = heart.train,
                 kernel="linear", cost=2, scale=T)

heartPred.SVM <- predict(heart.SVM, data=heart.train)

table(heartPred.SVM, heart.train$DEATH_EVENT)

tuned.svm <- tune("svm", DEATH_EVENT~., data=heart.train,
                  kernel="linear", ranges=list(cost=c(0.01, 0.5, 1, 2, 5, 10)))
summary(tuned.svm)

##GLM
##Binomial, link = "logit"  

heart.GLM <- glm(heart.train$DEATH_EVENT ~ ., family = binomial(link="logit"), data = heart.train)
summary(heart.GLM)   

step(heart.GLM)