
# Assignment 1: Handwritten Digit Recognition

# Task 1: Import and split the data

data <- read.csv(file.choose())
names(data)[65] <- "digit"

n <- nrow(data)

set.seed(12345)
id <- sample(1:n, floor(n * 0.5))
train <- data[id, ]

id1 <- setdiff(1:n, id)

set.seed(12345)
id2 <- sample(id1, floor(n * 0.25))
valid <- data[id2, ]

id3 <- setdiff(id1, id2)
test <- data[id3, ]


# Task 2: KNN classification

library(kknn)

train$digit <- as.factor(train$digit)
valid$digit <- as.factor(valid$digit)
test$digit <- as.factor(test$digit)

knn_model <- kknn(
  digit ~ .,
  train = train,
  test = train,
  k = 30,
  kernel = "rectangular"
)

train_pred <- knn_model$fitted.values

train_cm <- table(
  Actual = train$digit,
  Predicted = train_pred
)

train_error <- mean(train_pred != train$digit)


knn_test_model <- kknn(
  digit ~ .,
  train = train,
  test = test,
  k = 30,
  kernel = "rectangular"
)

test_pred <- knn_test_model$fitted.values

test_cm <- table(
  Actual = test$digit,
  Predicted = test_pred
)

test_error <- mean(test_pred != test$digit)
train_cm
train_error

test_cm
test_error

