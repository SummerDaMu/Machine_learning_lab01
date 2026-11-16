library(tidyverse)
PKS <- read_delim('parkinsons.csv',",")

#----------------------------------------2.1 divide into training and test and scale--------------------------------------------
#training60/test40/scale
set.seed(12345)
n = nrow(PKS) #totally numbers
id = sample(1:n, floor(n * 0.6))
train = PKS[id,]
test = PKS[-id,]
#scale
library(caret) #preprocess & predict
scaler = preProcess(train)
train_s = predict(scaler, train) 
test_s = predict(scaler, test)

#---------------------------------2.2 compute a linear regression model, estimate MSE, comment----------------------------------
#motor_UPDRS is normally distributed, function of the voice characteristics, intercept is 0.
#voice data
trainS_Vchar <- data.frame(train_s[, c(5, 7:ncol(train_s))])
testS_Vchar <- data.frame(test_s[, c(5, 7:ncol(test_s))])
#training model (lm-OLS)
train_model <- lm(motor_UPDRS ~ . - 1, data = trainS_Vchar) # use . to get all cols except y.
#estimate training and test MSE for training and test data.
Pred_mU_train <- predict(train_model, newdata = trainS_Vchar)
Train_MSE <- mean((trainS_Vchar$motor_UPDRS - Pred_mU_train) ^ 2)
print(Train_MSE)
Pred_mU_test <- predict(train_model, newdata = testS_Vchar)
Test_MSE <- mean((testS_Vchar$motor_UPDRS - Pred_mU_test) ^ 2)
print(Test_MSE)
summary(train_model)
#comment
#The variables that contribute significantly to the model are 'Jitter.Abs.', 'Shimmer', 'Shimmer.APQ5', 'Shimmer.APQ11', 'NHR', 'HNR', 'DFA' and 'PPE'.
#Jitter.Abs.(β = −0.169609, p = 3.31e-05), Shimmer(β = 0.592436, p = 0.004050), Shimmer.APQ5( β = -0.387507, p = 0.000668), Shimmer.APQ11(β = 0.305546, p = 6.34e-07), NHR(β = -0.185387, p = 4.84e-05), HNR(β =  -0.238543, p = 6.41e-11), DFA(β = -0.280318, p < 2e-16), PPE(β = 0.226467, p = 6.70e-12)

#-----------------------------------------------2.3 using basic R to implement 4 functions----------------------------------------
#a. loglikelihood function, vector theta, dispersion sigma, stated model, training data.
#log𝑃(𝑇|𝜽,𝜎), normally distribution.
Loglikelihood <- function(params, X, T){
  #1. params
    n_params <- length(params)
    sigma <- params[n_params]
    theta <- params[-n_params]
    if (sigma <= 0){
      return(Inf)
    }
  #2. rss
    rss <- sum((T - X %*% theta) ^ 2)
    n <- length(T)
    return(- n/2 * log(2 * pi) - n * log(sigma) - 1/(2 * (sigma ^ 2)) * rss )
}
#b. Ridge function, theta, sigema, lambda, add penalty.
Ridge <- function(params, X, T, lambda){
  #1.params
  n_params <- length(params)
  sigma <- params[n_params]
  theta <- params[-n_params]
  #2.penalty
  penalty <- lambda * sum(theta ^ 2)
  #3.ridge
  return(-Loglikelihood(params, X, T) + penalty)
}
#c. RidgeOpt function, lamda, optims method 'BFGS', find sigma and seta for given lambda.s
RidgeOpt <- function(X, T, lambda){
  p <- ncol(X)
  start_point <- c(rep(0, p), 1) #theta, sigma
  opt_result <- optim(
    start_point, 
    fn = Ridge, 
    X = X,
    T = T,
    lambda = lambda,
    method = 'BFGS'
  )
  return(opt_result)
}
#d. DF function, scala lamda, computes the degrees of freedom.
DF <- function(X, lambda) {
  p <- ncol(X)
  I <- diag(p) # indicate matrix
  XtX <- t(X) %*% X #transpose X-1 * X
  inv_matrix <- solve(XtX + lambda * I)
  df_value <- sum(diag(inv_matrix %*% XtX))
  return(df_value)
}
#------------------------------------------------------4.using function RidgeOpt -------------------------------------------------
real_trainX <- as.matrix(trainS_Vchar[-1])   #voice charactor
real_testX <- as.matrix(testS_Vchar[-1])   #voice charactor
real_trainT <- trainS_Vchar$motor_UPDRS      
real_testT <- testS_Vchar$motor_UPDRS     
results <- data.frame(lambda = numeric(), train_mse = numeric(), test_mse = numeric(), df = numeric())
#lambda = 1, 100, 1000
for (lam in c(1, 100, 1000)) {
  opt_res <- RidgeOpt(X = real_trainX, T = real_trainT, lambda = lam)
  best_theta <- opt_res$par[1 : ncol(real_trainX)] 
  train_preds <- real_trainX %*% best_theta
  test_preds <- real_testX %*% best_theta
  #calculate mse
  train_mse <- mean((real_trainT - train_preds)^2)
  test_mse <- mean((real_testT - test_preds)^2)
  #calculate df  
  df_val <- DF(X = real_trainX, lambda = lam)
  #save results
  results <- rbind(results, data.frame(lambda = lam, train_mse = train_mse, test_mse = test_mse, df = df_val))
}
print(results)
#Based on the results, as the penalty parameter lambda increases from 1 to 1000, the degrees of freedom (df) decrease significantly from 13.86 to 5.64, indicating that the Ridge penalty effectively reduces model complexity. 
#The most appropriate penalty parameter is lambda = 100, as it yields the lowest Test MSE (0.9323). 
#At lambda = 1,the model is slightly more complex and has a higher test MSE, suggesting mild overfitting. 
#Conversely, at lambda = 1000, the model is over-penalized (high bias/underfitting), leading to an increase in both training and test MSE. 
#Therefore, lambda = 100 achieves the best bias-variance trade-off for this dataset.
