############################
### KRR Cross Validation ###
############################

### Author: Yakun Wang
### Project 4

krr.cv <- function(dat_train, K.fold, lambda){
  ### Input:
  ### - train data frame
  ### - K.fold: a number stands for K-fold CV
  ### - tuning parameters 
  
  n <- dim(dat_train)[1]
  n.fold <- round(n/K.fold, 0)
  set.seed(0)
  s <- sample(rep(1:K.fold, c(rep(n.fold, K.fold-1), n-(K.fold-1)*n.fold)))  
  cv.train.error <- rep(NA, K.fold)
  cv.test.error <- rep(NA, K.fold)
  
  for (i in 1:K.fold){
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    
    krr.result <- KRR.Post(lambda = lambda, data = dat_train, train = train.data, test = test.data)
    
    cv.train.error[i] <- krr.result$train_RMSE
    cv.test.error[i] <- krr.result$test_RMSE
    
  }			
  return(list(mean_train_rmse = mean(cv.train.error), mean_test_rmse = mean(cv.test.error),
              sd_train_rmse = sd(cv.train.error), sd_test_rmse = sd(cv.test.error)))
}
