#Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[2]), as.character(obs[1])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

# Alternating least squares
# a function returns a list containing factorized matrices p and q, training and testing RMSEs.

ALS.R1R2 <- function(f = 10, 
                     lambda = 0.3, max.iter,
                     data, train, test){
  
  # Step 1: Initialize Movie vectors (q), Movie bias(bi) and user bias(bu)
  set.seed(0)
  
  # Initialize (q): assigning the average rating for that movie as the first row, 
  # and small random numbers for the remaining entries 
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  # movie.average <- data %>% group_by(movieId) %>% summarize(ave=mean(rating))
  # q[1,] <- movie.average$ave
  
  # Initialize (bi)
  bi <- matrix(rep(0, I), ncol = I)
  colnames(bi) <- levels(as.factor(data$movieId))
  
  # Initialize (bu)
  bu <- matrix(rep(0, U), ncol=U)
  colnames(bu) <- as.character(1:U)
  
  # grand mean
  mu <- mean(train$rating)
  
  train_RMSE <- c()
  test_RMSE <- c()
  
  
  for (l in 1:max.iter){
    
  # Step 2: Fix q, solve p
    # define new factors
    q.tilde <- rbind(rep(1,I), q)
    colnames(q.tilde) <- levels(as.factor(data$movieId))
    p.tilde <- rbind(bu, p)
    
  for (u in 1:U) {
    ru <- train[train$userId==u,] %>% arrange(movieId,rating)
    p.tilde[,u] <- solve(q.tilde[,colnames(q.tilde) %in% sort(train[train$userId==u,]$movieId)] %*% 
            t(q.tilde[,colnames(q.tilde) %in% sort(train[train$userId==u,]$movieId)]) + lambda * diag(f+1)) %*%
            q.tilde[,colnames(q.tilde) %in% sort(train[train$userId==u,]$movieId)] %*% 
            (ru$rating - mu - bi[,colnames(bi) %in% sort(train[train$userId==u,]$movieId)])
  }
    bu[1,] <- p.tilde[1, ]
    p <- p.tilde[-1, ]
  
  # Step 3: Fix p, solve q
    # define new factors
    p.tilde <- rbind(rep(1,U), p)
    colnames(p.tilde) <- as.character(1:U)
    q.tilde <- rbind(bi,q)
    
    for (i in 1:I) {
      ri <- train[train$movieId==i,] %>% arrange(userId, rating)
      q.tilde[,i] <- solve(p.tilde[,colnames(p.tilde) %in% sort(train[train$movieId==i,]$userId)] %*%
                  t(p.tilde[,colnames(p.tilde) %in% sort(train[train$movieId==i,]$userId)]) + lambda* diag(f+1)) %*%
                  p.tilde[,colnames(p.tilde) %in% sort(train[train$movieId==i,]$userId)] %*% 
                  (ri$rating - mu - bu[,colnames(bu) %in% sort(train[train$movieId==i,]$userId)])
      
    }
    bi[1,] <- q.tilde[1,]
    q <- q.tilde[-1,]
    
    # Summerize
    cat("iter:", l, "\t")
    est_rating <- t(q) %*% p + mu + bu[1,] + rep(bi[1,], each = ncol(p))
    rownames(est_rating) <- levels(as.factor(data$movieId))
    
    train_RMSE_cur <- RMSE(train, est_rating)
    cat("training RMSE:", train_RMSE_cur, "\t")
    train_RMSE <- c(train_RMSE, train_RMSE_cur)
    
    test_RMSE_cur <- RMSE(test, est_rating)
    cat("test RMSE:",test_RMSE_cur, "\n")
    test_RMSE <- c(test_RMSE, test_RMSE_cur)
    
    
  }
    
    
  
}
