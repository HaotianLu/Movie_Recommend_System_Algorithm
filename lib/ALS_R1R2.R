#Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[1]), as.character(obs[2])])^2
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

  
  # Initialize (q): assigning the average rating for that movie as the first row, 
  # and small random numbers for the remaining entries 
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- levels(as.factor(data$userId))
  
  # Initialize (bi)
  bi <- matrix(runif(I, -1, 1), ncol = I)
  colnames(bi) <- levels(as.factor(data$movieId))
  
  # Initialize (bu)
  bu <- matrix(runif(U, -1, 1), ncol=U)
  colnames(bu) <- levels(as.factor(data$userId))
  
  # grand mean
  mu <- mean(train$rating)
  movie.id <- sort(unique(data$movieId))
  
  train_RMSE <- c()
  test_RMSE <- c()
  
  
  for (l in 1:max.iter){
    
  # Step 2: Fix q, solve p
    # define new factors
    q.tilde <- rbind(bi, rep(1,I), q)
    colnames(q.tilde) <- levels(as.factor(data$movieId))
    p.tilde <- rbind(rep(1,U), bu, p)
    colnames(p.tilde) <- levels(as.factor(data$userId))

  for (u in 1:U) {
    ru <- train[train$userId==u,] %>% arrange(movieId,rating)
    p.tilde[,u] <- solve(q.tilde[,as.character(sort(train[train$userId==u,]$movieId))] %*% 
            t(q.tilde[,as.character(sort(train[train$userId==u,]$movieId))]) + lambda * diag(f+2)) %*%
            q.tilde[,as.character(sort(train[train$userId==u,]$movieId))] %*% 
            (ru$rating-mu)
  }
    bu[1,] <- p.tilde[2, ]
    p <- p.tilde[-c(1,2), ]
  
  # Step 3: Fix p, solve q
    # define new factors
    p.tilde <- rbind(rep(1,U), bu, p)
    colnames(p.tilde) <- levels(as.factor(data$userId))
    q.tilde <- rbind(bi, rep(1,I), q)
    colnames(q.tilde) <- levels(as.factor(data$movieId))
    
    for (i in 1:I) {
      ri <- train[train$movieId==movie.id[i],] %>% arrange(userId, rating)
      q.tilde[,i] <- solve(p.tilde[,as.character(sort(train[train$movieId==movie.id[i],]$userId))] %*%
                  t(p.tilde[,as.character(sort(train[train$movieId==movie.id[i],]$userId))]) + lambda* diag(f+2)) %*%
                  p.tilde[,as.character(sort(train[train$movieId==movie.id[i],]$userId))] %*% 
                  (ri$rating-mu)
      
    }
    bi[1,] <- q.tilde[1,]
    q <- q.tilde[-c(1,2),]
    
    # Summerize
    cat("iter:", l, "\t")
    est_rating <- t(p) %*% q + mu + bu[1,] + rep(bi[1,], each = ncol(p))
    colnames(est_rating) <- levels(as.factor(data$movieId))
    
    train_RMSE_cur <- RMSE(train, est_rating)
    cat("training RMSE:", train_RMSE_cur, "\t")
    train_RMSE <- c(train_RMSE, train_RMSE_cur)
    
    test_RMSE_cur <- RMSE(test, est_rating)
    cat("test RMSE:",test_RMSE_cur, "\n")
    test_RMSE <- c(test_RMSE, test_RMSE_cur)
    
    
  }
  
  return(list(p = p, q = q, bi = bi, bu = bu, train_RMSE = train_RMSE, test_RMSE = test_RMSE))

}
