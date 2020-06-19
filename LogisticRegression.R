# implementing sigmoid function for logistic regression
sigmoid <-function(z)
{
  sigmoid_hyp <- 1/(1+exp(-z))
}

h <- function(wt,input) {
  
 prob_x<-sigmoid(input %*% wt)
 if(prob_x > threshold)
 {
   y_pred = 1
   else{
     y_pred = 0
   }
 }
  
}
# z = input (B0 +B1xi)
#loss function and maximum loglikelihood estimates

j_loss<- function(input, y, wt) {
   
  (1/length(y)) * sum(-y*log(h(wt,input))-(1-y)*log(1-h(wt,input)))
}

backward_propogation<- function()
{
  
  d_i <- 2*()
  
}

gradient_descent<- function()
{
  
}

logistic_reg <- function(X, y, epochs, lr)
{
  X_mat <- cbind(1, X)
  beta_hat <- matrix(1, nrow=ncol(X_mat))
  for (j in 1:epochs)
  {
    residual <- sigmoid(X_mat %*% beta_hat) - y
    # Update weights with gradient descent
    delta <- t(X_mat) %*% as.matrix(residual, ncol=nrow(X_mat))*(1/nrow(X_mat))
    beta_hat <- beta_hat - (lr*delta)
  }
  # Print log-likliehood
  print(log_likelihood(X_mat, y, beta_hat))
  # Return
  beta_hat
}