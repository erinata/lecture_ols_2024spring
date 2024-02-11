rm(list=ls())

generate_sample <- function(n, mu_x1,sigma_x1,mu_x2,sigma_x2,mu_epsilon,sigma_epsilon,beta0,beta1,beta2){
  x1 <- rnorm(n, mu_x1, sigma_x1)
  x2 <- rnorm(n, mu_x2, sigma_x2)
  epsilon <- rnorm(n, mu_epsilon, sigma_epsilon)
  
  y <- beta0 + beta1*x1 + beta2*x2 +epsilon
  data.frame(y=y, x1=x1, x2=x2)
}


generate_estimate <- function(dataset) {
  n <- dim(dataset)[1]
  y <- matrix(dataset$y, ncol=1)
  X <- matrix(c(rep(1,n), dataset$x1, dataset$x2), ncol=3)
  p <- dim(X)[2]
  
  beta_hat <- solve(t(X)%*%X)%*%(t(X)%*%y)
  print(beta_hat)
  
  y_hat <- X%*%beta_hat
  epsilon_hat <- y - y_hat
  
  sigma_sq_hat <- sum((epsilon_hat - (sum(epsilon_hat)/n))^2)/(n-p)
  var_cov_hat <- sigma_sq_hat*solve(t(X)%*%X)
  se <- sqrt(diag(var_cov_hat))
  print(se)
}


mu_x1 <- 1
sigma_x1 <- 10
mu_x2 <- 2
sigma_x2 <- 20
mu_epsilon <- 0
sigma_epsilon <-1
beta0 <- 1
beta1 <- 2
beta2 <- -3

n<-100

dataset <- generate_sample(n, mu_x1,sigma_x1,mu_x2,sigma_x2,mu_epsilon,sigma_epsilon,beta0,beta1,beta2)

generate_estimate(dataset)

summary(lm(dataset$y~ dataset$x1 + dataset$x2))





