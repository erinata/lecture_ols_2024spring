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
  
  beta_hat <- solve(t(X)%*%X)%*%(t(X)%*%y)
  
  beta_hat[[2]]

}


generate_sample_and_estimate_N_times <- function(N, n, mu_x1,sigma_x1,mu_x2,sigma_x2,mu_epsilon,sigma_epsilon,beta0,beta1,beta2){
  result <- sapply(1:N, function(x) {
    dataset <- generate_sample(n, mu_x1,sigma_x1,mu_x2,sigma_x2,mu_epsilon,sigma_epsilon,beta0,beta1,beta2)
    generate_estimate(dataset)

  })
}






N<-300
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
n_list <- ceiling(seq(from=20, to=1000, length.out = 500))


point_estimates_list <- c()
variance_list <- c()
for (n in n_list){
  estimates <- generate_sample_and_estimate_N_times(N, n, mu_x1,sigma_x1,mu_x2,sigma_x2,mu_epsilon,sigma_epsilon,beta0,beta1,beta2)
  point_estimates_list <- append(point_estimates_list, mean(estimates))
  variance_list <- append(variance_list, var(estimates))
}


png(filename="beta1_hat_point_estimate.png")
plot(n_list, point_estimates_list)
dev.off()

png(filename="beta1_hat_variance.png")
plot(n_list, variance_list)
dev.off()
















