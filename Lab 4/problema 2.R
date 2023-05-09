#b
MC_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, 1, 4);
    sum = sum + exp(x);
  }
  return(3*sum/N);
}

MC_integr_average= function(k, N) {
  estimates=0;
  for(i in 1:k)
    estimates[i] = MC_integration(N);
  
  # Aria exactă 
  f1 <- function(x) exp(x);
  exact_area1 <- exp(4) - exp(1);
  
  mean_est <- mean(estimates);
  sd_est <- sd(estimates);
  abs_err <- abs(mean_est - exact_area1);
  rel_err <- abs_err / exact_area1;
  cat("Estimarea MC: ", mean_est, "\n");
  cat("Eroarea absoluta: ", abs_err, "\n");
  cat("Eroarea relativa: ", rel_err, "\n");
}
#d
#Aria exactă 
f2 <- function(x) 1/(2*x-1)^2;
exact_area2 <- log(3/4);

MC_integration2 = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, 0, 1);
    sum = sum + f2(x)/(2*x-1)^2;
  }
  return(sum/N);
}

MC_integr_average2= function(k, N) {
  estimates=0 # initialize the vector of estimates
  for(i in 1:k)
    estimates[i] = MC_integration2(N);
  
  mean_est <- mean(estimates)
  sd_est <- sd(estimates)
  abs_err <- abs(mean_est - exact_area2)
  rel_err <- abs_err / exact_area2
  cat("Estimarea MC: ", mean_est, "\n")
  cat("Eroarea absoluta: ", abs_err, "\n")
  cat("Eroarea relativa: ", rel_err, "\n")
}