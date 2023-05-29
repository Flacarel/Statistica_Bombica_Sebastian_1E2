#C1

MC_volume_fn <- function(N, a) {
  N_C <- 0;
  sqrt_a<-sqrt(a);
  for (i in 1:N) {
    x1 <- runif(1, -sqrt_a, sqrt_a);
    x2 <- runif(1, -sqrt_a, sqrt_a);
    x3 <- x1*x1 + x2*x2;
    if(x3 <= a)
      N_C <- N_C + (a - x3);
  }
  return (4*a * N_C / N)
}
apel_MC<- function(N, a) 
{
  pi = 3.1415926;
  MC_volume <- MC_volume_fn(N, a);
  exact_area = pi*a*a/2
  abs_err <- abs(MC_volume - exact_area)
  relative_error <- abs_err/exact_area
  cat("\nestimated area is", MC_volume)
  cat("\nabsolute error is", abs_err)
  cat("\nrelative error is", relative_error)
}
apel_MC(10000, 2);
apel_MC(10000, 4);
apel_MC(10000, 10);
apel_MC(20000, 2);
apel_MC(20000, 4);
apel_MC(20000, 10);
apel_MC(50000, 2);
apel_MC(50000, 4);
apel_MC(50000, 10)

#C2

MC_area_fn <- function(N)
{
  N_C <- 0;
  for (i in 1:N) {
    x = runif(1, 0, 4);
    if(x<=3)
    {
      y <- (x+6)/3;
    }
    else y <- 3*(4-x);
    N_C <- N_C + y;
    
  }
  return (4 * N_C/N)
}
MC_area <- MC_area_fn(30000);
exact_area = 9; # (3+2)*3/2 + 3*1/2
abs_err <- abs(MC_area - exact_area)
relative_error <- abs_err/exact_area
cat("\nestimated area is", MC_area)
cat("\nabsolute error is", abs_err)
cat("\nrelative error is", relative_error)

#C3

#a)

MC_area_fn <- function(N, a, b) {
  N_C <- 0;
  for (i in 1:N) {
    x <- runif(1, a, b);
    N_C <- N_C +( (x + 1) /sqrt(4 - x*x) );
  }
  return ((b - a) * N_C / N)
}
MC_integral <- MC_area_fn(10000, -1, 1)
exact_area <- integrate(function(x) ((x + 1) /sqrt(4 - x*x)), -1, 1)$value
abs_err <- abs(MC_integral - exact_area)
relative_error <- abs_err/exact_area
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", relative_error)

#b)

integrala <- function(N, a) {
  N_C <- 0;
  for (i in 1:N) {
    x <- -runif(1, 0, a);
    N_C <- N_C + 1/(4+x*x) 
  }
  return (a * N_C / N)
}
MC_integral <- integrala(100000, 1000);
exact_area <- integrate(function(x)  1/(4+x*x), -Inf, 0)$value
abs_err <- abs(MC_integral - exact_area)
relative_error <- abs_err/exact_area
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", relative_error)

#c)

integrala <- function(N, a) {
  N_C <- 0;
  for (i in 1:N) {
    x <- -runif(1, 0, a);
    N_C <- N_C + x*exp(x);
  }
  return (a * N_C / N)
}
MC_integral <- integrala(100000, 1000);
exact_area <- integrate(function(x)  x*exp(x), -Inf, 0)$value
abs_err <- abs(MC_integral - exact_area)
relative_error <- abs_err/exact_area
cat("estimated area is", MC_integral)
cat("absolute error is", abs_err)
cat("relative error is", relative_error)

#C4
           #(disclaimer) ia foarte mult timp ca compileze c4 si solicita laptopul 
#a
m <- 100000
n <- 500
p <- 0.5
q <- 0.1

num_simulations <- 1000
total_days <- 0

for (i in 1:num_simulations) {
  false_accounts <- m
  
  while (false_accounts > 0) {
    false_accounts_added <- rbinom(1, n, p)
    
    for (j in 1:false_accounts_added) {
      if (runif(1) > q) {
        false_accounts <- false_accounts + 1
      }
    }
    
    false_accounts <- false_accounts - false_accounts_added
    total_days <- total_days + 1
  }
}

average_days <- total_days / num_simulations

cat("Numărul mediu de zile necesare până când nu mai există conturi false în rețea:", average_days, "\n")


#b

num_simulations <- 1000
count <- 0

for (i in 1:num_simulations) {
  false_accounts <- m
  
  for (j in 1:40) {
    false_accounts_added <- rbinom(1, n, p)
    
    for (k in 1:false_accounts_added) {
      if (runif(1) > q) {
        false_accounts <- false_accounts + 1
      }
    }
    
    false_accounts <- false_accounts - false_accounts_added
  }
  
  if (false_accounts <= 50000) {
    count <- count + 1
  }
}

probability_40_days <- count / num_simulations

cat("Probabilitatea ca după 40 de zile să existe cel mult 50000 de conturi false:", probability_40_days, "\n")



#c


num_simulations <- 1000
count <- 0
error <- 1
desired_error <- 0.01
desired_probability <- 0.99

while (error > desired_error || count < desired_probability * num_simulations) {
  false_accounts <- m
  current_simulations <- 0
  count_inside <- 0
  
  while (current_simulations < num_simulations) {
    false_accounts_added <- rbinom(1, n, p)
    
    for (i in 1:false_accounts_added) {
      if (runif(1) > q) {
        false_accounts <- false_accounts + 1
      }
    }
    
    false_accounts <- false_accounts - false_accounts_added
    
    if (false_accounts <= 50000) {
      count_inside <- count_inside + 1
    }
    
    current_simulations <- current_simulations + 1
  }
  
  count <- count + count_inside
  error <- abs(count / (num_simulations * current_simulations) - probability_40_days)
  num_simulations <- num_simulations * 10
}

cat("Probabilitatea cu o eroare de cel mult ±0.01 și o probabilitate de 0.99:", count / (num_simulations * current_simulations), "\n")