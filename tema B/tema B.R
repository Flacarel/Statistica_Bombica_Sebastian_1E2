#B1

LLN_Geom = function(n,p) {
  return(mean(rgeom(n,p)));
}

n_values <- c(5000, 10000, 100000, 500000)
p_values <- c(0.2, 0.6, 0.6, 0.8)
print("B1")

for (n in n_values) 
{
  for (p in p_values) 
  {
    print(LLN_Geom(n, p))
  }
}

#B2

CLT_Student = function(r, n, N, z) {
  expectation <- 0
  variance <- r / (r - 2)
  st_dev <- sqrt(variance)
  upper_bound <- z * st_dev / sqrt(n) + expectation
  sum <- 0
  for (i in 1:N) {
    x_n <- mean(rt(n, r))
    if (x_n <= upper_bound) {
      sum <- sum + 1
    }
  }
  return(sum / N)
}

n <- 50
N <- c(5000, 10000, 20000)
z <- c(-1.5, 0, 1.5)
print("B2:")
for (i in N) {
  for (j in z) {
    error <- abs(CLT_Student(5, n, i, j) - pt(j, 5))
    print(error)
  }
}

#B3

B3 = function(n, p, h, k) 
{
  expectation = n*p
  variance = n*p*(1 - p)
  standard_deviation = sqrt(variance)
  q1 = (h - 0.5 - expectation)/standard_deviation
  q2 = (k - 0.5 - expectation)/standard_deviation
  
  return(pnorm(q2)-pnorm(q2))
}
n <- 100
p <- 0.4
h <- 20
k <- 50
print("B3:")
print(B3(n,p,h,k))
