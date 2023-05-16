functie_aleatoare <- function(val, prob) 
{
  if (length(val) != length(prob)) 
  {
    stop("Error:numaru de prob nu e acelasi cu numar de val")
  }
  if (any(prob < 0) || sum(prob) != 1) 
  {
    stop("Error:suma prob nu este egala cu 1/prob nu sunt pozitive")
  }
  interval <- c(0, cumsum(prob))
  r <- runif(1) # 0<=r<=1
  for (i in 1:(length(interval) - 1)) {
    if (interval[i] < r && r <= interval[i + 1]) {
      return(val[i])
    }
  }
  
  stop("Error")
}

val <- c(1, 2, 3)
prob <- c(0.5, 0.4, 0.1)

rezultat <- functie_aleatoare(val, prob)
print(rezultat)
