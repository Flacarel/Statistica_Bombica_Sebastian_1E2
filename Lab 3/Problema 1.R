densitate <- function(mu, sigma) {
  x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)
  y <- dnorm(x, mean = mu, sd = sigma)
  plot(x, y, type = "l", xlab = "x", ylab = "densitate", main = "variatie")
}
