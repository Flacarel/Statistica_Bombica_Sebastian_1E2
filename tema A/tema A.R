#A1
#a

plot_probability_functions <- function(lambda, p, n, k) {
  x <- k:n
  poisson_prob <- dpois(x, lambda)
  geometric_prob <- dgeom(x, p)
  binomial_prob <- dbinom(x, n, p)
  
  data <- data.frame(x = rep(x, 3), 
                     Probability = c(poisson_prob, geometric_prob, binomial_prob), 
                     Distribution = rep(c("Poisson", "Geometric", "Binomial"), each = length(x)))
  
  barplot(data$Probability, names.arg = data$x, col = rep(c("red", "blue", "green"), each = length(x)),
          main = "Mass Probability Functions", xlab = "x", ylab = "Probability")
  legend("topright", legend = c("Poisson", "Geometric", "Binomial"), fill = c("red", "blue", "green"))
}
plot_probability_functions(lambda = 2, p = 0.3, n = 10, k = 2)

#b


p_odd <- pgeom(1, 0.1, lower.tail = FALSE)
p_greater_than_4 <- pgeom(3, 0.3, lower.tail = FALSE)
p_less_than_or_equal_to_20 <- pgeom(20, 0.2)

print(p_odd)
print(p_greater_than_4)
print(p_less_than_or_equal_to_20)



#c

lambda <- 2
k0 <- qpois(1 - 10^-7, lambda, lower.tail = FALSE)

print(k0)

#A2

#a


calculate_statistics <- function(file_name) {
  data <- read.csv("note.csv",header = T)
  
  P <- data$P
  S <- data$S
  median_P <- median(P)
  median_S <- median(S)
  mean_P <- mean(P)
  mean_S <- mean(S)
  sd_P <- sd(P)
  sd_S <- sd(S)
  quartile_P <- quantile(P, probs = c(0.25, 0.75))
  quartile_S <- quantile(S, probs = c(0.25, 0.75))
  cat("P - Mediana:", median_P, "\n")
  cat("P - Medie:", mean_P, "\n")
  cat("P - Deviația standard:", sd_P, "\n")
  cat("P - Cvartile:", quartile_P, "\n")
  cat("\n")
  cat("S - Mediana:", median_S, "\n")
  cat("S - Medie:", mean_S, "\n")
  cat("S - Deviația standard:", sd_S, "\n")
  cat("S - Cvartile:", quartile_S, "\n")
}
calculate_statistics("note.csv")

#b

remove_outliers <- function(file_name, sample_name) {
  data <- read.csv("note.csv",header=T)
  sample <- data[[sample_name]]
  q1 <- quantile(sample, probs = 0.25)
  q3 <- quantile(sample, probs = 0.75)
  iqr <- q3 - q1
  
  lower_threshold <- q1 - 1.5 * iqr
  upper_threshold <- q3 + 1.5 * iqr
  cleaned_sample <- sample[sample >= lower_threshold & sample <= upper_threshold]
  
  return(cleaned_sample)
}
cleaned_P <- remove_outliers("note.csv", "P")
cleaned_S <- remove_outliers("note.csv", "S")

#c

plot_histogram <- function(file_name, sample_name) {
  data <- read.csv("note.csv",header=T)
  sample <- data[[sample_name]]
  cleaned_sample <- remove_outliers(file_name, sample_name)
  hist(cleaned_sample, breaks = 9, xlab = "Interval", ylab = "Frecvență", main = sample_name)
}
plot_histogram("note.csv", "P")
plot_histogram("note.csv", "S")