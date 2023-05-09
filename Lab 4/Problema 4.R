MC_prob_all_infected <- function(N) {
  count_all_infected <- 0
  for (i in 1:N) {
    nr_infected <- 1
    nr_days <- 2
    last_errors <- c(18, 22, 28)
    nr_errors <- 18
    while (nr_errors > 0) {
      lambda <- min(last_errors)
      nr_errors <- rpois(1, lambda)
      last_errors <- c(nr_errors, last_errors[1:2])
      nr_infected <- nr_infected + nr_errors
      nr_days <- nr_days + 1
    }
    if (nr_infected == 40) {
      count_all_infected <- count_all_infected + 1
    }
  }
  return(count_all_infected / N)
}

MC_prob_at_least_15_infected <- function(N) {
  count_at_least_15_infected <- 0
  for (i in 1:N) {
    nr_infected <- 1
    nr_days <- 2
    last_errors <- c(18, 22, 28)
    nr_errors <- 18
    while (nr_errors > 0) {
      lambda <- min(last_errors)
      nr_errors <- rpois(1, lambda)
      last_errors <- c(nr_errors, last_errors[1:2])
      nr_infected <- nr_infected + nr_errors
      nr_days <- nr_days + 1
    }
    if (nr_infected >= 15) {
      count_at_least_15_infected <- count_at_least_15_infected + 1
    }
  }
  return(count_at_least_15_infected / N)
}

MC_prob_at_least_15_infected_error <- function(target_error, confidence) {
  N <- 10000
  p_hat <- MC_prob_at_least_15_infected(N)
  error <- 1
  while (error > target_error) {
    N <- N * 2
    p_hat_prev <- p_hat
    p_hat <- MC_prob_at_least_15_infected(N)
    error <- qnorm(1 - (1 - confidence) / 2) * sqrt(p_hat_prev * (1 - p_hat_prev) / N)
  }
  return(list(probability = p_hat, error = error))
}
# (a) Estimarea probabilitătii ca într-o anumita zi toate computerele să fie infectate
prob_all_infected <- MC_prob_all_infected(10000)

cat("Probabilitatea ca într-o anumita zi toate computerele să fie infectate:", prob_all_infected, "\n")

# (b) Estimarea probabilitătii ca intr-o anumita zi cel puțin 15 computere să fie infectate
prob_at_least_15_infected <- MC_prob_at_least_15_infected(10000)

cat("Probabilitatea ca într-o anumita zi cel puțin 15 computere să fie infectate:", prob_at_least_15_infected, "\n")

# (c) Estimarea probabilitătii ca într-o anumita zi cel puțin 15 computere să fie infectate cu o eroare de +-0.01 cu o probabilitate
target_error <- 0.01
confidence <- 0.95

result <- MC_prob_at_least_15_infected_error(target_error, confidence)

cat("Probabilitatea ca într-o anumita zi cel puțin 15 computere să fie infectate:", result$probability, "\n")
cat("Eroarea estimarii:", result$error, "\n")
