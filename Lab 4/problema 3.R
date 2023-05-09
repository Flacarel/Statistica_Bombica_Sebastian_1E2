
lambda1 <- 4 # rata pt primu mecanic
lambda2 <- 12 # rata pt al doilea mecanic
prob_faster <- 3/4 # probabilitatea de servire a mecanicului mai rapid


if(runif(1) < prob_faster) { #generarea timpului X
  X <- rexp(1, lambda2)
} else {
  X <- rexp(1, lambda1)
}

#Afisam X-ul
cat("Service time:", X, "hours\n")

#aici facem numaru de simulari
n_sim <- 10000

# aici calculam average-ul
X_vals <- numeric(n_sim)
for(i in 1:n_sim) {
  if(runif(1) < prob_faster) {
    X_vals[i] <- rexp(1, lambda2)
  } else {
    X_vals[i] <- rexp(1, lambda1)
  }
}
#prindam average-ul
cat("Expected service time:", mean(X_vals), "hours\n")
