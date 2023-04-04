funct2 = function(lambda, n) {
  # calculez probabilitate poisson a primelor n
  probabilitate = dpois(0:(n-1), lambda)
  
  # fac grafic de bare
  barplot(probabilitate, names.arg = 0:(n-1), xlab = "valori", ylab = "probabilitate", 
          main = paste("Distributia =", lambda, "and n =", n))
}

funct2(5, 8)