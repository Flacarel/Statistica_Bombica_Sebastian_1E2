funct = function(p, n) {
  # calculez probabilitățile primei n valori ale distribuției geometrice cu o probabilitate de succes de p
  probabilitate = dgeom(1:n, prob = p)
  
  # trasez un grafic cu bare
  barplot(probabilitate, names.arg = 1:n, xlab = "valori", ylab = "probabilitate", 
          main = paste("distributie geometrica =", p, "and n =", n))
}
funct(0.25, 7)




