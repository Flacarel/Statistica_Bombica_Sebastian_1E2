LLN_Student = function(n,r) {
  return(mean(rt(n,df=r)));
}

n_values <- c(1000, 10000, 100000, 1000000)
r_values <- c(2, 3, 4, 5)

for (n in n_values) 
{
  for (r in r_values) 
  {
    print(LLN_Student(n, r))
  }
}
