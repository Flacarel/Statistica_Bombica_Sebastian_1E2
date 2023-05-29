#E1

zconfidence_interval = function(alpha, n, sample_mean, sigma) {
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return (interval)
}
zconfidence_interval(1 - 0.9, 8, 138, 11)
zconfidence_interval(1 - 0.95, 8, 138, 11)
zconfidence_interval(1 - 0.99, 8, 138, 11)

#E2

zconfidence_interval(1 - 0.95, 256, 18, sqrt(1.44))

#E3

test_proportion = function(alfa)
{
  n = 153
  succeses = 17
  p0 = 0.12
  p_prim = succeses/n
  z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n)
  critical_z = qnorm(alfa,0,1)
  cat(critical_z,z_score, "\n")
  if(z_score > critical_z)
    print("Schimbarea n a fost inutila")
  else
    print("Schimbarea a fost inutila")
}

test_proportion(0.01)
test_proportion(0.05)