#D1

MC_find_M_element = function(x, k) {
  i = 0
  while (i < k) {
    number = sample(x, 1)
    count = 0
    j = 1
    while (j <= length(x)) {
      if (x[j] == number) {
        count = count + 1
      }
      j = j + 1
    }
    if (count >= length(x) / 2 + 1) {
      return (number)
    }
    i = i + 1
  }
  return ("x has no M-element")
}
x = sample(1:2, 100, replace = TRUE)
MC_find_M_element(x, 24)

#D2

element_ith = function(i, A) {
  z = sample(A, 1)
  Alt = A[A < z]
  Agt = A[A > z]
  if (length(Alt) > i) {
    return (element_ith(i, Alt))
  }
  else {
    if (length(A) > i + length(Agt)) {
      return (z);
    }
    else {
      return (element_ith(i - length(A) + length(Agt), Agt))
    }
  }
}
element_ith(54, x)
sort(x)[55]
element_ith(55, x)
sort(x)[56]

#D3

MC_median = function(S, a) {
  if (a < 0) {
    a = -a
  }
  m = floor(a * log(length(S)))
  SS = sample(S, m)
  sorted_SS = sort(SS)
  median_index = ceiling(m / 2)
  return (sorted_SS[median_index])
}

x = runif(4473, 0, 100)#4473 este aproximare a radical(2*10^7)
median(x)
MC_median(x, 532)