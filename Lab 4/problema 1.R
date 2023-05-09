#definesc functia
f <- function(x) -2*x^2 + 5*x - 2

# definesc "limitele" domeniului care i dreptunghi
x_min <- 0
x_max <- 2
y_min <- 0
y_max <- max(f(seq(x_min, x_max, length.out=100)))

# cate puncte sa faca
n_points <- 10000

# aici generez puncte random care sa fie IN domeniu
x_points <- runif(n_points, x_min, x_max)
y_points <- runif(n_points, y_min, y_max)

# numaru de puncte care sunt sub curba
n_points_under_curve <- sum(y_points < f(x_points))

# folosesc monte carlo
rect_area <- (x_max - x_min) * (y_max - y_min)
curve_area <- rect_area * n_points_under_curve / n_points

# acm aria exacta cu integraal
exact_area <- integrate(f, x_min, x_max)$value

relative_error <- abs(exact_area - curve_area) / exact_area

cat("Estimated area:", curve_area, "\n")
cat("Exact area:", exact_area, "\n")
cat("Relative error:", relative_error, "\n")
