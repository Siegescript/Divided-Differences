# ====================== DIVIDED DIFFERENCES ALGORITHM ======================
newton_divided_diff <- function(x, y) {
  n <- length(x)
  dd <- matrix(0, nrow = n, ncol = n)
  dd[, 1] <- y                    # zeroth-order differences
  
  for (j in 2:n) {                # j = order + 1
    for (i in 1:(n - j + 1)) {
      dd[i, j] <- (dd[i + 1, j - 1] - dd[i, j - 1]) / (x[i + j - 1] - x[i])
    }
  }
  return(dd)
}

# ====================== NEWTON POLYNOMIAL BUILDER ======================
newton_polynomial <- function(x, y) {
  dd <- newton_divided_diff(x, y)
  n <- length(x)
  
  # Returns a function P(t) that evaluates the Newton polynomial
  function(t) {
    result <- dd[1, 1]                    # f[x0]
    prod_term <- 1
    
    for (k in 1:(n - 1)) {
      prod_term <- prod_term * (t - x[k])
      result <- result + dd[1, k + 1] * prod_term
    }
    return(result)
  }
}

# ====================== MULTIPLY POLY BY (x - r) ======================
multiply_by_linear <- function(p, r) {
  len <- length(p)
  new_p <- numeric(len + 1)
  new_p[2:(len + 1)] <- p          # x * p
  new_p[1:len] <- new_p[1:len] - r * p   # - r * p
  new_p
}

# ====================== EXPAND NEWTON → MONOMIAL COEFFICIENTS ======================
newton_to_monomial_coeffs <- function(x, dd) {
  n <- length(x)
  poly <- c(dd[1, 1])               # start with constant
  pi_coeffs <- c(1)                 # current product starts as 1
  
  for (k in seq_len(n - 1)) {
    pi_coeffs <- multiply_by_linear(pi_coeffs, x[k])
    a_k <- dd[1, k + 1]
    # pad if needed
    if (length(pi_coeffs) > length(poly)) {
      poly <- c(poly, rep(0, length(pi_coeffs) - length(poly)))
    }
    poly[seq_along(pi_coeffs)] <- poly[seq_along(pi_coeffs)] + a_k * pi_coeffs
  }
  poly
}

# ====================== EXAMPLE ======================
# Sample points
x_points <- c(-1, 2, 3, 5)
y_points <- c(-3, 3, 13, 81)

# Build the table
dd_table <- newton_divided_diff(x_points, y_points)

# Show the full table
cat("Divided Difference Table:\n")
print(dd_table)

# The leading diagonal gives coefficients:
cat("\nNewton coefficients (f[x0], f[x0,x1], ...):\n")
print(dd_table[1, ])