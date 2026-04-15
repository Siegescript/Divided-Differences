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

# ====================== NEWTON FORM AS STRING (just like you wrote it) ======================
newton_form_string <- function(x, dd, simplify_signs = TRUE) {
  n <- length(x)
  # Start with the constant term
  str <- sprintf("%.4g", dd[1, 1])
  
  prod_str <- ""
  for (k in seq_len(n - 1)) {
    xi <- x[k]
    # Make it pretty like your classwork: (x + 1) instead of (x - (-1))
    if (simplify_signs && xi < 0) {
      factor <- sprintf("(x + %g)", -xi)
    } else if (abs(xi) < 1e-8) {
      factor <- "(x)"
    } else {
      factor <- sprintf("(x - %g)", xi)
    }
    prod_str <- paste0(prod_str, factor)
    
    a_k <- dd[1, k + 1]
    # Show coefficient exactly like your notes (even if it's 1)
    if (a_k >= 0) {
      str <- paste0(str, " + ", a_k, prod_str)
    } else {
      str <- paste0(str, " - ", abs(a_k), prod_str)
    }
  }
  str
}

# ====================== MONOMIAL STRING ======================
format_monomial <- function(coeffs, var = "x") {
  coeffs <- round(coeffs, 8)        # clean any floating-point noise
  deg <- length(coeffs) - 1
  if (deg < 0) return("0")
  
  terms <- character(0)
  for (power in deg:0) {
    i <- power + 1
    c <- coeffs[i]
    if (abs(c) < 1e-8) next
    
    abs_c <- abs(c)
    coef_str <- if (abs(abs_c - 1) < 1e-8 && power > 0) "" else sprintf("%.4g", abs_c)
    
    if (power == 0) {
      term <- coef_str
    } else if (power == 1) {
      term <- paste0(coef_str, if (nchar(coef_str) > 0) " " else "", var)
    } else {
      term <- paste0(coef_str, if (nchar(coef_str) > 0) " " else "", var, "^", power)
    }
    
    # First term (highest degree) gets no leading "+" 
    if (length(terms) == 0) {
      prefix <- if (c < 0) "-" else ""
    } else {
      prefix <- if (c > 0) " + " else " - "
    }
    terms <- c(terms, paste0(prefix, term))
  }
  result <- paste(terms, collapse = "")
  if (result == "") "0" else result
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

cat("\nNewton interpolation polynomial:\n")
print(newton_form_string(x_points, dd_table))

cat("\nSimplified:\n")
mon_coeffs <- newton_to_monomial_coeffs(x_points, dd_table)
print(format_monomial(mon_coeffs))