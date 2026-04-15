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