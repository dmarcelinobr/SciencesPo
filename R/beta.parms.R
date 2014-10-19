beta.parms <-
function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(parameters = list(alpha = alpha, beta = beta))
}
