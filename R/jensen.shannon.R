#' @encoding UTF-8
#' @title Jensen-Shannon Distance
#'
#' @description The Jensen-Shannon divergence or distance matrix stores the \eqn{n*(n-1)/2} pairwise distances/similarities between observations in an \eqn{n x p} matrix where n correspond to the independent observational units and p represent the covariates measured on each individual.
#' @param mat An n x p matrix.
#' @examples
#'# create a matrix
#' n  = 10
#' m = matrix(runif(n*10), ncol = 10)
#' m = m/rowSums(m)
#' jensen.shannon(m)
#' @export
jensen.shannon <- function(mat) {
  kld = function(p,q) sum(ifelse(p == 0 | q == 0, 0, log(p/q)*p))
  res = matrix(0, nrow(mat), nrow(mat))
  for (i in 1:(nrow(mat) - 1)) {
    for (j in (i+1):nrow(mat)) {
      m = (mat[i,] + mat[j,])/2
      d1 = kld(mat[i,], m)
      d2 = kld(mat[j,], m)
      res[j,i] = sqrt(.5*(d1 + d2))
    }
  }
  res
}
NULL
