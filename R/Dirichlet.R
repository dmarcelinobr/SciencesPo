#' @encoding UTF-8
#' @title Dirichlet Distribution
#' @description Density function and random number generation for the Dirichlet distribution
#' @param n number of random observations to draw.
#' @param alpha the Dirichlet distribution's parameters. Can be a vector (one set of parameters for all observations) or a matrix (a different set of parameters for each observation), see \dQuote{Details}.
#'
#' If \code{alpha} is a matrix, a complete set of \eqn{\alpha}-parameters must be supplied for each observation.
#' \code{log} returns the logarithm of the densities (therefore the log-likelihood) and \code{sum.up} returns the product or sum and thereby the likelihood or log-likelihood.
#'
#' @return
#' The \code{rdirichlet} returns a matrix with \code{n} rows, each containing a single random number according to the supplied alpha vector or matrix.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords Distributions
#' @examples
#' # 1) General usage:
#' rdirichlet(20, c(1,1,1) );
#' alphas <- cbind(1:10, 5, 10:1);
#' alphas;
#' rdirichlet(10, alphas );
#' alpha.0 <- sum( alphas );
#' test <- rdirichlet(10, alphas );
#' apply( test, 2, mean );
#' alphas / alpha.0;
#' apply( test, 2, var );
#' alphas * ( alpha.0 - alphas ) / ( alpha.0^2 * ( alpha.0 + 1 ) );
#'
#' # 2) A pratical example of usage:
#' # A Brazilian face-to-face poll by Datafolha conducted on Oct 03-04
#' # with 18,116 insterviews asking for their vote preferences among the
#' # presidential candidates.
#'
#' ## First, draw a sample from the posterior
#' set.seed(1234);
#' n <- 18116;
#' poll <- c(40,24,22,5,5,4) / 100 * n; # The data
#' mcmc <- 100000;
#' sim <- rdirichlet(mcmc, alpha = poll + 1);
#'
#' ## Second, look at the margins of Aecio over Marina in the very last minute of the campaign:
#' margin <- sim[,2] - sim[,3];
#' mn <- mean(margin); # Bayes estimate
#' mn;
#' s <- sd(margin); # posterior standard deviation
#'
#' qnts <- quantile(margin, probs = c(0.025, 0.975)); # 90% credible interval
#' qnts;
#' pr <- mean(margin > 0); # posterior probability of a positive margin
#' pr;
#'
#' ## Third, plot the posterior density
#' hist(margin, prob = TRUE, # posterior distribution
#'   breaks = "FD", xlab = expression(p[2] - p[3]),
#'   main = expression(paste(bold("Posterior distribution of "), p[2] - p[3])));
#' abline(v=mn, col='red', lwd=3, lty=3);

#' @useDynLib SciencesPo
#' @aliases Dirichlet
#' @export
`rdirichlet` <- function(n,
                         alpha) {
  if (((n %% 1) != 0) | (n <= 0))
    stop("n must be an integer > 0")
  if (any(alpha <= 0))
    stop("all values in alpha must be > 0")
  .vec <- is.vector(alpha)
  .mat <- is.matrix(alpha)
  if (!.vec & !.mat) {
    stop("alpha must be a vector or a matrix")
  } else if (.vec & !.mat) {
    X <- .Call("rdirichlet_vector", n, alpha, NAOK=TRUE, PACKAGE="SciencesPo")
  } else {
    if (n != nrow(alpha))
      stop("when alpha is a matrix, the number of its rows must be equal to n")
    X <- .Call("rdirichlet_matrix", n, alpha, dim(alpha), NAOK=TRUE, PACKAGE="SciencesPo")
  }
  return(X)
}
NULL



#' @encoding UTF-8
#' @title Dirichlet distribution
#' @description Density function and random number generation for the Dirichlet distribution
#' @param x a matrix containing observations.
#' @param alpha the Dirichlet distribution's parameters. Can be a vector (one set of parameters for all observations) or a matrix (a different set of parameters for each observation), see \dQuote{Details}.
#' @return the \code{ddirichlet} returns a vector of densities (if \code{sum = FALSE}) or the (log-)likelihood (if \code{sum = TRUE}) for the given data and alphas.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @param log if \code{TRUE}, logarithmic densities are returned.
#' @param sum if \code{TRUE}, the (log-)likelihood is returned.
#' @keywords Distributions
#' @examples
#' mat <- cbind(1:10, 5, 10:1);
#' mat;
#' x <- rdirichlet(10, mat);
#'
#' ddirichlet(x, mat);
#'
#' @useDynLib SciencesPo
#' @export
`ddirichlet` <- function(x, alpha, log = FALSE, sum = FALSE) {
  if (is.null(dim(x)))
    stop("x must be a matrix")
  x_dims <- dim(x)
  if (any(alpha <= 0))
    stop('all values in alpha must be > 0.')

  res <- if (is.vector(alpha)) {
    .Call("ddirichlet_log_vector", x, alpha, dim(x), AOK=TRUE, PACKAGE="SciencesPo")
  } else {
    if (any(dim(alpha) != dim(x)))
      stop("check if x and alpha are correctly specified")
    .Call("ddirichlet_log_matrix", x, alpha, dim(x), dim(alpha), AOK=TRUE, PACKAGE="SciencesPo")
  }

  if (sum) {
    if (log)
      return(sum(res))
    else
      return(exp(sum(res)))
  } else {
    if (log)
      return(res)
    else
      return(exp(res))
  }
}
NULL





`rDirichlet` <- function(n, alpha) {
  l = length(alpha)
  theta = matrix(0, n, l)
  for (j in 1:l) {
    theta[, j] = stats::rgamma(n, alpha[j], 1)
  }
  theta = theta / apply(theta, 1, sum)
  return(theta)
}
NULL
