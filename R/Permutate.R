#' @encoding UTF-8
#' @title  Create k random permutations of a vector
#' @description Creates a k random permutation of a vector.
#' @details should be used only for length(input)! >> k
#' @param x a vector to be permutated.
#' @param k number of permutations to be conducted.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @keywords Sampling
#' @examples
#' #  5! = 5 x 4 x 3 x 2 x 1 = 120
#' # row wise permutations
#' Permutate(x=1:5, k=5)
#' @export
`Permutate` <- function(x, k){
  n <- length(x)
  mat <- matrix(data=NA,nrow=k,ncol=n) # allocate memory
.npermutate <- function(v){
  tab <- table(v); # count occurrences of each element
  occurrences <- tab[tab>1]; # get those greater than 1
  numerator <- lfactorial(length(v))
  if(length(occurrences ) > 0){
    denominator <- sum(sapply(occurrences , lfactorial))
  } else {
    denominator <- 0
  }
 exp(numerator-denominator)
}
  k <- min(k, .npermutate(x))
  inserted <- 0
  while(inserted < k){
    p <- sample(x)
    # check if the vector has already been inserted
    if(sum(apply(mat,1,identical,p)) == 0){
      mat[inserted+1,] <- p
      inserted <- inserted+1
    }
  }
cat("\n")
cat("There are", .npermutate(x), "possible outcomes, using", k, "\n")
cat("\n")
  return(mat)
}
NULL
