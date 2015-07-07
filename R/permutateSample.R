#' @encoding UTF-8
#' @title  Create k random permutations of a vector
#' @details  should be used only for length(input)! >> k
#' @param input vector to be permutated.
#' @param k number of permutations.
#' @keywords Sampling
#' @examples permutateSample(input=1:5, k=5)
#' @export
permutateSample <- function(input,k){
  n <- length(input)
  mat <- matrix(data=NA,nrow=k,ncol=n) # allocate memory
  k <- min(k, nPermutate(input))
  inserted <- 0
  while(inserted < k){
    p <- sample(input)
    # check if the vector has already been inserted
    if(sum(apply(mat,1,identical,p)) == 0){
      mat[inserted+1,] <- p
      inserted <- inserted+1
    }
  }
  mat
}
NULL



#' @encoding UTF-8
#' @title Calculate number of permutations taking repeated elements into consideration
#'
#' @param vec The vector which number of permutations will be calculated
#' @export
#' @examples
#' myvar <- 1:10
#' nPermutate(myvar)
nPermutate <- function(vec){
  tab <- table(vec); # count occurences of each element
  occurences <- tab[tab>1]; # get those greater than 1
  numerator <- lfactorial(length(vec))
  if(length(occurences ) > 0){
    denominator <- sum(sapply(occurences , lfactorial))
  } else {
    denominator <- 0
  }
  exp(numerator-denominator)
}
NULL
