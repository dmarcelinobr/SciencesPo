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
  k <- min(k, nperm(input))
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
