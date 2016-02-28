#' @title Split Data into Test and Train Sets 
#' @description Split data given a vector \code{x} into two sets using a predefined ratio while preserving relative ratios of different labels in \code{x}. It is often used to split data for classification models into train and test subsets.
#' @param x Vector of data labels.
#' @param ratio The spliting ratio. Note that: if (0<=ratio<1), then \code{ratio} fraction of points from \code{x} will be set to \code{TRUE}. if (ratio==1) then one random point from \code{x} will be set to \code{TRUE}. if (ratio>1) then \code{ratio} number of points from \code{x} will be set to \code{TRUE}.
#' @param group Optional vector/list used when multiple copies of each sample are present.  
#' 
#' @examples 
#' data(titanic)
#' y = titanic[,4] # extract labels from the data
#' set.seed(88)
#' survived = data.split(y, ratio=3/5)
#' table(y,survived)
#'
#' t=sum( survived)  # n of elements in one class
#' f=sum( !survived) # n of elements in the other class
#' stopifnot( round((t+f)*3/5) == t) # test ratios
#' 
#' # use results of data.split to subset data into train and test sets
#' train = subset(titanic, survived == TRUE)
#' test  = subset(titanic, survived == FALSE) 
#' 
#' @export
`data.split` <- function (x, ratio = 2/3, group = NULL) 
{
  N = length(x)
  n = length(group)
  if (n > 0 && n != N) 
    stop("Error in data.split: Vectors 'x' and 'group' have to have the same length")
  BinOne = logical(N)
  ratio = abs(ratio)
  if (ratio >= N) 
    stop("Error in data.split: 'ratio' parameter has to be i [0, 1] range or [1, length(.data)] range")
  U = unique(x)
  nU = length(U)
  if (2 * nU > N | nU == 1) {
    nh = if (ratio >= 1) 
      ratio
    else ratio * N
    rnd = runif(N)
    if (n) 
      split(rnd, group) <- lapply(split(rnd, group), mean)
    ord = order(rnd)
    BinOne[ord[1:nh]] = TRUE
  }
  else {
    rat = if (ratio >= 1) 
      ratio/N
    else ratio
    for (iU in 1:nU) {
      idx = which(x == U[iU])
      nh = round(length(idx) * rat)
      rnd = runif(length(idx))
      if (n) {
        grp = group[idx]
        split(rnd, grp) <- lapply(split(rnd, grp), mean)
      }
      ord = order(rnd)
      BinOne[idx[ord[1:nh]]] = TRUE
    }
  }
  if (ratio >= 1) {
    nh = sum(BinOne) - ratio
    if (nh > 0) 
      BinOne[sample(which(BinOne), nh)] = FALSE
    else if (n < 0) 
      BinOne[sample(which(!BinOne), -nh)] = TRUE
  }
  return(BinOne)
}