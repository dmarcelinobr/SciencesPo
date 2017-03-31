#' Normal transformation
#'
#' @param dat vector of values to be normal transformed
#' @return vector of transformed values, with attributes required for the backtransform
#' @export
`normalTransform` <- function(dat)
{
  attributes(dat) <- list(transform_type='normal',transform_data=dat)
  dat[order(dat)] <- (c(1:length(dat))-0.5)/length(dat)
  dat <- qnorm(dat)
  return(dat)
}
