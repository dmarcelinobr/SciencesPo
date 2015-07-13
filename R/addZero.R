#' @title Add leading zero
#' @param numb The number.
#' @examples
#' addZero(123)
#' @export
addZero = function(numb) {
  if (nchar(numb) == 3) {numb = paste("00", numb, sep = "")
  } else if (nchar(numb) == 4) {numb = paste("0", numb, sep = "")
  } else numb = numb
  return(numb)
}
