#' Converts integer into its Roman Numeral equivalent
#' @param x The integer to be translated.
#' @examples
#' as.roman(1977)
#' @export
as.roman<-function(x) {
  if(0 < x & x < 5000) {
    x<-as.integer(x)
    digits<-c(1000,900,500,400,100,90,50,40,10,9,5,4,1)
    numerals<-c("M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I")
    digits.numerals<-as.data.frame(cbind(digits,numerals), stringsAsFactors=FALSE)
    numeral<-""
    for(i in 1:nrow(digits.numerals)) {
      while(x >= as.numeric(digits.numerals[i,1])) {
        numeral<-paste(numeral,digits.numerals[i,2],sep="")
        x<-x-as.numeric(digits.numerals[i,1])
      }
    }
    return(numeral)
  }
  else {
    stop(paste(x,"is invalid. Input must be an integer between 1 and 4,999"))
  }
}
