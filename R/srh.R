#' Scheirer-Ray-Hare Test
#'
#' @description Performs the Scheirer-Ray-Hare test for balanced data.
#' @param r The rank vvector
#' @param pf The first factor.
#' @param sf The second factor.
#' @param n The number of elements in each subgroup.
#' @examples
#' # Sokal RR, Rohlf FJ, 1995. Biometry, 3rd ed. New York: Freeman.
#' rat <- c(709,679,699,657,594,677,592,538,476,508,505,539)
#' fat <- gl(2,6,labels=c("Fresh","Rancid"))
#' sex <- gl(2,3,12,labels=c("man","woman"))
#' srh(rat,sex,fat)
#' @export
srh <-function(r, pf , sf, n=3){
  abn = (nlevels(pf)*nlevels(sf)*n)
  lm_rank <- lm(rank(r)~pf*sf)
  # the variance must be correct if there are ties
  aov_lm <- anova(lm_rank)
  ans <-  aov_lm[1:3,1:3]
  ans[,4] <- ans[,2]/(length(r)*(length(r)+1)/abn)
  ans[,5] <- (1-pchisq(ans[,4],ans[,1]))
  colnames(ans)[4:5] <- c("H","p-value")
  ans
}
NULL
