
#' @encoding UTF-8
#' @title Computes the region significance
#' @description Probing Regression Interactions
#' @param y the dependent variable.
#' @param x the independent variable.
#' @param z the moderator variable.
#' @param zval an specific value for the moderator (z).
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @importFrom stats lm vcov coef pt
#'@export
`regionSignificance` = function(y, x, z , zval){
  mod = lm(y~x + z + predictor:z)
  gam1.v = vcov(mod)["x","x"]
  gam3.v = vcov(mod)["x:z","x:z"]
  gam1gam3.cv = vcov(mod)["x","z"]
  df = length(y)-length(coef(mod))
  se.w1 = sqrt(gam1.v + 2*zval*gam1gam3.cv + zval^2*gam3.v)
  w1.hat = coef(mod)["x"]+coef(mod)["x:z"]*zval
  p.value = (1-pt(w1.hat/se.w1,df=df))*2
  w1.tab = cbind(w1.hat,se.w1,zval,p.value)
  rownames(w1.tab) = "Moderator"
  colnames(w1.tab) = c("Est","SE","Z","p-value")
  w1.tab
}
NULL
