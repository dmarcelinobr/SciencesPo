#' @encoding UTF-8
#' @title Johnson-Neyman Regression
#' @description Probing Regression Interactions
#' @param y the dependent variable.
#' @param x the independent variable.
#' @param z the moderator variable.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @importFrom stats lm vcov coef qt
#'@export
`johnson.neyman` = function(y,x,z){
  mod = lm(y~x + z + x:z)
  gam1.v = vcov(mod)["x","x"]
  gam3.v = vcov(mod)["x:z","x:z"]
  gam1gam3.cv = vcov(mod)["x","z"]
  df = length(y)-length(coef(mod))
  t = qt(.975,df)
  zz = seq(min(z),max(z),by=.01)
  se.w1 = sqrt(gam1.v + 2*zz*gam1gam3.cv + zz^2*gam3.v)
  w1.hat = coef(mod)["x"]+coef(mod)["x:z"]*zz
  z.tab = cbind(zz,t<abs(w1.hat/se.w1))
  ci.low = w1.hat - t*se.w1
  ci.upp = w1.hat + t*se.w1
  w1.tab = data.frame(w1.hat,z=z.tab[,1],z.tab[,2],ci.low,ci.upp)
  colnames(w1.tab) = c("Est","Z","Significant","LB95", "UB95")
  w1.tab[,3] = ifelse(w1.tab[,3]=="1","Yes","No")
  w1.tab
}
NULL


