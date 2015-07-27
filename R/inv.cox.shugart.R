inv.cox.shugart <- function(Z,R) {
  # m <- lm (z ~ x)
  # a <- as.numeric(m$coefficients[1])
  # b <- as.numeric(m$coefficients[2])
  ZZ <- mean(Z)
  RR <- mean(R)
  b <- sum((Z-ZZ) * (R-RR))/sum((R-RR)^2)
  return(b)
  }
