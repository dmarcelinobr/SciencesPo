
#' @title Likelihood Ratio Test (LRT)
#'
#' @description Computes the likelihood ratio test (LRT)--a goodness-of-fit--between two nested models. It checks the difference in the deviance (-2*logLikelihood) between the two candidate models against the change in degrees of freedom using a chi-squared test.
#'  
#' @param model1 the first model object.
#' @param model2 the second model object.
#' 
#' @details \verb{LR = -2 ln(L(m1)/L(m2)) = 2(ll(m2)-ll(m1))}
#' 
#' @examples
#' data(titanic)
#' mod0 <- glm( SURVIVED ~ CLASS, family=binomial, data=titanic)
#' mod1 <- glm( SURVIVED ~ CLASS + SEX, family=binomial, data=titanic) 
#' 
#' lrtest(mod0, mod1)
#' 
#' @export
lrtest <- function (model1, model2)
{
  if (any(class(model1) != class(model2))) {
    stop("Two models have different classes")
  }
  if (any(class(model1) == "coxph") & any(class(model2) ==
                                            "coxph")) {
    if (model1$n != model2$n) {
      stop("Two models has different sample sizes")
    }
    cat("\n")
    df1 <- length(model1$coefficients)
    df2 <- length(model2$coefficients)
    lrt <- 2 * (model2$loglik[2] - model1$loglik[2])
    diff.df <- df2 - df1
    if (lrt < 0) {
      lrt <- -lrt
      diff.df <- -diff.df
    }
    if (lrt * diff.df < 0) {
      stop("Likelihood gets worse with more variables. Test not executed")
    }
  }
  if (any(class(model1) == "multinom") & any(class(model2) ==
                                               "multinom")) {
    if (any(dim(model1$residuals) != dim(model2$residuals))) {
      stop("Two models have different outcomes or different sample sizes")
    }
    cat("\n")
    df1 <- model1$edf
    df2 <- model2$edf
    lrt <- model2$deviance - model1$deviance
    diff.df <- df1 - df2
    if (lrt < 0) {
      lrt <- -lrt
      diff.df <- -diff.df
    }
    if (lrt * diff.df < 0) {
      stop("Likelihood gets worse with more variables. Test not executed")
    }
  }
  if (any(class(model1) == "polr") & any(class(model2) == "polr")) {
    if (model1$n != model2$n) {
      stop("Two models have different outcomes or different sample sizes")
    }
    cat("\n")
    df1 <- model1$edf
    df2 <- model2$edf
    lrt <- model2$deviance - model1$deviance
    diff.df <- df1 - df2
    if (lrt < 0) {
      lrt <- -lrt
      diff.df <- -diff.df
    }
    if (lrt * diff.df < 0) {
      stop("Likelihood gets worse with more variables. Test not executed")
    }
  }
  if (suppressWarnings((all(class(model1) == c("glm", "lm")) &
                          all(class(model2) == c("glm", "lm"))) | (any(class(model1) ==
                                                                         "negbin") & any(class(model2) == "negbin")))) {
    if (sum(model1$df.null) != sum(model2$df.null))
      stop("Number of observation not equal!!")
    df1 <- attributes(logLik(model1))$df
    df2 <- attributes(logLik(model2))$df
    lrt <- 2 * (as.numeric(logLik(model2) - logLik(model1)))
    diff.df <- df2 - df1
    if (lrt < 0) {
      lrt <- -lrt
      diff.df <- -diff.df
    }
    if (lrt * diff.df < 0) {
      stop("Likelihood gets worse with more variables. Test not executed")
    }
  }
  output <- list(model1 = model1$call, model2 = model2$call, model.class =class(model1),
                 Chisquared = lrt, df = diff.df, p.value = pchisq(lrt,
                                                                  diff.df, lower.tail = FALSE))
  class(output) <- "lrtest"
  output
}
NULL




#' @title Method for printing lrtest
#'
#' @param x a model class object.
#' @param \dots typically unecessary parameters.
#'
#' 
#' @export
print.lrtest <- function(x, ...) {
  if(any(x$model.class == "coxph")){
    cat("Likelihood ratio test for Cox regression & conditional logistic regression",
        "\n")
    cat("Chi-squared", x$df, "d.f. = ", x$Chisquared, ",",
        "P value = ", x$p.value, "\n")
    cat("\n")
  }
  if(any(x$model.class == "multinom")){
    cat("Likelihood ratio test for multinomial logistic regression",
        "\n")
    cat("Chi-squared", x$df, "d.f. = ", x$Chisquared, ",",
        "P value = ", x$p.value, "\n")
    cat("\n")
  }
  if(any(x$model.class == "polr")){
    cat("Likelihood ratio test for ordinal regression",
        "\n")
    cat("Chi-squared", x$df, "d.f. = ", x$Chisquared, ",",
        "P value = ", x$p.value, "\n")
    cat("\n")
  }
  if (suppressWarnings((all(x$model.class == c("glm", "lm"))) | (any(x$model.class ==
                                                                       "negbin")))){
    
    cat("Likelihood ratio test for MLE method", "\n")
    cat("Chi-squared", x$df, "d.f. = ", x$Chisquared, ",",
        "P value = ", x$p.value, "\n")
    cat("\n")
  }
}