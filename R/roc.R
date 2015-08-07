roc <- function(x, ...)  UseMethod("roc")

roc.default <- function(x, ..., group=NULL) {
  args <- list(x, ...)
  namedargs <- if (!is.null(names(args)))
    names(args) != ""
  else logical(length(args))
  groups <- if (is.list(x))
    x
  else if (!is.null(group)) split(x, group)
  else args[!namedargs]
  if ((n <- length(groups)) == 0)
    stop("invalid first argument")
  if (n != 2)
    stop("Only use 2 groups.")
  if (length(class(groups)))
    groups <- unclass(groups)
  gf <- names(groups)
  lx <- length(groups[[1]])
  ly <- length(groups[[2]])
  gp <- rep(gf, sapply(groups, length))[order(unlist(groups))]
  gx <- gp == gf[1]
  x <- 1 - c(0, cumsum(gx)/lx)
  y <- c(0, cumsum(!gx)/ly)
  a <- sum(y[c(FALSE, diff(x) != 0)])/lx
  result <- list(
    sens=y,
    spec=x,
    auc=a
  )
  class(result) <- "roc"
  return(result)
}

roc.formula <- function(formula, data=NULL, ..., subset, na.action=NULL) {
  if (missing(formula) || (length(formula) != 3))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots=FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m$na.action <- na.action
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  do.call("roc", c(list(split(mf[[response]], mf[-response])), args))
}

plot.roc <- function(x, ...) {
  plot.mine <- function(..., type, xlim, ylim, log, main="ROC",
                        sub=paste("AUC =", x$auc), xlab="1 - spec",
                        ylab="sens") {
    plot.default(1 - x$spec, x$sens, type="l", xlim=0:1,
                 ylim=0:1, log="", main=main, sub=sub, xlab=xlab, ylab=ylab)
  }
  plot.mine(...)
}
