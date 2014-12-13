ogive <- function(x, y = NULL, ...)
{
    ## Can compute the ogive either from an object of class
    ## 'grouped.data', or from a vector of class boundaries and a
    ## vector of class frequencies. The second vector is one element
    ## shorter than the first.
    if (inherits(x, "grouped.data"))
    {
        y <- x[, 2]
        x <- eval(expression(cj), env = environment(x))
    }
    else
    {
        if (length(x) - length(y) != 1)
            stop("invalid number of group boundaries and frequencies")
    }

    ## Create an object of class 'ogive'.
    res <- approxfun(x, cumsum(c(0, y)) / sum(y), yleft = 0, yright = 1,
                     method = "linear", ties = "ordered")
    class(res) <- c("ogive", class(res))
    attr(res, "call") <- sys.call()
    res
}

### Essentially identical to stats::print.ecdf().
print.ogive <- function(x, digits = getOption("digits") - 2, ...)
{
    ## Utility function
    numform <- function(x) paste(formatC(x, dig = digits), collapse = ", ")

    ## The rest is adapted from ecdf()
    cat("Ogive for grouped data \nCall: ")
    print(attr(x, "call"), ...)
    nc <- length(xxc <- get("x", env = environment(x)))
    nn <- length(xxn <- get("y", env = environment(x)))
    i1 <- 1:min(3, nc)
    i2 <- if (nc >= 4) max(4, nc - 1):nc else integer(0)
    i3 <- 1:min(3, nn)
    i4 <- if (nn >= 4) max(4, nn - 1):nn else integer(0)
    cat("    x = ", numform(xxc[i1]), if (nc > 3) ", ",
        if (nc > 5) " ..., ", numform(xxc[i2]), "\n", sep = "")
    cat(" F(x) = ", numform(xxn[i3]), if (nn > 3) ", ",
        if (nn > 5) " ..., ", numform(xxn[i4]), "\n", sep = "")
    invisible(x)
}

### Essentially identical to stats::summary.ecdf().
summary.ogive <- function (object, ...)
{
    cat("Ogive:\t ", eval(expression(n), env = environment(object)),
        "unique values with summary\n")
    summary(knots(object), ...)
}

### Identical to stats::knots.stepfun().
knots.ogive <- stats:::knots.stepfun

plot.ogive <- function(x, main = NULL, xlab = "x", ylab = "F(x)", ...)
{
    if (missing(main))
        main <- {
            cl <- attr(x, "call")
            deparse(if (!is.null(cl)) cl else sys.call())
        }

    kn <- knots(x)
    Fn <- x(kn)
    plot(kn, Fn,  ..., type = "o", pch = 16,
         main = main, xlab = xlab, ylab = ylab)
}