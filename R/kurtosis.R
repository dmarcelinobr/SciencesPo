kurtosis <-
function (x, na.rm = FALSE, type = 2) 
    {
        if (any(i.na <- is.na(x))) {
            if (na.rm) 
                x <- x[!i.na]
            else return(NA)
        }
        if (!(type %in% (1:3))) 
            stop("Your argument for 'type' is not valid.")
        n <- length(x)
        dev <- (x - mean(x))
        r <- (n * sum(dev^4)/(sum(dev^2)^2))
        y <- if (type == 1) 
            r - 3
        else if (type == 2) {
            if (n < 4) 
                stop("You need at least 4 complete observations.")
            ((n + 1) * (r - 3) + 6) * (n - 1)/((n - 2) * (n - 3))
        }
        else r * (1 - 1/n)^2 - 3
        y
    }
