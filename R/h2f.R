h2f <-
function (x, head = 4, tail = 4, digits = 2, ellipsis = TRUE) 
{
    if (is.data.frame(x) | is.matrix(x)) {
        if (is.matrix(x)) 
            x <- data.frame(unclass(x))
        nvar <- dim(x)[2]
        dots <- rep("...", nvar)
        h <- data.frame(head(x, head))
        t <- data.frame(tail(x, tail))
        for (i in 1:nvar) {
            if (is.numeric(h[1, i])) {
                h[i] <- round(h[i], digits)
                t[i] <- round(t[i], digits)
            }
            else {
                dots[i] <- NA
            }
        }
        if (ellipsis) {
            top.foot <- rbind(h, ... = dots, t)
        }
        else {
            top.foot <- rbind(h, t)
        }
    }
    else {
        h <- head(x, head)
        t <- tail(x, tail)
        if (ellipsis) {
            top.foot <- rbind(h, "...       ...", t)
        }
        else {
            top.foot <- rbind(h, t)
            top.foot <- as.matrix(top.foot)
        }
    }
    return(top.foot)
}
