se <-
function (x, na.rm = TRUE) 
    {
        valid <- function(x) return(sum(!is.na(x)))
        dim <- dim(x)
        if (is.null(dim)) {
            sd <- sd(x, na.rm = na.rm)
            n.valid <- valid(x)
        }
        else {
            if (is.data.frame(x)) {
                n.valid <- unlist(sapply(x, valid))
                sd <- unlist(sapply(x, sd, na.rm = na.rm))
            }
            else {
                n.valid <- unlist(apply(x, 2, valid))
                sd <- unlist(apply(x, 2, sd, na.rm = na.rm))
            }
        }
        return(sd/sqrt(n.valid))
    }
