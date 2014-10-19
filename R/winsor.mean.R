winsor.mean <-
function (x, k = 1, na.rm=TRUE) {
        if (any(is.na <- is.na(x))) {
            if (na.rm) 
                x <- x[!is.na]
            else return(NA)
        }
        n <- length(x)
        if (!(k %in% (0:n))) 
            stop("'k' should be > 0 and less than half the number of non-missing observations.")
        else {
            x <- sort(x)
            x[1:k] <- x[k+1] # Here I solve the lower values
            x[(n-k+1):n] <- x[n-k] #Then I go over the higher ones
            return(mean(x))
        }
    }
