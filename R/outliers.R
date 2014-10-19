outliers <-
function(x, index=NULL) {
        if (is.data.frame(x)) {
            as.data.frame(sapply(x, outliers, index))
        } else if (is.matrix(x)) {
            apply(x, 2, outliers, index)
        } else if (is.list(x)) {
            lapply(x, outliers, index)
        } else if (is.vector(x)) {
            if (!is.null(index)) {
                if (!is.list(index)) {
                    index <- list(index) # make sure index is a list
                }
                unsplit(outliers(split(x,index),index=NULL),index)
            } else {
                
                mu <- mean(x)
                dev <- abs(x - mu)
                closest <- which.min(dev)
                farthest <- which.max(dev)
                min <- dev[closest]
                max <- dev[ farthest]
                output <- data.frame(closest, min, farthest, max)
                return(output)
            }
        } else {
            cat("non-numeric argument to 'outlier'",class(x),"\n",sep="")
        }
    }
