skewness <-
function (x, na.rm = TRUE, type = 2) 
    {
        if (length(dim(x)) == 0) {
            if (na.rm) {
                x <- x[!is.na(x)]
            }
            stdev <- sd(x, na.rm = na.rm)
            mu <- mean(x)
            n <- length(x[!is.na(x)])
            switch(type, {
                skewer <- sqrt(n) * (sum((x - mu)^3, na.rm = na.rm)/(sum((x - 
                                                                              mu)^2, na.rm = na.rm)^(3/2)))
            }, {
                skewer <- n * sqrt(n - 1) * (sum((x - mu)^3, na.rm = na.rm)/((n - 
                                                                                  2) * sum((x - mu)^2, na.rm = na.rm)^(3/2)))
            }, {
                skewer <- sum((x - mu)^3)/(n * sd(x)^3)
            })
        }
        else {
            skewer <- rep(NA, dim(x)[2])
            if (is.matrix(x)) {
                mu <- colMeans(x, na.rm = na.rm)
            }
            else {
                mu <- apply(x, 2, mean, na.rm = na.rm)
            }
            stdev <- apply(x, 2, sd, na.rm = na.rm)
            for (i in 1:dim(x)[2]) {
                n <- length(x[!is.na(x[, i]), i])
                switch(type, {
                    skewer[i] <- sqrt(n) * (sum((x[, i] - mu[i])^3, 
                                                na.rm = na.rm)/(sum((x[, i] - mu[i])^2, na.rm = na.rm)^(3/2)))
                }, {
                    skewer[i] <- n * sqrt(n - 1) * (sum((x[, i] - 
                                                             mu[i])^3, na.rm = na.rm)/((n - 2) * sum((x[, 
                                                                                                        i] - mu[i])^2, na.rm = na.rm)^(3/2)))
                }, {
                    skewer[i] <- sum((x[, i] - mu[i])^3, na.rm = na.rm)/(n * 
                                                                             stdev[i]^3)
                })
            }
        }
        return(skewer)
    }
