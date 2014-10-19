detail <-
function (x, basic = FALSE, na.rm = TRUE, trim = 0.2, type = 2, k = 1)
    {
        cl <- match.call()
        valid <- function(x) {
            sum(!is.na(x))
        }
        if (!na.rm)
            x <- na.omit(x)
        if (is.null(dim(x)[2])) {
            len <- 1
            scores = matrix(rep(NA, 13), ncol = 13)
            scores[1, 1] <- valid(x)
            scores[1, 2] <- mean(x, na.rm = na.rm)
            scores[1, 3] <- sd(x, na.rm = na.rm)
            scores[1, 4] <- var(x, na.rm = na.rm)
            scores[1, 5] <- se(x, na.rm = na.rm)
            scores[1, 6] <- median(x, na.rm = na.rm)
            scores[1, 7] <- mad(x, na.rm = na.rm)
            scores[1, 8] <- mean(x, na.rm = na.rm, trim = trim)
            scores[1, 9] <- winsor.mean(x, k=k, na.rm = na.rm)
            scores[1, 10] <- min(x, na.rm = na.rm)
            scores[1, 11] <- max(x, na.rm = na.rm)
            scores[1, 12] <- skewness(x, na.rm = na.rm, type = type)
            scores[1, 13] <- kurtosis(x, na.rm = na.rm, type = type)
            
            vars <- 1
        }
        else {
            scores = matrix(rep(NA, ncol(x) * 13), ncol = 13)
            rownames(scores) <- colnames(x)
            scores[, 1] <- apply(x, 2, valid)
            vars <- c(1:ncol(x))
            for (i in seq_along(x)) {
                if (is.factor(x[[i]]) || is.logical(x[[i]])) {
                    x[[i]] <- as.numeric(x[[i]])
                    rownames(scores)[i] <- paste(rownames(scores)[i],
                                                "!#", sep = " ")
                }
                if (!is.numeric(unclass(x[[i]])))
                    stop("'detail' still doesn't know how to deal with  'Date' or 'character' vectors")
            }
            scores[, 2] <- apply(x, 2, mean, na.rm = na.rm)
            if (!basic) {
            scores[, 12] <- sapply(x, FUN=skewness, na.rm = na.rm, type = type)
            scores[, 13] <- sapply(x, FUN=kurtosis, na.rm = na.rm, type = type)
            }
            scores[, 3] <- sapply(x, FUN=sd, na.rm = na.rm)
            scores[, 4] <- sapply(x, FUN=var, na.rm = na.rm)
            scores[, 5] <- sapply(x, FUN=se, na.rm = na.rm)
            scores[, 6] <- sapply(x, FUN=median, na.rm = na.rm)
            scores[, 7] <- sapply(x, FUN=mad, na.rm = na.rm)
            scores[, 8] <- sapply(x, FUN=mean, na.rm = na.rm, trim = trim)
            scores[, 9] <- sapply(x, FUN=winsor.mean, k=k, na.rm = na.rm)
            scores[, 10] <- sapply(x, FUN=min, na.rm = na.rm)
            scores[, 11] <- sapply(x, FUN=max, na.rm = na.rm)      
        }
        if (!basic) {
            if (!basic) {
            temp <- data.frame(obs = scores[, 1], 
                mean = scores[, 2], 
                sd = scores[, 3],  
                var = scores[, 4], 
                se = scores[, 5], 
                median = scores[, 6], 
                mad = scores[, 7],
                trimmed = scores[, 8], 
                winsor = scores[, 9],
                range = scores[, 11] - scores[, 10],
                min = scores[, 10], 
                max = scores[, 11], 
                skew = scores[, 12], 
                kurt = scores[, 13])
            }
            else {
                temp <- data.frame(obs = scores[, 1], 
                    mean = scores[, 2], 
                    sd = scores[, 3],  
                    var = scores[, 4], 
                    se = scores[, 5], 
                    median = scores[, 6], 
                    mad = scores[, 7],
                    trimmed = scores[, 8],
                    winsor = scores[, 9],
                    range = scores[, 11] - scores[, 10],
                    min = scores[, 10], 
                    max = scores[, 11])
            }
        }
        else {
            if (!basic) {
                temp <- data.frame(obs = scores[, 1], 
                    mean = scores[, 2], 
                    sd = scores[, 3], 
                    var = scores[, 4], 
                    skew = scores[, 12], 
                    kurt = scores[, 13])
            }
            else {
                temp <- data.frame(obs = scores[, 1], 
                    mean = scores[, 2], 
                    sd = scores[, 3], 
                    var = scores[, 4], 
                    min = scores[, 10], 
                    max = scores[, 11] )
            }
        }
   output <- format(round(data.frame(vars = vars, temp), 1), nsmall = 0)
   
   class(output) <- c("SciencePo", "detail", "data.frame")
   return(output)

}
