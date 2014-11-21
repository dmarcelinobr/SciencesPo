add.alpha<- function (color, alpha = .5) 
{
    if(missing(color))
      stop("vector of colors missing")
    col <- col2rgb(color, TRUE)/255
    if (length(color) != length(alpha)) {
        if (length(color) > 1 && length(alpha) > 1) {
            stop("Only one color and alpha can be vectorised!")
        }
        if (length(color) > 1) {
            alpha <- rep(alpha, length.out = length(color))
        }
        else if (length(alpha) > 1) {
            col <- col[, rep(1, length(alpha)), drop = FALSE]
        }
    }
    alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
    alpha_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
    alpha_col[is.na(color)] <- NA
    alpha_col
}