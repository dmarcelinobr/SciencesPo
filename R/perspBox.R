#' @title Create an Empty Persp Plot 
#'
#' @description Creates an empty persp plot objtect. 
#'
#' @param x1 data for the first horizontal axis, an R vector
#' @param x2 data for the second horizontal axis, an R vector
#' @param y data for the vertical axis, an R vector
#' @param x1lab label for the x1 axis, (the one called "xlab" inside persp)
#' @param x2lab label for the x2 axis, (the one called "ylab" inside persp)
#' @param ylab label for the y (vertical) axis (the one called "zlab" inside persp)
#' @param x1lim Optional: limits for x1 axis (should be a vector with 2 elements)
#' @param x2lim Optional: limits for x2 axis (should be a vector with 2 elements)
#' @param ... further arguments that are passed to persp, including xlab,
#' ylab, zlab, xlim, ylim, and zlim are going to be ignored.
#' 
#' @return The perspective matrix that is returned by persp
#' @examples
#' x1 <- 1:10
#' x2 <- 41:50
#' y <-  rnorm(10)
#' perspBox(x1, x2, y)
#' box <- perspBox(x1, x2, y, ticktype = "detailed", nticks=10)
#' mypoints1 <- trans3d( x1, x2, y, pmat = box )
#' points(mypoints1, pch = 16, col= "blue")
#'
#' @export
#'  
perspBox <-
    function(x1, x2, y, x1lab = "x1", x2lab = "x2", ylab = "y", x1lim, x2lim, ... )
{
    x1range <- range(x1, na.rm = TRUE)
    x2range <- range(x2, na.rm = TRUE)
    yrange <- range(y, na.rm = TRUE)
    zero <- outer(x1, x2, function(a,b) { a*b*0 + yrange[1] })

    dotargs <- list(...)
    dotargs[["xlab"]] <- x1lab
    dotargs[["ylab"]] <- x2lab
    dotargs[["zlab"]] <- ylab
    if (!missing(x1lim)) dotargs[["xlim"]] <- x1lim
    if (!missing(x2lim)) dotargs[["ylim"]] <- x2lim
    my_param <- list(x = x1, y = x2, z = zero, zlim = yrange, lwd = .5, theta = -25, phi = 25)

    myargs <- modifyList(my_param, dotargs)
    ans <- do.call("persp", myargs)
}

