#' @title The Butterfly Curve
#'
#' @description The butterfly curve is a parametric equation discovered by Temple Fay where two functions in a plane produces a butterfly-like curves.

#' @param n An integer for the background points.
#' @param nb An integer for the butterfly's points.
#' @param title A character vector for the plot title.
#' @references
#' Fay, Temple H. (May 1989). The Butterfly Curve. \emph{Amer. Math. Monthly} 96 (5): 442-443. doi:10.2307/2325155.
#' @export
#' @examples
#' butterfly(10, 100, title="10 x 100")
#' butterfly(10, 200, title="10 x 200")
#' butterfly(10, 500, title="100 x 500")
#' butterfly(100, 1000, title="100 x 1000")
#'
butterfly <- function(n=100, nb=500, title=element_blank())
{
s1 <- Sys.time()
t <- seq(0,10*pi,length=nb)

butterfly <- data.frame(x=sin(t)*(exp(1)^cos(t)-2*cos(4*t)-(sin(t/12))^5),
                        y=cos(t)*(exp(1)^cos(t)-2*cos(4*t)-(sin(t/12))^5),
                        s=runif(nb, min=.1, max=10),
                        f=factor(sample(1:10,nb,TRUE)),
                        a=runif(nb,min=.1, max=.4))

points <- data.frame(x=runif(n,-4,4),
                     y=runif(n,-3,5),
                     s=runif(n,min=30, max=50),
                     f=factor(sample(1:10,n,TRUE)),
                     a=runif(n,min=.05, max=.15))

.data <- rbind(butterfly, points)

gplot <- ggplot2::ggplot(.data, ggplot2::aes(x, y, colour=f))
gplot <- gplot + ggplot2::geom_point(alpha=.data$a,size=.data$s)
gplot <- gplot + ggplot2::theme(legend.position="none",
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.text =element_blank())
gplot <- gplot + ggplot2::ggtitle(title)

s2 <- Sys.time()
timediff <- c( s2 - s1 )
cat("\n", "Date of Analysis: ",format(Sys.time(), "%a %b %d %Y"), "\n", "Computation time: ",timediff,sep="","\n")
cat("-----------------------------------\n")
return(gplot)
}
