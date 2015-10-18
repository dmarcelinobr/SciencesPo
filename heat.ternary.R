library(ggplot2)
library(ggtern)
library(MASS)
library(scales)
library(plyr)
library(SciencesPo)

palette <- c( "#FF9933", "#002C54", "#3375B2", "#CCDDEC", "#BFBFBF", "#000000")

# Example data
 sig <- matrix(c(3,0,0,2),2,2)
 data <- data.frame(mvrnorm(n=10000, rep(2, 2), sig))
 data$X1 <- data$X1/max(data$X1)
 data$X2 <- data$X2/max(data$X2)
 data$X1[which(data$X1<0)] <- runif(length(data$X1[which(data$X1<0)]))
 data$X2[which(data$X2<0)] <- runif(length(data$X2[which(data$X2<0)]))

# Print 2d heatmap
heatmap2d <- function(data) {
  p <- ggplot(data, aes(x=X1, y=X2)) +
    stat_bin2d(bins=50) +
    scale_fill_gradient2(low=palette[4], mid=palette[3], high=palette[2]) +
    xlab("Percentage x") +
    ylab("Percentage y") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = percent) +
    theme_minimal() + theme(text = element_text(size = 15))
  print(p)
}

# Example data
data$X3 <- with(data, 1-X1-X2)
 data <- data[data$X3 >= 0,]

# Auxiliary function for heat.ternary
count_bin <- function(data, minT, maxT, minR, maxR, minL, maxL) {
  ret <- data
  ret <- with(ret, ret[minT <= X1 & X1 < maxT,])
  ret <- with(ret, ret[minL <= X2 & X2 < maxL,])
  ret <- with(ret, ret[minR <= X3 & X3 < maxR,])
  if(is.na(nrow(ret))) {
    ret <- 0
  } else {
    ret <- nrow(ret)
  }
  ret
}

# Plot 3dimensional histogram in a triangle
# See dataframe data for example of the input dataformat
heat.ternary <- function(data, inc, logscale=FALSE, text=FALSE, plot_corner=TRUE) {
  #   When plot_corner is FALSE, corner_cutoff determines where to stop plotting
  corner_cutoff = 1
  #   When plot_corner is FALSE, corner_number toggles display of obervations in the corners
  #   This only has an effect when text==FALSE
  corner_numbers = TRUE

  count <- 1
  points <- data.frame()
  for (z in seq(0,1,inc)) {
    x <- 1- z
    y <- 0
    while (x>0) {
      points <- rbind(points, c(count, x, y, z))
      x <- round(x - inc, digits=2)
      y <- round(y + inc, digits=2)
      count <- count + 1
    }
    points <- rbind(points, c(count, x, y, z))
    count <- count + 1
  }
  colnames(points) = c("IDPoint","T","L","R")

  #   base <- ggtern(data=points,aes(L,T,R)) +
  #               theme_bw() + theme_hidetitles() + theme_hidearrows() +
  #               geom_point(shape=21,size=10,color="blue",fill="white") +
  #               geom_text(aes(label=IDPoint),color="blue")
  #   print(base)

  polygons <- data.frame()
  c <- 1
  #   Normal triangles
  for (p in points$IDPoint) {
    if (is.element(p, points$IDPoint[points$T==0])) {
      next
    } else {
      pL <- points$L[points$IDPoint==p]
      pT <- points$T[points$IDPoint==p]
      pR <- points$R[points$IDPoint==p]
      polygons <- rbind(polygons,
                        c(c,p),
                        c(c,points$IDPoint[abs(points$L-pL) < inc/2 & abs(points$R-pR-inc) < inc/2]),
                        c(c,points$IDPoint[abs(points$L-pL-inc) < inc/2 & abs(points$R-pR) < inc/2]))
      c <- c + 1
    }
  }

  # Upside down triangles
  for (p in points$IDPoint) {
    if (!is.element(p, points$IDPoint[points$T==0])) {
      if (!is.element(p, points$IDPoint[points$L==0])) {
        pL <- points$L[points$IDPoint==p]
        pT <- points$T[points$IDPoint==p]
        pR <- points$R[points$IDPoint==p]
        polygons <- rbind(polygons,
                          c(c,p),
                          c(c,points$IDPoint[abs(points$T-pT) < inc/2 & abs(points$R-pR-inc) < inc/2]),
                          c(c,points$IDPoint[abs(points$L-pL) < inc/2 & abs(points$R-pR-inc) < inc/2]))
        c <- c + 1
      }
    }
  }

  #   IMPORTANT FOR CORRECT ORDERING.
  polygons$PointOrder <- 1:nrow(polygons)
  colnames(polygons) = c("IDLabel","IDPoint","PointOrder")

  df.tr <- merge(polygons,points)

  Labs = ddply(df.tr,"IDLabel",function(x){c(c(mean(x$T),mean(x$L),mean(x$R)))})
  colnames(Labs) = c("Label","T","L","R")

  #   triangles <- ggtern(data=df.tr,aes(L,T,R)) +
  #                   geom_polygon(aes(group=IDLabel),color="black",alpha=0.25) +
  #                   geom_text(data=Labs,aes(label=Label),size=4,color="black") +
  #                   theme_bw()
  #        print(triangles)

  bins <- ddply(df.tr, .(IDLabel), summarize,
                maxT=max(T),
                maxL=max(L),
                maxR=max(R),
                minT=min(T),
                minL=min(L),
                minR=min(R))

  count <- ddply(bins, .(IDLabel), summarize, N=count_bin(data, minT, maxT, minR, maxR, minL, maxL))
  df <- join(df.tr, count, by="IDLabel")

  Labs = ddply(df,.(IDLabel,N),function(x){c(c(mean(x$T),mean(x$L),mean(x$R)))})
  colnames(Labs) = c("Label","N","T","L","R")

  if (plot_corner==FALSE){
    corner <- ddply(df, .(IDPoint, IDLabel), summarize, maxperc=max(T,L,R))
    corner <- corner$IDLabel[corner$maxperc>=corner_cutoff]

    df$N[is.element(df$IDLabel, corner)] <- 0
    if (text==FALSE & corner_numbers==TRUE) {
      Labs$N[!is.element(Labs$Label, corner)] <- ""
      text=TRUE
    }
  }

  heat <- ggtern(data=df,aes(L,T,R)) +
    geom_polygon(aes(fill=N,group=IDLabel),color="black",alpha=1)
  if (logscale == TRUE) {
    heat <- heat + scale_fill_gradient(name="Observations", trans = "log",
                                       low=palette[2], high=palette[4])
  } else {
    heat <- heat + scale_fill_gradient(name="Observations",
                                       low=palette[2], high=palette[4])
  }
  heat <- heat +
    Tlab("x") +
    Rlab("y") +
    Llab("z") +
    theme_bw() +
    theme(axis.tern.arrowsep=unit(0.02,"npc"), #0.01npc away from ticks ticklength
          axis.tern.arrowstart=0.25,axis.tern.arrowfinish=0.75,
          axis.tern.text=element_text(size=12),
          axis.tern.arrow.text.T=element_text(vjust=-1),
          axis.tern.arrow.text.R=element_text(vjust=2),
          axis.tern.arrow.text.L=element_text(vjust=-1),
          axis.tern.arrow.text=element_text(size=12),
          axis.tern.title=element_text(size=15))
  if (text==FALSE) {
    print(heat)
  } else {
    print(heat + geom_text(data=Labs,aes(label=N),size=3,color="white"))
  }
}

# Usage examples
heat.ternary(data, 0.2, text=TRUE)
 heat.ternary(data, 0.05)
 heat.ternary(data, 0.1, text=FALSE, logscale=TRUE)
 heat.ternary(data, 0.1, text=TRUE, logscale=FALSE, plot_corner=FALSE)
 heat.ternary(data, 0.1, text=FALSE, logscale=FALSE, plot_corner=FALSE)
