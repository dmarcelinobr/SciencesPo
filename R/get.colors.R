get.colors <- function() {
  par(mar = c(0, 0, 0, 0) ,mgp = c(0, 0, 0))
  plot(c(0:24), type = 'n')
  c <- 0
  mouse <- function(b, x, y) {
    x <- as.integer(x * 26)
    y <- as.integer(y * 26)
    print(colors()[(x + 26 *y) %% 657 + 1])
    return()
  }
  k <- colours()[(1:26 ^ 2 - 1) %% 657 + 1]
  for (i in 1:26) {
    for (j in 1:26) {
      c <- c + 1
      polygon(c(j, j, j-1, j-1), c(i, i-1, i-1, i) - 1, col=k[(c - 1) %% 657 + 1])
    }
  }
  #X11(type="Xlib")
  getGraphicsEvent('Click on the color to select it', onMouseDown = mouse)
}