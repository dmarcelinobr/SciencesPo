.onAttach <- function(...) {
  # Send message
  msg <- function(){
    #message("")
    packageStartupMessage("initializing ...", appendLF = FALSE)
    Sys.sleep(1)
    packageStartupMessage(" done")
}
packageStartupMessage(msg())
# suppressMessages(msg())
options(scipen=999)
# options(quiet = FALSE)
# ggplot2::theme_set(theme_pub())
}
NULL




`%=%` <- function(x, y) {
    assign(as.character(substitute(x)), y, envir = parent.frame())
  }
NULL

`charopts` <- function(x) {
  paste(sprintf("\\code{\"%s\"}", x), collapse = ", ")
}
NULL

`say` <- function(){
  print(sample(c("Hello World!", "Yo world!", "Yo, you lookin' at twenty", "Bitch, you ain't givin' me any"),1))
}
NULL

# useful for avoinding extra space between columns
`is.wholenumber` <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}
NULL

`is.valid.name` <- function(x) {
  length(x) == 1 && is.character(x) && x == make.names(x)
  }
NULL

`is.empty` <- function(df) {
is.null(df) || nrow(df) == 0 || ncol(df) == 0
}
NULL

`is.discrete` <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}
NULL

`is.formula` <- function(x) inherits(x, "formula")

# This function takes a string referring to existing data and parses it
# to get information on the data structure.
#
# info returned: df.name, var.name, col.names, rows.subset, col.index, data.struct




`.Capitalise` <- function (x) {
  stopifnot(is.character(x))
  s <- strsplit(x, "", "")
  sapply(s, function(x) {
    paste(toupper(x[1]), paste(x[2:length(x)], collapse = ""),
          collapse = "", sep = "")
  })
}
NULL





# Round Numbers Without Leading Zeros
`.Rounded` <- function(x, digits=2, add=FALSE, max=(digits+2)){
  y <- round(x, digits=digits)
  yk <- format(y, nsmall=digits)
  nzero <- sum(unlist(y)==0)
  if(add==TRUE){
    while(nzero>0){
      zeros <- y==0
      digits <- digits+1
      y[zeros] <- round(x, digits=digits)[zeros]
      yk[zeros] <- format(y[zeros], nsmall=digits)
      nzero <- sum(y==0)
      if(digits>(max-1))
        nzero <- 0
    }
  }##--end of add zeros
  z <- sub("^([-]?)0[.]","\\1.", gsub(" +", "", yk))
  return(noquote(z))
}##--end of rounded
NULL






# Adds extra habilities to the base match.arg function:
`.Match` <- function(arg,
                     choices,
                     base = 1,
                     several.ok = FALSE,
                     numeric = FALSE,
                     ignore.case = TRUE)
{
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }
  if (is.character(arg)) {
    if (ignore.case) {
      choices = tolower(choices)
      arg = tolower(arg)
    }
    res = match.arg(arg = arg,
                    choices = choices,
                    several.ok = several.ok)
    if (numeric)
      res = which(choices %in% res) + base - 1
  } else if (is.numeric(arg)) {
    if ((arg < base) | (arg > (length(choices) + base - 1)))
      stop("'arg' should be between ",
           base,
           " and ",
           length(choices) + base - 1)
    if (numeric) {
      res = arg
    } else {
      res = choices[arg - base + 1]
    }
  } else
    stop("'arg' should be numeric or character")
  return(res)
}
NULL


# gplot() + geom_rect(aes(xmin=-1,ymin=-1,xmax=1,ymax=1), fill=NA) + coord_polar()

# circle1 <- Circlize(c(10,10),20,npoints = 100)
# circle2 <- Circlize(c(10,10),15,npoints = 100)
# circle3 <- Circlize(c(10,10),10,npoints = 100)

`Circlize` <- function(center = c(0,0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
NULL


# df <- data.frame(strategy=5:10, offering=c(5,7,9,8,6,9),labelz=c("PSDB","PSB","PMDB","PTB","PP","PT"))

# maxi <- max(df$offering)/30

# library(ggplot2)
# library(digest)

# ggplot(data=df, aes(x=strategy, y=offering)) +
#  geom_polygon(data=circle1, aes(x,y), fill = "#99CCFF") +
#  geom_polygon(data=circle2, aes(x,y), fill = "#6699CC") +
#  geom_polygon(data=circle3, aes(x,y), fill = "#336699") +
#  geom_point(aes(size=offering*strategy),color = "white") +
#  geom_point(aes(size=1)) +
#  geom_text(aes(label=labelz), nudge_y = maxi) +
#  coord_cartesian(xlim=c(0,10),ylim=c(0,10)) +
#  theme_bw()
