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
`.parse.arg` <- function(arg.str) {
  # Check if arg.str is a string
  if(!is.character(arg.str))
    stop("arg.str must be a string")
  # Initialise output list
  output <- list()
  output$arg.str <- arg.str
  # Recuperate the object designated by arg.str
  x <- try(eval(parse(text=arg.str)),silent = TRUE)
  if(inherits(x, "try-error")) {
    return(output)
  }
  if(!is.data.frame(x) && !is.atomic(x)) {
    return(output)
  }
  # Trim the string removing leading/trailing blanks
  arg.str <- gsub("^\\s+|\\s+$", "",
                  gsub( sprintf("\\s+[%s]\\s+|\\s+[%s]|[%s]\\s+",
                                " ", " ", " "), " ", arg.str))

  # Get rid of spaces next to brackets and next to comma in indexing brackets.
  # Note: that way assures us to not remove any spaces in quoted structures
  # such as ['var name']
  arg.str <- gsub("\\s*\\[\\s*","[", arg.str, perl=TRUE) # remove blanks near [
  arg.str <- gsub("\\s*\\]\\s*","]", arg.str, perl=TRUE) # remove blanks near ]

  # remove blanks around comma
  arg.str <- gsub("^(.*)(\\[\\d+:\\d+)?\\s?,\\s?(.+)$", "\\1\\2,\\3", arg.str, perl=TRUE)

  # Change [[]] to [] for the last pair of brackets; this simplifies the work
  arg.str <- sub("\\[{2}(.*)\\]{2}$", "[\\1]", arg.str, perl=TRUE)

  # Change references to data with ['name'] or [['name']] into $name, also to simplify matters
  re.brack <- '\\[{1,2}[\'\"]'
  if(grepl(re.brack, arg.str)) {
    arg.str <- gsub('\\[{1,2}[\'\"]', "$", arg.str, perl=TRUE)
    arg.str <- gsub('[\'\"]\\]{1,2}', "", arg.str, perl=TRUE)
  }

  # Isolate indexing in the last brackets
  re.index <- "(.*?)\\[(.*?)\\]$"

  if(grepl(re.index, arg.str)) {
    indexes <- sub(re.index, "\\2", arg.str, perl=TRUE)

    # Further decompose the indexes
    # indexing having 2 elements (rows, columns), will be identified by this regex
    # [1:10,] or [,"Species] will also match
    re.split.index <- "^(.+)?,+(c\\(.*\\)|\\d+|\\d+:\\d+|'.*'|\".+\")$"
    if(grepl(re.split.index, indexes, perl = TRUE)) {
      output$rows.subset <- sub(re.split.index, "\\1", indexes, perl=TRUE)
      output$col.index <- sub(re.split.index, "\\2", indexes, perl=TRUE)

      # Remove any empty string
      if(nchar(output$rows.subset) == 0)
        output$rows.subset <- NULL
      if(nchar(output$col.index) == 0)
        output$col.index <- NULL
    }

    # When previous regex does not match, it means the index has only 1 element,
    # either row or column.
    # When indexing starts with a comma:
    else if(substring(indexes, 1, 1) == ",")
      output$col.indexes <- sub("^,", "", indexes, perl = TRUE)
    # When indexing ends with a comma:
    else if(substring(indexes, nchar(indexes), nchar(indexes)) == ",")
      output$rows.subset <- sub(",$", "", indexes, perl = TRUE)

    # When there is no comma, we'll check if x is a dataframe or not.
    # If it is, the index refers to columns, and otherwise, to rows
    else {
      # first we need to reevaluate the arg.str
      x.tmp <- eval(parse(text = arg.str))
      if(is.data.frame(x.tmp))
        output$col.index <- indexes
      else
        output$rows.subset <- indexes
    }

    # Update the string to remove what's already accounted for
    arg.str <- sub(re.index, "\\1", arg.str, perl=TRUE)
  }

  # Split arg.str by "$" to identify structures
  output$data.struct <- strsplit(arg.str, "$", fixed = TRUE)[[1]]

  if(is.data.frame(x)) {
    # If x is a dataframe, we can set the col.names
    output$col.names <- colnames(x)

    # normally the last element in the data structures
    # should be the df name; unless it's nested in a list and referred to by [[n]]
    output$df.name <- utils::tail(output$data.struct,1)
  }

  # Otherwise, depending on the situation, we'll try to get at the df name and its colnames
  else {
    # If vector is referred to via column indexing, recup the column's name
    # by an evaluation of the form df[col.index]
    if("col.index" %in% names(output)) {
      output$var.name <- eval(parse(text=paste("colnames(",arg.str,"[",output$col.index,"])")))
      #output$col.names <- eval(parse(text=paste("colnames(",arg.str,"[",output$col.index,"])")))
      output$df.name <- utils::tail(output$data.struct,1)
    }

    # If there is no column indexing, it means the vector's name is in the
    # data.struc list, along with the df name one level higher, unless the vector
    # was "standalone"
    else {
      output$var.name <- utils::tail(output$data.struct,1)
      if(length(output$data.struct)>1)
        output$df.name <- output$data.struct[length(output$data.struct)-1]
    }
  }

  # remove last item from data.struct when it's the same as var.name to avoid redundancy
  output$data.struct <- setdiff(output$data.struct, output$var.name)

  # same with df.name and data.struct
  output$data.struct <- setdiff(output$data.struct, output$df.name)

  # cleanup
  if(length(output$data.struct)==0)
    output$data.struct <- NULL

  # Further validate the items to return;
  if(isTRUE(grepl('[\\(\\[]', output$df.name)))
    output$df.name <- NULL

  if(isTRUE(grepl('[\\(\\[]', output$var.name)))
    output$var.name <- NULL

  return(output)
}
NULL




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
                     ignore.case = TRUE){
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




