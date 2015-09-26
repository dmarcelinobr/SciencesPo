#' @title Add variable label
#'
#' @description \code{label(x)} retrieves the \code{label} attribute of \code{x}, while \code{label(x) <- "a label"} stores the label attribute, and also puts the class \code{labelled} as the first class of \code{x}. The \code{label} function can optionally append a \code{"units"}, usually a string or expression suitable for plotting.
#'
#' @details The \code{"units"} attributes are lost upon subsetting.
#'
#' @param x A character string.
#' @param self Logical, where to interact with the object or its components.
#' @param units Set to \code{TRUE} to append the \code{'units'} attribute (if present) to the returned label.  The \code{'units'} are surrounded by brackets.
#'
#'
#' @param plot set to \code{TRUE} to return a label suitable for \R's \code{plotmath} facility (returns an expression instead of a character string) if R is in effect.  If \code{units} is also \code{TRUE}, and if both \code{'label'} and \code{'units'} attributes are present, the \code{'units'} will appear after the label but in smaller type andwill not be surrounded by brackets.

#' @param default if \code{x} does not have a \code{'label'} attribute and \code{default} (a character string) is specified, the label will be taken as \code{default}.  For \code{labelLatex} the \code{default} is the name of the first argument if it is a variable and not a label.

#' @param grid Currently \R's \code{lattice} and \code{grid} functions do not support \code{plotmath} expressions for \code{xlab} and \code{ylab} arguments.  When using \code{lattice} functions in \R, set the argument \code{grid} to \code{TRUE} so that \code{labelPlotmath} can return an ordinary character string instead of an expression.
#' @param value the label of the object, or "".
#' @param \dots a list of variables or expressions to be formed into a \code{list}. Ignored for \code{print.labelled}.
#'
#' @examples
#' year <- c(2010,2012,2014,2016)
#' support   <- sample(60, 4)
#' label(year) <- "Election Years"
#' units(year) <- '%'
#' plot(year, support, xlab=label(year))
#'
#' data <- data.frame(year=year, support=support)
#' label(data)
#'
#' label(data, self=TRUE) <- "This is a data.frame"
#' label(data, self=TRUE)
#'
#' @export
#'
`label` <- function(x, default=NULL, ...){
  UseMethod("label")}

#' @rdname label
#' @export
`label.default` <- function(x, default=NULL, units=plot, plot=FALSE,
                          grid=FALSE, ...)
{
  if(length(default) > 1)
    stop("the default string cannot be of length greater then one")

  at <- attributes(x)
  lab <- at$label
  if(length(default) && (!length(lab) || lab==''))
    lab <- default

  un  <- at$units
  .labelPlotmath(lab,
                if(units) un else NULL,
                plotmath=plot, grid=grid)
}


#' @rdname label
#' @export
`label.data.frame` <- function(x, default=NULL, self=FALSE, ...) {
  if(self) {
    label.default(x)
  } else {
    if(length(default) > 0 && length(default) != length(x)) {
      stop('length of default must same as x')
    } else if(length(default) == 0) {
      default <- list(default)
    }

    labels <- mapply(FUN=label, x=x, default=default, MoreArgs=list(self=TRUE), USE.NAMES=FALSE)
    names(labels) <- names(x)
    return(labels)
  }
}


#' @rdname label
#' @export
"label<-" <- function(x, ..., value) UseMethod("label<-")


#' @rdname label
#' @export
"label<-.default" <- function(x, ..., value)
{
  if(is.list(value)) {
    stop("cannot assign a list to be a object label")
  }

  if(length(value) != 1L) {
    stop("value must be character vector of length 1")
  }

  attr(x, 'label') <- value

  if('labelled' %nin% class(x)) {
    class(x) <- c('labelled', class(x))
  }
  return(x)
}


#' @rdname label
#' @export
"label<-.data.frame" <- function(x, self=TRUE, ..., value) {
  if(!is.data.frame(x)) {
    stop("x must be a data.frame")
  }

  if(missing(self) && is.list(value)) {
    self <- FALSE
  }

  if(self) {
    xc <- class(x)
    xx <- unclass(x)
    label(xx) <- value
    class(xx) <- xc
    return(xx)
  } else {
    if(length(value) != length(x)) {
      stop("value must have the same length as x")
    }

    for (i in seq(along.with=x)) {
      label(x[[i]]) <- value[[i]]
    }
  }

  return(x)
}




.valueTagAttrs <- c(label="label", units="units", name="shortlabel")


valueTags <- function(x)
  attributes(x)[names(attributes(x)) %in% .valueTagAttrs]

"valueTags<-" <- function(x, value) {
  if(is.null(value) || length(value) == 0) {
    attributes(x)[names(attributes(x)) %in% .valueTagAttrs] <- NULL
    class(x) <- class(x)[class(x) != 'labelled']
    return(x)
  }

  if(!is.list(value)) {
    stop("list must be a named list of valueTags")
  }

  value[(!names(value) %in% .valueTagAttrs) |
        unlist(lapply(value, is.null))] <- NULL

  if(length(value) == 0) {
    attributes(x)[names(attributes(x)) %in% .valueTagAttrs] <- NULL
    class(x) <- class(x)[class(x) != 'labelled']
    return(x)
  }

  attributes(x)[setdiff(names(attributes(x))[names(attributes(x)) %in%
                                             .valueTagAttrs],
                        names(value))] <- NULL

  consolidate(attributes(x)) <- value

  if(all(class(x) != 'labelled'))
    class(x) <- c('labelled', class(x))

  return(x)
}




"[.labelled"<- function(x, ...) {
  tags <- valueTags(x)
  x <- NextMethod("[")
  valueTags(x) <- tags
  x
}

"print.labelled"<- function(x, ...) {
  x.orig <- x
  u <- attr(x,'units')
  if(length(u))
    attr(x,'units') <- NULL   # so won't print twice

  cat(attr(x, "label"),
      if(length(u))
        paste('[', u, ']', sep=''),
      "\n")

  attr(x, "label") <- NULL
  class(x) <-
    if(length(class(x))==1 && class(x)=='labelled')
      NULL
  else
    class(x)[class(x) != 'labelled']

  ## next line works around print bug
  if(!length(attr(x,'class')))
    attr(x,'class') <- NULL

  NextMethod("print")
  invisible(x.orig)
}



`.labelPlotmath` <- function(label, units=NULL, plotmath=TRUE, grid=FALSE,
                          chexpr=FALSE)
{
  if(!length(label)) label <- ''

  if(!length(units) || (length(units)==1 && is.na(units))) units <- ''

  g <-
    if(plotmath) function(x, y=NULL, xstyle=NULL, ystyle=NULL)
    {
      h <- function(w, style=NULL)
        if(length(style)) sprintf('%s(%s)', style, w) else w

      tryparse <- function(z, original, chexpr) {
        p <- try(parse(text=z), silent=TRUE)
        if(is.character(p)) original else
          if(chexpr) sprintf('expression(%s)', z) else p
      }
      if(!length(y))
        return(tryparse(h(.plotmathTranslate(x), xstyle), x, chexpr))

      w <- paste('list(',h(.plotmathTranslate(x), xstyle), ',',
                 h(.plotmathTranslate(y), ystyle), ')', sep='')
      tryparse(w, paste(x, y), chexpr)
    } else function(x, y=NULL, ...) if(length(y)) paste(x,y) else x

  if(units=='') g(label)
  else if(label=='') g(units)
  else if(plotmath)
    g(label, units, ystyle='scriptstyle')
  else paste(label,' [',units,']',sep='')
}


`.plotmathTranslate` <- function(x)
{
  if(length(grep('paste', x))) return(x)

  specials <- c(' ','%','_')
  spec <- FALSE
  for(s in specials)
    if(length(grep(s,x)))
      spec <- TRUE

  if(spec) x <- paste('paste("',x,'")',sep='')
  else if(substring(x,1,1)=='/') x <- paste('phantom()', x, sep='')
  x
}




`consolidate` <- function(x, value, protect, ...) {
  UseMethod("consolidate")
}

'consolidate<-' <- function(x, protect, ..., value)
  eval.parent(replace(match.call(expand.dots=FALSE), list=1,
                      values=list(as.name("consolidate"))))

`consolidate.default` <- function(x, value, protect=FALSE, ...) {
  if(missing(x) || is.null(x))
    x <- vector()

  if(missing(value) || is.null(value))
    value <- vector()

  xNames <- names(x)
  valueNames <- names(value)

  if(is.null(xNames) || is.null(valueNames) || all(valueNames == "") ||
     all(xNames == ""))
    return(c(x, value))

  vars <- intersect(xNames, valueNames[valueNames != ""])
  if(!protect)
    x[vars] <- value[vars]

  c(x, value[!valueNames %in% vars])
}


#' Append attribute to an object
#'
#' @description Sets or retrieves the \code{"units"} attribute of an object.
#'
#' @param x A character string.
#' @param \dots Ignored for parameters.
#' @param value the units of the object, or "".
#' @param none value to which to set result if no appropriate attribute is found.
#' @export
`units` <- function(x, ...){
  UseMethod("units")}

#' @rdname units
#' @export
"units<-.default"  <- function(x, value)
{
  attr(x, "units") <- value
  x
}

#' @rdname units
#' @export
`units.default` <- function(x, none='', ...)
{
  lab <- attr(x, "units")
  if(is.null(lab))
    lab <- attr(attr(x,'tspar'),'units')

  if(is.null(lab))
    lab <- none

  lab
}



