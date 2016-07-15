.onAttach <- function(lib, pkg) {
  ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  packageStartupMessage(paste(pkg, ver))
  options(scipen = 999)
  options(quiet = TRUE)
  #ggplot2::theme_set(theme_pub())
}
NULL
# SciencesPo_env <- new.env()


`%=%` <- function(x, y) {
  assign(as.character(substitute(x)), y, envir = parent.frame())
}
NULL

`charopts` <- function(x) {
  paste(sprintf("\\code{\"%s\"}", x), collapse = ", ")
}
NULL

# copied from ggplot2
"%||%" <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

# copied from ggplot2
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

`say` <- function() {
  print(sample(
    c(
      "Hello World!",
      "Yo world!",
      "Before there was R, there was S.",
      "Programming is like writing a book ...\n...except if you miss out a single comma on page 126 the whole thing makes no damn sense."
    ),
    1
  ))
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

`is.formula` <- function(x)
  inherits(x, "formula")

# This function takes a string referring to existing data and parses it
# to get information on the data structure.
#
# info returned: df.name, var.name, col.names, rows.subset, col.index, data.struct
`.parse.arg` <- function(arg.str) {
  # Check if arg.str is a string
  if (!is.character(arg.str))
    stop("arg.str must be a string")
  # Initialise output list
  output <- list()
  output$arg.str <- arg.str
  # Recuperate the object designated by arg.str
  x <- try(eval(parse(text = arg.str)), silent = TRUE)
  if (inherits(x, "try-error")) {
    return(output)
  }
  if (!is.data.frame(x) && !is.atomic(x)) {
    return(output)
  }
  # Trim the string removing leading/trailing blanks
  arg.str <- gsub("^\\s+|\\s+$", "",
                  gsub(
                    sprintf("\\s+[%s]\\s+|\\s+[%s]|[%s]\\s+",
                            " ", " ", " "),
                    " ",
                    arg.str
                  ))

  # Get rid of spaces next to brackets and next to comma in indexing brackets.
  # Note: that way assures us to not remove any spaces in quoted structures
  # such as ['var name']
  arg.str <-
    gsub("\\s*\\[\\s*", "[", arg.str, perl = TRUE) # remove blanks near [
  arg.str <-
    gsub("\\s*\\]\\s*", "]", arg.str, perl = TRUE) # remove blanks near ]

  # remove blanks around comma
  arg.str <-
    gsub("^(.*)(\\[\\d+:\\d+)?\\s?,\\s?(.+)$",
         "\\1\\2,\\3",
         arg.str,
         perl = TRUE)

  # Change [[]] to [] for the last pair of brackets; this simplifies the work
  arg.str <- sub("\\[{2}(.*)\\]{2}$", "[\\1]", arg.str, perl = TRUE)

  # Change references to data with ['name'] or [['name']] into $name, also to simplify matters
  re.brack <- '\\[{1,2}[\'\"]'
  if (grepl(re.brack, arg.str)) {
    arg.str <- gsub('\\[{1,2}[\'\"]', "$", arg.str, perl = TRUE)
    arg.str <- gsub('[\'\"]\\]{1,2}', "", arg.str, perl = TRUE)
  }

  # Isolate indexing in the last brackets
  re.index <- "(.*?)\\[(.*?)\\]$"

  if (grepl(re.index, arg.str)) {
    indexes <- sub(re.index, "\\2", arg.str, perl = TRUE)

    # Further decompose the indexes
    # indexing having 2 elements (rows, columns), will be identified by this regex
    # [1:10,] or [,"Species] will also match
    re.split.index <-
      "^(.+)?,+(c\\(.*\\)|\\d+|\\d+:\\d+|'.*'|\".+\")$"
    if (grepl(re.split.index, indexes, perl = TRUE)) {
      output$rows.subset <- sub(re.split.index, "\\1", indexes, perl = TRUE)
      output$col.index <-
        sub(re.split.index, "\\2", indexes, perl = TRUE)

      # Remove any empty string
      if (nchar(output$rows.subset) == 0)
        output$rows.subset <- NULL
      if (nchar(output$col.index) == 0)
        output$col.index <- NULL
    }

    # When previous regex does not match, it means the index has only 1 element,
    # either row or column.
    # When indexing starts with a comma:
    else if (substring(indexes, 1, 1) == ",")
      output$col.indexes <- sub("^,", "", indexes, perl = TRUE)
    # When indexing ends with a comma:
    else if (substring(indexes, nchar(indexes), nchar(indexes)) == ",")
      output$rows.subset <- sub(",$", "", indexes, perl = TRUE)

    # When there is no comma, we'll check if x is a dataframe or not.
    # If it is, the index refers to columns, and otherwise, to rows
    else {
      # first we need to reevaluate the arg.str
      x.tmp <- eval(parse(text = arg.str))
      if (is.data.frame(x.tmp))
        output$col.index <- indexes
      else
        output$rows.subset <- indexes
    }

    # Update the string to remove what's already accounted for
    arg.str <- sub(re.index, "\\1", arg.str, perl = TRUE)
  }

  # Split arg.str by "$" to identify structures
  output$data.struct <- strsplit(arg.str, "$", fixed = TRUE)[[1]]

  if (is.data.frame(x)) {
    # If x is a dataframe, we can set the col.names
    output$col.names <- colnames(x)

    # normally the last element in the data structures
    # should be the df name; unless it's nested in a list and referred to by [[n]]
    output$df.name <- utils::tail(output$data.struct, 1)
  }

  # Otherwise, depending on the situation, we'll try to get at the df name and its colnames
  else {
    # If vector is referred to via column indexing, recup the column's name
    # by an evaluation of the form df[col.index]
    if ("col.index" %in% names(output)) {
      output$var.name <-
        eval(parse(text = paste(
          "colnames(", arg.str, "[", output$col.index, "])"
        )))
      #output$col.names <- eval(parse(text=paste("colnames(",arg.str,"[",output$col.index,"])")))
      output$df.name <- utils::tail(output$data.struct, 1)
    }

    # If there is no column indexing, it means the vector's name is in the
    # data.struc list, along with the df name one level higher, unless the vector
    # was "standalone"
    else {
      output$var.name <- utils::tail(output$data.struct, 1)
      if (length(output$data.struct) > 1)
        output$df.name <-
          output$data.struct[length(output$data.struct) - 1]
    }
  }

  # remove last item from data.struct when it's the same as var.name to avoid redundancy
  output$data.struct <- setdiff(output$data.struct, output$var.name)

  # same with df.name and data.struct
  output$data.struct <- setdiff(output$data.struct, output$df.name)

  # cleanup
  if (length(output$data.struct) == 0)
    output$data.struct <- NULL

  # Further validate the items to return;
  if (isTRUE(grepl('[\\(\\[]', output$df.name)))
    output$df.name <- NULL

  if (isTRUE(grepl('[\\(\\[]', output$var.name)))
    output$var.name <- NULL

  return(output)
}
NULL




`.Capitalise` <- function (x) {
  stopifnot(is.character(x))
  s <- strsplit(x, "", "")
  sapply(s, function(x) {
    paste(toupper(x[1]),
          paste(x[2:length(x)], collapse = ""),
          collapse = "",
          sep = "")
  })
}
NULL





# Round Numbers Without Leading Zeros
`.Rounded` <- function(x,
                       digits = 2,
                       add = FALSE,
                       max = (digits + 2)) {
  y <- round(x, digits = digits)
  yk <- format(y, nsmall = digits)
  nzero <- sum(unlist(y) == 0)
  if (add == TRUE) {
    while (nzero > 0) {
      zeros <- y == 0
      digits <- digits + 1
      y[zeros] <- round(x, digits = digits)[zeros]
      yk[zeros] <- format(y[zeros], nsmall = digits)
      nzero <- sum(y == 0)
      if (digits > (max - 1))
        nzero <- 0
    }
  }##--end of add zeros
  z <- sub("^([-]?)0[.]", "\\1.", gsub(" +", "", yk))
  return(noquote(z))
}##--end of rounded
NULL




#' A replication of MatLab pause function.
#' x is optional. If x>0 a call is made to \code{\link{Sys.sleep}}. Else, execution pauses until a key is entered.
#' @export
`.Pause` <-
  function (x = 0) {
    if (x > 0) {
      Sys.sleep(x)
    } else{
      cat("Hit <enter> to continue...", "green")
      readline()
      invisible()
    }
  }
NULL




# Adds extra habilities to the base match.arg function:
`.Match` <- function(arg,
                     choices,
                     base = 1,
                     several.ok = FALSE,
                     numeric = FALSE,
                     ignore.case = TRUE) {
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

`Circlize` <-
  function(center = c(0, 0),
           diameter = 1,
           npoints = 100) {
    r = diameter / 2
    tt <- seq(0, 2 * pi, length.out = npoints)
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



# The argument can take a single age
# .DateToAge("1977-07-77")
.DateToAge <- function (x, refdate = Sys.time()) {
  lubridate::year(lubridate::as.period(lubridate::interval(x, refdate)))
}


# Convert Ages or Dates, to a Factor Variable of Standard Age Groups
#
AgeBreaker <-
  function(x,
           breaks = c(-Inf, 18, 25, 35, 45, 55, 65,+Inf),
           right = FALSE,
           ...) {
    .ageLab <- function(x) {
      levs <- levels(x)
      # The bounds of the range
      bounds <-
        stringr::str_extract_all(levs, "[[:digit:]]+|-Inf| Inf")

      labs <- unlist(lapply(bounds, function(x) {
        if (x[1] == "-Inf")
          return(paste("<", x[2]))
        if (x[2] == " Inf")
          return(paste(x[1], "+"))

        paste(x[1], "-", as.numeric(x[2]) - 1)
      }))

      factor(x, levels = levs, labels = labs)
    }

    if (class(x) %in% c("Date", "POSIXct", "POSIXt")) {
      .dateToAge <- function (dob, refdate = Sys.time()) {
        lubridate::year(lubridate::as.period(
          lubridate::interval(dob, refdate)
        ))
      }
      x <- .dateToAge(x)
    }

    .ageLab(cut(x, breaks, right = right, ...))
  }




# Attempt to Rationalise a Factor Variable
ConsolidateValues <- function(x, case = "lower",
                              na_regex = "no info*|don't know|<na>|#na|n/a|^[[:space:]]*$") {
  case_fun <- switch(case, "lower" = tolower, "upper" = toupper)
  # You should change the encoding here!
  # Change the case
  x <- case_fun(x)
  # Remove any space around slashes, and turn them all to /
  x <- gsub("[[:space:]]*/[[:space:]]*", "/", x)
  # Remove spaces before commas, colons, and semicolons
  # Pretty sure there's a nicer way of doing this with clever regex!
  x <- gsub("[[:space:]]*,[[:space:]]*", ", ", x)
  x <- gsub("[[:space:]]*;[[:space:]]*", "; ", x)
  x <- gsub("[[:space:]]*:[[:space:]]*", ": ", x)
  x <- gsub("[[:space:]]*-[[:space:]]*", " - ", x)
  # Remove spaces after currency symbols (just dollars for now)
  x <- gsub("\\$[[:space:]]*", "$", x)
  # Replace all underscores and full stops with spaces
  x <- gsub("\\.|_", " ", x)
  # Change all whitespace to a single space
  x <- gsub("[[:space:]]+", " ", x)
  # Strip all leading and trailing whitespace
  x <- gsub("^[[:space:]]|[[:space:]]$", "", x)
  # Remove blanks
  x[x == ""] <- NA
  # Remove with the NA regex
  x[grepl(na_regex, x)] <- NA
  x
}


# Only Valid Columns in a data.frame
ValidColumns <- function(x, empty.strings = TRUE) {
  n_levs <- function(x) {
    if (empty.strings)
      x[grepl("^[[:space:]]*$", x)] <- NA
    length(na.omit(unique(x)))
  }
  x[, as.vector(apply(x, 2, n_levs)) > 1]
}
NULL


PlusMinus <- function(x, ...) {
  paste0(ifelse(x > 0, "+", ""), round(x, ...))
}
NULL

PlusMinusPercent <- function(x, ...) {
  paste0(ifelse(x > 0, "+", ""), round(x * 100, ...), "%")
}
NULL




# Report rounding convention
# - 0 is written as "0"
# - values under 0.1 are written "<0.1"
# - from 0.1 to under 10 are written rounding 1 decimal place
# - 10 and above are written as 3 significant figures for rates and 2 significant figures for absolute numbers.
# - data that are not reported, but could be are represented as empty cells and should be accompanied by a footnote.
# - data that cannot be calculated, either because of missing data, data was not requested, or any other reason are represented with an en-dash (ctrl - on your keyboard).

# 0 is 0, under .1 to "<0.1", under 1 to 1 sig fig, otherwise 2 sig fig
round.conv <- function(x) {
  ifelse(x == 0, 0, ifelse(x < 0.1, "<0.1", ifelse(
    signif(x, 2) < 1,
    formatC(round(x, 1), format = 'f', digits = 1),
    ifelse(
      signif(x, 2) < 10,
      sapply(signif(x, 2), sprintf, fmt = "%#.2g"),
      signif(x, 2)
    )
  )))
}  # Note: The second method for trailing 0s does not work with 9.99

# rounding convention for rates
# 0 is 0, under .1 to "<0.1", under 1 to 1 sig fig, under 100 to 2 sig figs,
# otherwise 3 sig figs
round.conv.rates <- function(x) {
  ifelse(x == 0, 0, ifelse(x < 0.1, "<0.1", ifelse(
    signif(x, 2) < 10,
    formatC(round(x, 1), format = 'f', digits = 1),
    # ifelse(signif(x, 2) < 10, formatC(round(x,1), format='f', digits=1),
    ifelse(signif(x, 3) < 100, signif(x, 2), signif(x, 3))
  )))
}

# Depends on whether dealing with thousands or rates. In general, 0 is 0, under .1 to "<0.1", then appropriate sig figs.
# Amended by Hazim 2012-08-31 to fix double-rounding error, plus also
# changed so that numbers < 1 are only rounded to 1 sig fig
frmt <- function(x, rates = FALSE, thou = FALSE) {
  ifelse(x == 0, "0",
         ifelse(
           x < 0.01 & thou == TRUE,
           "<0.01",
           ifelse(
             x < 0.1 & thou == FALSE,
             "<0.1",
             ifelse(
               signif(x, 2) < 1 &
                 thou == TRUE,
               formatC(signif(x, 2), format = 'f', digits = 2),
               ifelse(
                 signif(x, 2) < 1,
                 formatC(signif(x, 1), format = 'f', digits = 1),
                 ifelse(
                   signif(x, 2) < 10,
                   formatC(signif(x, 2), format = 'f', digits = 1),
                   ifelse(
                     x > 1 &
                       rates == FALSE,
                     formatC(signif(x, 2), big.mark = " ", format = 'd'),
                     ifelse(
                       signif(x, 3) < 100,
                       formatC(signif(x, 2), big.mark = " ", format = 'd'),
                       formatC(signif(x, 3), big.mark = " ", format = 'd')
                     )
                   )
                 )
               )
             )
           )
         ))
}

# Simple rounder that just adds in the thousands separator
rounder <- function(x, decimals = FALSE) {
  if (decimals == TRUE) {
    ifelse(is.na(x), NA, ifelse(x == 0, 0, ifelse(
      x < 0.01, "<0.01", ifelse(
        round(x, 2) < 0.1,
        formatC(round(x, 2), format = 'f', digits = 2),
        ifelse(
          round(x, 1) < 10,
          formatC(round(x, 1), format = 'f', digits = 1),
          formatC(round(x, 0), big.mark = " ", format = 'd')
        )
      )
    )))
  }
  else
    ifelse(is.na(x), NA, ifelse(x == 0, 0, ifelse(
      x < 1, "< 1", formatC(round(x, 0), big.mark = " ", format = 'd')
    )))
}


.ggplot_to_gtable <- function(plot)
{
  if (methods::is(plot, "ggplot")) {
    ggplot2::ggplotGrob(plot)
  }
  else if (methods::is(plot, "gtable")) {
    plot
  }
  else{
    stop('Argument needs to be of class "ggplot" or "gtable"')
  }
}


describe_df <- function(x) {
  sprintf("\\code{data.frame} with %d observations of the following %d variables,",
          nrow(x), ncol(x))
}

df_format <- function(x) {
  template <- "%s\n\\describe{\n%s\n}"
  items <- sapply(names(x),
                  function(i) sprintf("\\item{\\code{%s}}{%s}", i,
                                      ifelse(is.null(comment(x[[i]])), i, comment(x[[i]]))))
  sprintf(template, describe_df(x), paste(items, collapse="\n"))
}



`IsExtrafontInstalled` <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    requireNamespace("extrafont")
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts.
            To enable full font support, run:
            install.packages('extrafont')
            font_import()")
    return(F)
  }
}
NULL





# Determines if a value is above or below a line
Intersection <- function(x.min, x.max, y, b, c) {
  y.min <- x.min * b + c
  y.max <- x.max * b + c

  if ((y.min <= y & y.max >= y) | (y.min >= y & y.max <= y)) {
    print("intersection")
  }
  else if (y > y.min) {
    print("above")
  }
  else {
    print("below")
  }
}
NULL




rbind_gtable_max <- function(...) {

  gtl <- list(...)
  stopifnot(all(sapply(gtl, gtable::is.gtable)))
  bind2 <- function (x, y) {
    stopifnot(ncol(x) == ncol(y))
    if (nrow(x) == 0)
      return(y)
    if (nrow(y) == 0)
      return(x)
    y$layout$t <- y$layout$t + nrow(x)
    y$layout$b <- y$layout$b + nrow(x)
    x$layout <- rbind(x$layout, y$layout)
    x$heights <- insert_unit(x$heights, y$heights)
    x$rownames <- c(x$rownames, y$rownames)
    x$widths <- grid::unit.pmax(x$widths, y$widths)
    x$grobs <- append(x$grobs, y$grobs)
    x
  }
  Reduce(bind2, gtl)

}

cbind_gtable_max <- function(...) {

  gtl <- list(...)
  stopifnot(all(sapply(gtl, gtable::is.gtable)))
  bind2 <- function (x, y) {
    stopifnot(nrow(x) == nrow(y))
    if (ncol(x) == 0)
      return(y)
    if (ncol(y) == 0)
      return(x)
    y$layout$l <- y$layout$l + ncol(x)
    y$layout$r <- y$layout$r + ncol(x)
    x$layout <- rbind(x$layout, y$layout)
    x$widths <- insert_unit(x$widths, y$widths)
    x$colnames <- c(x$colnames, y$colnames)
    x$heights <- grid::unit.pmax(x$heights, y$heights)
    x$grobs <- append(x$grobs, y$grobs)
    x
  }
  Reduce(bind2, gtl)

}

insert_unit <- function (x, values, after = length(x)) {
  lengx <- length(x)
  if (lengx == 0)
    return(values)
  if (length(values) == 0)
    return(x)
  if (after <= 0) {
    grid::unit.c(values, x)
  }
  else if (after >= lengx) {
    grid::unit.c(x, values)
  }
  else {
    grid::unit.c(x[1L:after], values, x[(after + 1L):lengx])
  }
}


