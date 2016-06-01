#' @encoding UTF-8
#' @title Simple Frequency Table
#'
#' @description Creates a frequency table or data frame.
#'
#' @param x A vector of values for which the frequency is desired.
#' @param weighs A vector of weights.
#' @param breaks one of: 1) a vector giving the breakpoints between histogram
#' cells; 2) a function to compute the vector of breakpoints; 3) a single
#' number giving the number of cells for the histogram; 4) a character string
#' naming an algorithm to compute the number of cells (see 'Details'); 5) a
#' function to compute the number of cells.
#' @param digits The number of significant digits required.
#' @param include.lowest Logical; if \code{TRUE}, an x[i] equal to the breaks value will be included in the first (or last) category or bin.
#' @param order The order method.
#' @param perc logical; if \code{TRUE} percents are returned.
#' @param useNA Logical; if \code{TRUE} NA's values are included.
#' @param \dots Additional arguements (currently ignored)
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @seealso \code{\link{Frequency}}, \code{\link{Crosstable}}.
#'
#' @examples
#' data(presheights)
#'
#' freq(presheights$winner.party)
#'
#'
#' @rdname freq
#' @export
`freq` <-
  function(x,
           weighs = NULL,
           breaks = graphics::hist(x, plot = FALSE)$breaks,
           digits = 3,
           include.lowest = TRUE,
           order = c("desc", "asc", "level", "name"),
           perc = FALSE,
           useNA = c("no", "ifany", "always"),
           ...)
    UseMethod("freq")

#' @rdname freq
#' @export
`freq.default` <-
  function(x,
           weighs = NULL,
           breaks = graphics::hist(x, plot = FALSE)$breaks,
           digits = 3,
           include.lowest = TRUE,
           order = c("desc", "asc", "level", "name"),
           perc = FALSE,
           useNA = c("no", "ifany", "always"),
           ...) {
    # check if x is a vector (do not use is.vector())
    if (!(is.atomic(x) || is.list(x)))
      stop("'x' must be a vector")

    if (inherits(x, "table")) {
      TABLE <- x

    } else {
      if (is.numeric(x)) {
        x <-
          base::cut(
            x,
            breaks = breaks,
            include.lowest = include.lowest,
            ordered_result = TRUE,
            ...
          )
      }

      TABLE <- table(x, useNA = useNA)
    }

    # how should the table be sorted, by name, level or frq? (NULL means "desc")
    switch(
      .Match(
        arg = order,
        choices = c("desc", "asc", "level", "name")
      ),
      level  = {

      }
      ,
      name   = {
        TABLE <- TABLE[rownames(TABLE)]
      }
      ,
      asc    = {
        TABLE <- sort(TABLE)
      }
      ,
      desc   = {
        TABLE <- -sort(-TABLE)
      }
    )

    ptab <- base::prop.table(TABLE)
    names(TABLE)[is.na(names(TABLE))] <- "<NA>"

    output <- data.frame(
      class = names(TABLE),
      Freq = as.vector(TABLE[]),
      Prop = if(perc == TRUE){100*round(as.vector(ptab[]), digits)} else {round(as.vector(ptab[]), digits)}
    )
    #cumfreq = cumsum(TABLE[]), cumperc = round(cumsum(ptab[]),dig))
    rownames(output) <- NULL # enumerate from 1:nrow(z)
    class(output) <- c("SciencesPo", class(output))
    attr(output, "scpo.type") <- "Standard"
    return(output)
  }##-end of freq
NULL





#' @encoding UTF-8
#' @title Frequency Table
#'
#' @description Simulating the FREQ procedure of SPSS.
#'
#' @param .data The data.frame.
#' @param x A column for which a frequency of values is desired.
#' @param verbose A logical value, if \code{TRUE}, extra statistics are also provided.
#' @param \dots Additional arguements (currently ignored)
#'
#' @seealso \code{\link{freq}}, \code{\link{Crosstable}}.
#'
#' @examples
#' data(cathedrals)
#'
#' Frequency(cathedrals, Type)
#'
#' # may work with operators like %>%
#' cathedrals %>% Frequency(Height)
#'
#' @importFrom stats sd
#' @rdname Frequency
#' @aliases Freq
#' @export
`Frequency` <-
  function(.data, x=NULL, verbose = TRUE, ...)
    UseMethod("Frequency")


#' @rdname Frequency
#' @export
`Frequency.default` <- function(.data, x=NULL, verbose = TRUE, ...) {
  vec <- eval(substitute(x), .data, parent.frame())
  nmiss = sum(is.na(vec))
  fsum = summary(factor(vec))
  ftab = cbind(fsum, 100 * fsum/sum(fsum))
  if (nmiss == 0) {
    ftab = cbind(ftab, 100 * cumsum(fsum)/sum(fsum))
    colnames(ftab) = c("Frequency", " Valid Percent", " Cum Percent")
    ftab[, 2] <- round(ftab[, 2], 2)
    ftab[, 3] <- round(ftab[, 3], 2)
    if (verbose == FALSE) {
      cat("\n")
      return(ftab)
    }
    cat("\n")
    print(ftab)
  }
  else {
    ftab = cbind(ftab, 100 * fsum / sum(fsum[1:(length(fsum) - 1)]), 100 * cumsum(fsum) /sum(fsum[1:(length(fsum) - 1)]))
    ftab[length(fsum), 3:4] = NA
    ftab[, 2] <- round(ftab[, 2], 2)
    ftab[, 3] <- round(ftab[, 3], 2)
    ftab[, 4] <- round(ftab[, 4], 2)
    cat("\n")
    cat("--------------------------------------------------------\n")
    colnames(ftab) = c("Frequency", "   Percent", " Valid Percent", " Cum Percent")
    if (dim(ftab)[1] == length(levels(vec)))
    {
      rownames(ftab)[1:length(levels(factor(vec)))] = levels(factor(vec))
    }
    if (verbose == FALSE) {
      return(ftab)
    }
    print(ftab)
  }

  cat("--------------------------------------------------------\n")
  cat("Total", rep(" ", 8-trunc(log10(sum(fsum)))), sum(fsum), "\n", sep = "")
  cat("\n")

  if (length(attributes(vec)$class) != 0)
  {
    if ("factor" %in% attributes(vec)$class)
    {
      cat("Warning: Statistics may not be meaningful for factors!\n\n")
    }
  }

  s1 = cbind(mean(as.numeric(vec), na.rm = TRUE),
             stats::sd(as.numeric(vec), na.rm = TRUE))
  rownames(s1) = " "
  colnames(s1) = c("       Mean", "          Std dev")
  print(s1)
  s2 = cbind(min(as.numeric(vec), na.rm = TRUE), max(as.numeric(vec), na.rm =
                                                       TRUE))
  rownames(s2) = " "
  colnames(s2) = c("    Minimum", "          Maximum")
  print(s2)
  s3 = cbind(sum(!is.na(vec)), nmiss)
  rownames(s3) = " "
  colnames(s3) = c("Valid cases",  "    Missing cases")
  print(s3)
  cat("\n")
}#--end of Freq
NULL


#' @export
#' @rdname Frequency
Freq <- Frequency

