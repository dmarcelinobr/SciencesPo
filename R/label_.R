#' Return or Assign a Variable Label
#'
#' @description Display a variable label for output, either text output at the console or graphics, such as a title on a graph.
#'
#'  @param x The variable for which to obtain the corresponding variable label.
#'  @param value If assigned, then the specified data frame is updated with this asssigned label.
#'  @param data Data frame that contains the variable of interest. The output of the function is assigned to this data frame.
#'
#' @examples
#'
#' .data <- data.frame(AgeGroup=factor(
#'  1:9,
#'  labels= c("Younger than 16", "16-24",
#'            "25-34", "35-44", "45-54",
#'            "55-64", "65-74",
#'            "Older than 74", "(blank)"),
#'  ordered=TRUE),
#'  No=c(34, 2079, 2585, 1593,
#'       1274, 802, 291, 78, 13),
#'  Yes=c(18, 1970, 4035, 2328,
#'        1707, 924, 386, 68, 4))
#'
#' dat <- label(AgeGroup, "Age group")
#'
#'
#' @export
label <-
function(x, value=NULL, data=.data) {

  x.name <- deparse(substitute(x))
  options(xname = x.name)

  # get data frame name
  dname <- deparse(substitute(data))
  options(dname = dname)

  if (nchar(x.name)>0) if (!exists(x.name, where=data)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "For data frame: ", dname, "\n",
    "This variable does not exist: ", x.name, "\n")
  }

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  in.global <- xs$ig

  # see if the data frame exists, if x not in Global Env or function call
  if (!in.global) {
    if (!exists(dname)) {
      if (dname == ".data")
        txtA <- ", the default data frame name, " else txtA <- " "
      txtB1 <- "Create the labels by reading with Read and labels option\n"
      txtB <- paste(txtB1, sep="")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Data frame ", dname, txtA, "does not exist\n\n", txtB, "\n")
    }
  }

  if (is.null(value)) {  # display an existing label
    if (!is.null(x.name)) {
      gl <- .getlabels()
      lbl <- gl$xl
      if (is.null(lbl)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable label does not exist for variable: ", x.name, "\n\n")
      }
      cat(x.name, ": ", lbl, "\n", sep="")
     }
    else {
      mylabels <- attr(data, which="variable.labels")
      for (i in 1:length(mylabels))
        cat(names(mylabels)[i], ": ", mylabels[i], "\n", sep="")
    }
  }

  else {  # assign a label to a var in a data frame and return data frame
    mylabels <- attr(data, which="variable.labels")
    lbl.len <- length(mylabels)
    if (x.name %in% names(mylabels)) { #cat("IS IN\n")
      lbl.index <- which(names(mylabels) == x.name)
      indx <- lbl.index
    }
    else
      indx <- length(mylabels) + 1
    mylabels[indx] <- value
    names(mylabels)[indx] <- x.name
    cat("\n")
    cat("Variable Name:",  names(mylabels)[indx], "\n")
    cat("Variable Label:", mylabels[indx], "\n")
    cat("\n")
    attr(data, which="variable.labels") <- mylabels
    return(data)
  }

  cat("\n")
}
NULL



#' List the Values of a Variable
#'
#' List the values of a variable from the global environment or a data frame.
#' @param x Variable for which to construct the histogram and density plots.
#' @param data Data frame that contains the variable of interest, default is \code{.data}.
#' @param \dots Other parameter values for as defined processed by \code{\link{print}}, including \code{digits}.
#'
#' @examples
#' .data <- data.frame(AgeGroup=factor(
#'  1:9,
#'  labels= c("Younger than 16", "16-24",
#'            "25-34", "35-44", "45-54",
#'            "55-64", "65-74",
#'            "Older than 74", "(blank)"),
#'  ordered=TRUE),
#'  No=c(34, 2079, 2585, 1593,
#'       1274, 802, 291, 78, 13),
#'  Yes=c(18, 1970, 4035, 2328,
#'        1707, 924, 386, 68, 4))
#'
#' values(Yes)
#' @export
values <-
function(x, data=.data, ...) {

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))

  # get data frame name
  dname <- deparse(substitute(data))

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  in.global <- xs$ig

  # see if variable exists in the data frame, if x not in Global Env or function call
  if (!missing(x) && !in.global) .xcheck(x.name, dname, data)

  if (!in.global) x.call <- eval(substitute(data$x))
  else {  # vars that are function names get assigned to global
    x.call <- x
    if (is.function(x.call)) x.call <- eval(substitute(data$x))
  }

  print(x.call, ...)

}
