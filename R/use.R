#' @encoding UTF-8
#' @title Attach exclusively various file formats
#'
#' @description This works rigorously as the \pkg{epicalc}'s \code{use} function, though limited for the file formats it can read. Fundamentally, it replaces the command attach of R and save an object with extension \code{.data}, which becomes the default dataset. All other \code{data.frames} will be detached, by the time of using the \code{use} function, unless the argument \code{clear = FALSE} is specified.
#'
#' @param file The name of the file which the data are to be read from.
#' @param data The internal name after attaching the data file.
#' @param clear If \code{clear = TRUE}, all attached data in the environment will be detached first.
#' @param spss.missing Whether SPSS missing values should be replaced with NA; default is \code{spss.missing = TRUE}.
#' @param tolower Whether variable names should be forced to lower case; default is \code{tolower = TRUE}.
#'
#' @details By using this \dQuote{attach} version, the data becomes available globally usually positioned in the second place, \code{search()}.
#'
#' @importFrom foreign read.dta
#' @importFrom foreign read.spss
#' @importFrom foreign read.dbf
#'
#' @examples
#' use(ssex)
#'
#' @export
`use` <-
  function (file, data = .data, clear = TRUE, spss.missing = TRUE, tolower = TRUE)
  {
    if (clear) {
      detach.all()
    }
    if (is.character(file)) {
      ext <- tolower(substring(file, first = nchar(file) -
                                 3, last = nchar(file)))
      if (ext == ".dta") {
        data1 <- read.dta(file)
      }
      else {
        if (ext == ".dbf") {
          data1 <- read.dbf(file)
          if (tolower)
            names(data1) <- tolower(names(data1))
        }
        else {
          if (ext == ".sav") {
            data0 <- read.spss(file)
            var.labels <- attr(data0, "variable.labels")
            data1 <- read.spss(file, to.data.frame=TRUE, trim.factor.names=TRUE)
            data1 <- data1[1:nrow(data1), 1:ncol(data1)]
            attr(data1, "var.labels") <- var.labels
            if(spss.missing){
              for(i in 1:ncol(data1)){
                if(!is.null(attr(data0, "missing")[[i]]$value)){
                  data1[,i] <- ifelse((data1[,i] %in% attr(data0, "missing")[[i]]$value),NA,data1[,i])
                }
                if(!is.null(attributes(data0[[i]])$value.labels)){
                  data1[,i] <- ifelse((data1[,i] %in% attributes(data0[[i]])$value.labels),NA,data1[,i])
                }
              }
            }
            if (tolower)
              names(data1) <- tolower(names(data1))
          }
          else {
            if (substring(file, first = nchar(file) -
                          3, last = nchar(file)) == ".csv") {
              data1 <- read.csv(file, header = TRUE,
                                sep = ",")
            }
            else {
              stop("This type of file cannot be 'use'd.")
            }
          }
        }
      }
    }
    else {
      if (is.data.frame(file)) {
        data1 <- file
      }
      else {
        stop("The argument is not a data frame or no such file")
      }
    }
    nr <- nrow(data1);
    nc <- ncol(data1);
    #assign(as.character(substitute(data)), data1, pos = 1)
    #attach(data1, name = as.character(substitute(data)),
     #      warn.conflicts = FALSE)
    #message(paste0('[', nr, " x ", nc, ']', " assigned to `.data`", sep=""));
    message(paste0("A kind of ", '[', nr, " x ", nc, ']', " data object.", sep=""));
  }
NULL
