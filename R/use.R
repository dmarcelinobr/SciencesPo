#' @encoding UTF-8
#' @title Attach exclusively various file formats
#' 
#' @description This works rigoroulsy as the \pkg{epicalc}'s \code{use} function, though limited for the file formats it can read. Fundamentally, it replaces the command attach of R and save an object with extension \code{.data}, which becomes the default dataset. All other \code{data.frames} will be detached, by the time of using the \code{use} function, unless the argument \code{clear=FALSE} is specified.
#' 
#' @param file the name of the file which the data are to be read from.
#' @param data the internal name after attaching the data file.
#' @param clear if \code{TRUE}, all attached data in the environment will be detached first.
#' @param spss.missing , whether the values for missing is a SPSS dataset should be replaced with NA, the default is \code{spss.missing = TRUE}.
#' @param tolower  whether the variable names should be forced to lower case, default is \code{tolower = TRUE}.
#' 
#' @details By using this \dQuote{attach} version, the data becomes available globally usually positioned in the second place, see for instance \code{search()}. 
#' 
#' @importFrom foreign read.dta 
#' @importFrom foreign read.spss
#' 
#' @examples
#' data(ssex)
#' use(ssex)
#' 
#' @export

"use" <-
  function (file, data = .data, clear = TRUE, spss.missing = TRUE, tolower = TRUE) 
  {
    
    if (clear) {
      detachAll()
    }
    if (is.character(file)) {
      ext <- tolower(substring(file, first = nchar(file) - 
                                 3, last = nchar(file)))
      if (ext == ".dta") {
        dataset <- read.dta(file)
      }
      else {
        if (ext == ".sav") {
          data0 <- read.spss(file)
          var.labels <- attr(data0, "variable.labels")
          dataset <- read.spss(file, to.data.frame=TRUE, trim.factor.names=TRUE)
          dataset <- dataset[1:nrow(dataset), 1:ncol(dataset)]
          attr(dataset, "var.labels") <- var.labels
          if(spss.missing){
            for(i in 1:ncol(dataset)){
              if(!is.null(attr(data0, "missing")[[i]]$value)){
                dataset[,i] <- ifelse((dataset[,i] %in% attr(data0, "missing")[[i]]$value),NA,dataset[,i])
              }
              if(!is.null(attributes(data0[[i]])$value.labels)){
                dataset[,i] <- ifelse((dataset[,i] %in% attributes(data0[[i]])$value.labels),NA,dataset[,i])
              }
          }
          if (tolower) 
            names(dataset) <- tolower(names(dataset))
        }
        else {
          if (substring(file, first = nchar(file) - 
                          3, last = nchar(file)) == ".tsv") {
            dataset <- read.delim(file, header = TRUE, 
                                sep = "\t", stringsAsFactors=FALSE)
          }
          else {
            if (substring(file, first = nchar(file) - 
                            3, last = nchar(file)) == ".csv") {
              dataset <- read.csv(file, header = TRUE, 
                                sep = ",", stringsAsFactors=FALSE )
            }
            else {
              stop("This type of file cannot be 'used'.")
            }
          }
        }
      }
    }
  }
else {
  if (is.data.frame(file)) {
    dataset <- file
  }
  else {
    stop("The argument is not a data frame or no such file")
  }
}
assign(as.character(substitute(data)), dataset, pos = 1)
attach(dataset, name = as.character(substitute(data)), 
       warn.conflicts = FALSE)
}
