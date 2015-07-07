#' @encoding UTF-8
#' @title Rename a variable
#' @description Rename a variable.
#' @param old a variable or a pattern among the names of the variables inside data.
#' @param new new name or new pattern of the variable(s).
#' @param data a data.frame of which the variables will be renamed.
#' @param \dots  further arguments passed to or used by other methods.
#' @export
`rename` <- function(old, new, data, ...){
  UseMethod("rename")
}

#' @rdname rename
#' @export
`rename.default` <- function (old, new, data, ...)
{
  dataset <- data
  if (any(names(dataset) == as.character(substitute(old)))) {
    names(dataset)[names(dataset) == as.character(substitute(old))] <- as.character(substitute(new))
    assign(as.character(substitute(data)), value=dataset, envir = sys.frame(-1))
    if(is.element(as.character(substitute(data)), search())){
      detach(pos=which(search() %in% as.character(substitute(data))))
      # attach(dataset, name=as.character(substitute(data)), warn.conflicts = FALSE)
    }
  }
  else {
    if (length(grep(pattern = old, x = names(dataset))) > 0) {
      rename.pattern(old, new, printNote = TRUE, data)
    }
    else {
      stop(paste("\n", "\"", as.character(substitute(old)),
                 "\"", " is neither a var name nor an available pattern"))
    }
  }
}

#' @examples
#' data(ssex)
#' rename("Oppose", "NO", data = ssex)
#'
## Rename a variable
#' @rdname rename
#' @export
`rename.var` <- function (old, new, data, ...)
{
  dataset <- data
  pos = 1 # does nothing just to trick the environment
  if (any(names(dataset) == as.character(substitute(old)))) {
    names(dataset)[names(dataset) == as.character(substitute(old))] <- as.character(substitute(new))

    assign(as.character(substitute(data)), value=dataset, envir = sys.frame(-1))
    if(is.element(as.character(substitute(data)), search())){
      detach(pos=which(search() %in% as.character(substitute(data))))
      #attach(dataset, name=as.character(substitute(data)), warn.conflicts = FALSE)
    }
  }
  else {
    if (any(names(dataset) == old)) {
      names(dataset)[names(dataset) == old] <- as.character(substitute(new))
      assign(as.character(substitute(data)), value=dataset, envir = sys.frame(-1))
      if(is.element(as.character(substitute(data)), search())){
        detach(pos=which(search() %in% as.character(substitute(data))))
        #attach(dataset, name=as.character(substitute(data)), warn.conflicts = FALSE)
      }
    }
    else {
      stop(paste("\n", "\"", as.character(substitute(old)),
                 "\"", " does not exist in the data frame",
                 sep = ""))
    }
  }
}


#' @rdname rename
#' @param verbose whether the old and new names of the variables(s) should be printed out.
#' @export
`rename.pattern` <- function (old, new, data, verbose = TRUE, ...)
{
  dataset <- data
  pos = 1 # does nothing just to trick the environment
  if (length(grep(pattern = old, x = names(dataset))) == 0)
    stop(paste("Pattern ", "\"", as.character(substitute(old)),
               "\"", " does not exist", sep = ""))
  table1 <- cbind(names(dataset)[grep(pattern = old, x = names(dataset))],
                  sub(pattern = old, replacement = new, x = names(dataset))[grep(pattern = old,
                                                                                 x = names(dataset))])
  rownames(table1) <- rep("    ", length(names(dataset)[grep(pattern = old,
                                                             x = names(dataset))]))
  colnames(table1) <- c("Old var names  ", "New var names")
  if (verbose) {
    cat("Note the following change(s) in variable name(s):",
        "\n")
    print(table1)
  }
  names(dataset) <- sub(pattern = old, replacement = new, x = names(dataset))
  assign(as.character(substitute(data)), value=dataset, envir = sys.frame(-1))
  if(is.element(as.character(substitute(data)), search())){
    detach(pos=which(search() %in% as.character(substitute(data))))
    #attach(dataset, name=as.character(substitute(data)), warn.conflicts = FALSE)
  }
}
NULL
