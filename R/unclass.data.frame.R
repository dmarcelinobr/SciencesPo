#' Unclass data frame
#'
#' @param vars The variable(s).
#' @param data The data object.
#'
#' @export
unclass.data.frame <- function(vars, data = .data){
  data1 <- data
  nl <- as.list(1:ncol(data1))
  names(nl) <- names(data1)
  selected <- eval(substitute(vars), nl, parent.frame())
  for(i in selected){
    data1[,i] <- unclass(data1[,i])
    attributes(data1[, i]) <- NULL
  }
  assign(as.character(substitute(data)), data1, pos=1)
  if(is.element(as.character(substitute(data)), search())){
    detach(pos=which(search() %in% as.character(substitute(data))))
    attach(data1, name=as.character(substitute(data)), warn.conflicts = FALSE)
  }
}