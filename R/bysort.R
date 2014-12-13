bysort <-
function(data, vars=NULL, var.order=names(data)) {
  # Sorts a data.frame by columns or rows or both.
  # Can refer to variables either by names or number.
  # If referring to variable by number, and sorting both the order
  # of variables and the sorting within variables, refer to the
  # variable numbers of the final data.frame
  #
  # === EXAMPLES ===
  #
  #    library(foreign)
  #    kidshtwt = read.dta("http://www.ats.ucla.edu/stat/stata/modules/kidshtwt.dta")
  #    sorter(kidshtwt, var.order = c("fam", "bir", "wt", "ht"))
  #    sorter(kidshtwt, var.order = c("fam", "bir", "wt", "ht"),
  #              vars = c("birth", "famid")) # USE FULL NAMES HERE
  #    sorter(kidshtwt, var.order = c(1:4), # DROP THE WT COLUMNS
  #              vars = 3) # SORT BY HT1
  # include a var that takes the order automaticaly
  
  if (is.numeric(var.order) == 1) {
    var.order = colnames(data)[var.order]
  } else if (is.numeric(var.order) == 0) {
    var.order = var.order
  }
  
  a = names(data)
  b = length(var.order)
  subs = vector("list", b)
  
  for (i in 1:b) {
    subs[[i]] = sort(grep(var.order[i], a, value=T))
  }
  x = unlist(subs)
  
  y = data[ , x ]
  
  if (is.null(vars)) {
    y
  } else if (is.numeric(vars) == 1) {
    vars = colnames(y)[vars]
    y[do.call(order, y[vars]), ]
  } else if (is.numeric(vars) == 0) {
    vars = vars
    y[do.call(order, y[vars]), ]
  }
}
