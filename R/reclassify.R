reclassify <-
function(data, classes) {
  cols <- rep(classes, len=length(data))
  data[] <- lapply(seq_along(data), function(i) switch(cols[i], 
      numeric=as.numeric(data[[i]]), 
      character=as.character(data[[i]]), 
      Date=as.Date(data[[i]], origin='1970-01-01'), 
      POSIXct=as.POSIXct(data[[i]], origin='1970-01-01'), 
      factor=as.factor(data[[i]]),
      as(data[[i]], cols[i]) ))
  data
}
