data.clean <-
function(data)
 {
data <- gsub('^[^a-zA-Z0-9]+', '', data, perl = TRUE)
data <- gsub('[^a-zA-Z0-9]+$', '', data, perl = TRUE)
data <- gsub('_+', '.', data, perl = TRUE)
data <- gsub('-+', '.', data, perl = TRUE)
data <- gsub('\\s+', '.', data, perl = TRUE)
data <- gsub('\\.+', '.', data, perl = TRUE)
data <- make.names(data)
return(data)
}
