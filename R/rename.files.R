rename.files <-
function(dir, pattern, replacement){
  setwd(dir)
  .rename <- function(x) {
    file.rename(x, gsub(pattern, replacement, x))
  }
  files <- dir(full.names=T)
  lapply(files, .rename)
  message("Done!") 
}
