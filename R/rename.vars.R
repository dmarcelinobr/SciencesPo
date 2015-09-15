#' @title Rename variables in a data.frame
#' @description Rename variables in a data.frame.
#' @param .data dataframe to be modified.
#' @param from character vector containing the current name of each variable to be renamed.
#' @param to character vector containing the new name of each variable to be renamed.
#' @param names character vector containing the names of variables to be removed.
#' @param info boolean value indicating whether to print details of the renaming.  Defaults to TRUE.
#' @export
#' @examples
#' rename.vars(twins, "C", "D")
rename.vars <- function(.data, from='', to='', info=TRUE) {

   dsn <- deparse(substitute(.data))
   dfn <- names(.data)

   if ( length(from) != length(to)) {
     cat('--------- from and to not same length ---------\n')
     stop()
   }

   if (length(dfn) < length(to)) {
     cat('--------- too many new names ---------\n')
     stop()
   }

   chng <- match(from,dfn)

   frm.in <- from %in% dfn
   if (!all(frm.in) ) {
     cat('---------- some of the from names not found in',dsn,'\n')
     stop()
   }

   if (length(to) != length(unique(to))) {
     cat('---------- New names not unique\n')
     stop()
   }

   dfn.new <- dfn
   dfn.new[chng] <- to
   if (info) cat('\nChanging in',dsn)
   tmp <- rbind(from,to)
   dimnames(tmp)[[1]] <- c('From:','To:')
   dimnames(tmp)[[2]] <- rep('',length(from))
   if (info)
     {
       print(tmp,quote=FALSE)
       cat("\n")
     }
   names(.data) <- dfn.new
   .data
}
NULL
