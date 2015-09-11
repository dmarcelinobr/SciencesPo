#' @title Computes the age
#' @description The `age` function computes the age given a bithdate (from) and elapsed time (to) variables.
#' @param from The first date (birthdate)
#' @param to The last date, usually the event date.
#'
#' @examples
#' birth = '1977-07-12'; today = '2015-07-12'
#' age(birth, today)
#' @export
`age` <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  age = to_lt$year - from_lt$year
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}
