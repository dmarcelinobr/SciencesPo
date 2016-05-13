#' @encoding UTF-8
#' @title Unit Converter
#'
#' @description Converts a numeric value from one measurement system to another. For instance, distances in miles to kilometers.
#'
#' @param x A numeric value or a vector of values.
#' @param from A character name defining the original unit.
#' @param to A character name defining the target unit.
#'
#' @details \code{NA} is returned if a conversion cannot be found.
#'
#' \tabular{lll}{
#'  \cr\cr
#'  \bold{Weight and mass}\tab \tab\cr
#'  Gram  \tab g \tab metric\cr
#'  Slug  \tab sg \cr
#'  Pound mass (avoirdupois)  \tab lbm \cr
#'  U (atomic mass unit)  \tab u \cr
#'  Ounce mass (avoirdupois)  \tab ozm \cr
#'  \cr\bold{Distance}\tab \cr
#'  Meter  \tab  m \tab metric \cr
#'  Statute mile  \tab mi \cr
#'  Nautical mile  \tab Nmi \cr
#'  Inch  \tab in \cr
#'  Foot  \tab ft \cr
#'  Yard  \tab yd \cr
#'  Angstrom  \tab ang \tab metric \cr
#'  Pica  \tab pica \cr
#'  \tab  \cr
#'  \cr\bold{Time}\tab \cr
#'  Year  \tab yr \cr
#'  Day  \tab day \cr
#'  Hour  \tab hr \cr
#'  Minute  \tab mn \cr
#'  Second  \tab sec \cr
#'  \cr\bold{Pressure}\tab \cr
#'  Pascal  \tab Pa (or p) \cr
#'  Atmosphere  \tab atm (or at) \cr
#'  mm of Mercury  \tab mmHg \cr
#'  \tab  \cr
#'  \cr\bold{Force}\tab \cr
#'  Newton  \tab N \tab metric \cr
#'  Dyne  \tab dyn (or dy) \cr
#'  Pound force  \tab lbf \cr
#'  \cr\bold{Energy}\tab \cr
#'  Joule  \tab J \tab metric \cr
#'  Erg  \tab e \cr
#'  Thermodynamic calorie  \tab c \cr
#'  IT calorie  \tab cal \tab metric \cr
#'  Electron volt  \tab eV (or ev) \tab metric \cr
#'  Horsepower-hour  \tab HPh (or hh) \cr
#'  Watt-hour  \tab Wh (or wh) \tab metric \cr
#'  Foot-pound  \tab flb \cr
#'  BTU  \tab BTU (or btu) \cr
#'  \tab  \cr
#'  \cr\bold{Power}\tab \cr
#'  Horsepower  \tab HP (or h) \cr
#'  Watt  \tab W (or w) \tab metric \cr
#'  \cr\bold{Magnetism}\tab \cr
#'  Tesla  \tab T \tab metric \cr
#'  Gauss  \tab ga \tab metric \cr
#'  \tab  \cr
#'  \cr\bold{Temperature}\tab \cr
#'  Degree Celsius  \tab C (or cel) \cr
#'  Degree Fahrenheit  \tab F (or fah) \cr
#'  Kelvin  \tab K (or kel) \tab metric \cr
#'  \cr\bold{Liquid measure}\tab \cr
#'  Teaspoon  \tab tsp \cr
#'  Tablespoon  \tab tbs \cr
#'  Fluid ounce  \tab oz \cr
#'  Cup  \tab cup \cr
#'  U.S. pint  \tab pt (or us_pt) \cr
#'  U.K. pint  \tab uk_pt \cr
#'  Quart  \tab qt \cr
#'  Gallon  \tab gal \cr
#'  Liter  \tab l (or lt) \tab metric \cr
#'}
#' @examples
#' scpo.units(c(5.6, 6.7), "in", "m")
#'
#' @export
`scpo.units` <- function(x, from, to) {
  if (from == "C") {
    if (to == "F")
      return(x * 1.8 + 32)
  }
  if (from == "F") {
    if (to == "C")
      return((x - 32) * 5 / 9)
  }

  factor <- Units[Units$from == from & Units$to == to, "factor"]
  if (length(factor) == 0)
    factor <- NA
  ans <- (x * factor)
  return(ans)

}
NULL




#' Bytes formatter: convert to byte measurement and display symbol.
#'
#' @return a function with three parameters, \code{x}, a numeric vector that
#'   returns a character vector, \code{symbol} the byte symbol (e.g. "\code{Kb}")
#'   desired and the measurement \code{units} (traditional \code{binary} or
#'   \code{si} for ISI metric units).
#' @param x a numeric vector to format
#' @param symbol byte symbol to use. If "\code{auto}" the symbol used will be
#'   determined by the maximum value of \code{x}. Valid symbols are
#'   "\code{b}", "\code{K}", "\code{Mb}", "\code{Gb}", "\code{Tb}", "\code{Pb}",
#'   "\code{Eb}", "\code{Zb}", and "\code{Yb}", along with their upper case
#'   equivalents and "\code{iB}" equivalents.
#' @param units which unit base to use, "\code{binary}" (1024 base) or
#'   "\code{si}" (1000 base) for ISI units.
#' @references Units of Information (Wikipedia) :
#'   \url{http://en.wikipedia.org/wiki/Units_of_information}
#' @export
#' @examples
#' byte_format()(sample(3000000000, 10))
#' bytes(sample(3000000000, 10))
byte_format <- function(symbol="auto", units="binary") {
  function(x) bytes(x, symbol, units)
}

#' @export
#' @rdname byte_format
Kb <- byte_format("Kb", "binary")

#' @export
#' @rdname byte_format
Mb <- byte_format("Mb", "binary")

#' @export
#' @rdname byte_format
Gb <- byte_format("Gb", "binary")

#' @export
#' @rdname byte_format
bytes <- function(x, symbol="auto", units=c("binary", "si")) {
  
  symbol <- match.arg(symbol, c("auto",
                                "b",  "Kb",  "Mb",  "Gb",  "Tb",  "Pb",  "Eb",  "Zb",  "Yb",
                                "B",  "KB",  "MB",  "GB",  "TB",  "PB",  "EB",  "ZB",  "YB",
                                "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))
  
  units <- match.arg(units, c("binary", "si"))
  
  base <- switch(units, `binary`=1024, `si`=1000)
  
  if (symbol == "auto") {
    symbol <-
      if      (max(x) >= (base^5)) { "Pb" }
    else if (max(x) >= (base^4)) { "Tb" }
    else if (max(x) >= (base^3)) { "Gb" }
    else if (max(x) >= (base^2)) { "Kb" }
    else if (max(x) >= (base^1)) { "Mb" }
    else                         {  "b" }
  }
  
  switch(symbol,
         "b" =,  "B"  = paste(x,                  "bytes"),
         
         "Kb" =, "KB" = paste(round(x/(base^1), 1L), "Kb"),
         "Mb" =, "MB" = paste(round(x/(base^2), 1L), "Mb"),
         "Gb" =, "GB" = paste(round(x/(base^3), 1L), "Gb"),
         "Tb" =, "TB" = paste(round(x/(base^4), 1L), "Tb"),
         "Pb" =, "PB" = paste(round(x/(base^5), 1L), "Pb"),
         "Eb" =, "EB" = paste(round(x/(base^6), 1L), "Eb"),
         "Zb" =, "ZB" = paste(round(x/(base^7), 1L), "Zb"),
         "Yb" =, "YB" = paste(round(x/(base^8), 1L), "Yb"),
         
         "KiB"        = paste(round(x/(base^1), 1L), "KiB"),
         "MiB"        = paste(round(x/(base^2), 1L), "MiB"),
         "GiB"        = paste(round(x/(base^3), 1L), "GiB"),
         "TiB"        = paste(round(x/(base^4), 1L), "TiB"),
         "PiB"        = paste(round(x/(base^5), 1L), "PiB"),
         "EiB"        = paste(round(x/(base^6), 1L), "EiB"),
         "ZiB"        = paste(round(x/(base^7), 1L), "ZiB"),
         "YiB"        = paste(round(x/(base^8), 1L), "YiB")
  )
  
}
NULL