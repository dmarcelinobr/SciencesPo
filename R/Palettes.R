globalVariables(c("k", "name"))

Palettes <- (function(filename){
  pals <- jsonlite::fromJSON(filename)
  for(p_name in names(pals)){
    pals[[p_name]]$name <- p_name
    pals[[p_name]]$size <- length(pals[[p_name]]$colors)
  }
  pals
})(system.file("extdata", "Palettes.json", package = "SciencesPo"))




#' Show details of specific palette
#'
#' @param palette.name the name of the palette
#'
#' @export
#' @examples
#' palette_info('colorblind')
palette_info <- function(palette.name){
  if(!palette.name %in% names(Palettes))
    stop(paste0("Requested palette '", palette.name, "' not found in SciencesPo library"))

  Palettes[[palette.name]]
}

#' List all available palettes
#'
#' @export
list_palettes <- function(){
  cat("Available SciencesPo palettes:\n")
  cat("----------------------------\n")
  for(p in names(Palettes)){
    cat(p, "\n")
  }

  invisible(Palettes)
}

#' Load a palette in hex definition
#'
#' See the available palettes using \code{list_palettes}.
#' \code{pal} will also search for the palette name in \code{RColorBrewer} and return it if found.
#'
#' @param palette.name the palette's name
#' @param n the number of colors to return, will default to the actual size of the palette, will recycle when necessary
#' @param evenly.spaced if \code{TRUE}, when the pallete has e.g. 5 colors but only 3 are requested, color 1, 3, and 5 will be returned.
#'
#' @return a character vector of hex colors
#' @export
`palette` <- function(palette.name, n=NULL, evenly.spaced=TRUE){
  if(palette.name %in% rownames(RColorBrewer::brewer.pal.info)){
    p <- RColorBrewer::brewer.pal(
      RColorBrewer::brewer.pal.info[rownames(RColorBrewer::brewer.pal.info) == palette.name, "maxcolors"],
      palette.name)

    return(p)
  }

  if(!palette.name %in% names(Palettes))
    stop(paste0("Requested palette '", palette.name, "' not found"))

  p<-Palettes[[palette.name]]$colors


  if(!is.null(n)){
    if(evenly.spaced & n < length(p)){
      p<-p[round(seq.int(1, length(p), length.out = n))]
    } else {
      p<-rep(p, length.out = n)
    }
  }

  p
}
NULL



#' Plot available palettes
#'
#' @return ggplot2 object
#' @export
`plot_palettes` <-function(){

  all_cols<-
    sapply(Palettes, function(p) {
      c(p$name, p$colors)
    })

  max_cols <- max(sapply(all_cols, function(p) length(p)))
  m <- matrix(nrow=NROW(all_cols), ncol=max_cols)

  for(i in 1:NROW(all_cols)){
    m[i,] <- c(all_cols[[i]], rep(NA, max_cols - (length(all_cols[[i]]))))
  }

  colnames(m) <-
    c("name", sapply(1:(NCOL(m) - 1), function(i) i))

  df<-
    tidyr::gather(
      as.data.frame(m, stringsAsFactors=FALSE),
      k, col, 2:NCOL(m))

  df<-
    dplyr::mutate(df, k = as.numeric(k))

  col <- df$col
  names(col) <- col


  ggplot(df) +
    aes(x=as.factor(k), y=name, fill=col) +
    geom_tile() +
    scale_fill_manual(values=col) +
    theme_bw() +
    labs(x="color",
         y="palette_name",
         title="Available palettes") +
    theme_base() +
    theme(legend.position="none")

}






#' SciencesPo colors and fills for ggplot2.
#'
#' The SciencesPo scales add colors that work nicely with `theme_flex()`.
#'
#' @details
#' \describe{
#'
#' \item{`scale_color_flex`}{
#' For use when `color` is specified as an `aes()` in a ggplot.}
#'
#' \item{`scale_fill_flex`}{
#' For use when `fill` is specified as an `aes()` in a ggplot.}
#' }
#'
#'
#' @inheritParams ggplot2::scale_color_manual
#'
#' @seealso [theme_flex()]
#'
#' @param theme one of "tableau20", "tableau10", "colorblind" etc. This should match the `theme_flex()` that is used with it.
#' @param ... common discrete scale parameters: `name`, `breaks`, `labels`, `na.value`, `limits` and `guide`. See [discrete_scale()] for more details
#'
#' @examples
#'
#' # Plot some variables
#' p <- ggplot(mtcars) +
#'      geom_point(aes(x = wt, y = mpg, color = factor(gear))) +
#'      facet_wrap(~am)
#'
#' # Plot with SciencesPo theme and colors
#' p +
#'     theme_flex() +
#'    scale_color_flex(theme = '538')
#'
#'
#' @name scale_manual
NULL




#' @rdname scale_manual
#' @export
#'
scale_color_flex <- function(..., theme = "tableau20") {
   if(!theme %in% names(Palettes))
  stop(paste0("Requested palette theme '", theme, "' not found in SciencesPo library"))
  ggplot2::scale_color_manual(values = palette(theme))
}
NULL



#' @rdname scale_manual
#' @export
scale_fill_flex <- function(..., theme = "tableau20") {
  if(!theme %in% names(Palettes))
    stop(paste0("Requested palette theme '", theme, "' not found in SciencesPo library"))
  ggplot2::scale_fill_manual(values = palette(theme))
}
NULL




#' @title Palette data for the themes used by package
#'
#' @description Data used by the palettes in the package.
#'
#' @format A \code{list}.
#'
`SciencesPo.Palettes` <- {
  x <- list()
  x$pub <- list()
  x$pub$colors <-
    list(
      default = c(
        rgb(0, 107, 164, maxColorValue = 255),
        rgb(255, 128, 14, maxColorValue = 255),
        rgb(89, 89, 89, maxColorValue = 255),
        rgb(48, 147, 67, maxColorValue = 255),
        rgb(102, 51, 204, maxColorValue = 255),
        rgb(95, 158, 209, maxColorValue = 255),
        rgb(200, 82, 0, maxColorValue = 255),
        rgb(171, 171, 171, maxColorValue = 255),
        rgb(105, 183, 100, maxColorValue = 255),
        rgb(255, 188, 121, maxColorValue = 255),
        rgb(137, 137, 137, maxColorValue = 255),
        rgb(162, 200, 236, maxColorValue = 255),
        rgb(59, 62, 172, maxColorValue = 255),
        rgb(170, 170, 17, maxColorValue = 255),
        rgb(85, 116, 166, maxColorValue = 255),
        rgb(207, 207, 207, maxColorValue = 255),
        rgb(204, 255, 51, maxColorValue = 255)
      ),
      gray = c(
        rgb(56, 56, 56, maxColorValue = 255),
        rgb(212, 218, 218, maxColorValue = 255),
        rgb(143, 135, 130, maxColorValue = 255),
        rgb(165, 172, 175, maxColorValue = 255),
        rgb(65, 68, 81, maxColorValue = 255),
        rgb(207, 207, 207, maxColorValue = 255),
        rgb(80, 100, 120, maxColorValue = 255),
        rgb(234, 234, 234, maxColorValue = 255),
        rgb(96, 99, 106, maxColorValue = 255)
      ),
      vape = c(
        rgb(6, 6, 6, maxColorValue = 255),
        rgb(212, 160, 32, maxColorValue = 255),
        rgb(21, 52, 78, maxColorValue = 255),
        rgb(121, 121, 121, maxColorValue = 255),
        rgb(170, 170, 170, maxColorValue = 255)
      ),
  carnival = c(
      rgb(142,255,0, maxColorValue = 255),
      rgb(0,215,249, maxColorValue = 255),
      rgb(241,0,255, maxColorValue = 255),
      rgb(249,55,0, maxColorValue = 255),
      rgb(255,170,20, maxColorValue = 255)
    ),
  seasons = c(
  autumn = rgb(153,0,51, maxColorValue = 255),
  spring = rgb(54,122,55, maxColorValue = 255),
  summer = rgb(204,102,0, maxColorValue = 255),
  winter = rgb(51,51,51, maxColorValue = 255)
  ),
  google = c(
      rgb(51, 102, 204, maxColorValue = 255),
      rgb(220, 57, 18, maxColorValue = 255),
      rgb(255, 153, 0, maxColorValue = 255),
      rgb(16, 150, 24, maxColorValue = 255),
      rgb(153, 0, 153, maxColorValue = 255),
      rgb(0, 153, 198, maxColorValue = 255),
      rgb(221, 68, 119, maxColorValue = 255),
      rgb(102, 170, 0, maxColorValue = 255),
      rgb(184, 46, 46, maxColorValue = 255),
      rgb(49, 99, 149, maxColorValue = 255),
      rgb(153, 68, 153, maxColorValue = 255),
      rgb(34, 170, 153, maxColorValue = 255),
      rgb(170, 170, 17, maxColorValue = 255),
      rgb(102, 51, 204, maxColorValue = 255),
      rgb(230, 115, 0, maxColorValue = 255),
      rgb(139, 7, 7, maxColorValue = 255),
      rgb(101, 16, 103, maxColorValue = 255),
      rgb(50, 146, 98, maxColorValue = 255),
      rgb(85, 116, 166, maxColorValue = 255),
      rgb(59, 62, 172, maxColorValue = 255)
      ),
      bluered12 = c(
        rgb(44,105,176, maxColorValue = 255),
        rgb(181,200,226, maxColorValue = 255),
        rgb(240,39,32, maxColorValue = 255),
        rgb(255,182,176, maxColorValue = 255),
        rgb(172,97,60, maxColorValue = 255),
        rgb(233,195,155, maxColorValue = 255),
        rgb(107,163,214, maxColorValue = 255),
        rgb(181,223,253, maxColorValue = 255),
        rgb(172,135,99, maxColorValue = 255),
        rgb(221,201,180, maxColorValue = 255),
        rgb(189,10,54, maxColorValue = 255),
        rgb(244,115,122, maxColorValue = 255)
      ),
      purplegray12 = c(
        rgb(123,102,210, maxColorValue = 255),
        rgb(166,153,232, maxColorValue = 255),
        rgb(220,95,189, maxColorValue = 255),
        rgb(255,192,218, maxColorValue = 255),
        rgb(95, 90, 65, maxColorValue = 255),
        rgb(220,95,189, maxColorValue = 255),
        rgb(180,177,155, maxColorValue = 255),
        rgb(153,86,136, maxColorValue = 255),
        rgb(216,152,186, maxColorValue = 255),
        rgb(171,106,213, maxColorValue = 255),
        rgb(208,152,238, maxColorValue = 255),
        rgb(139,124,110, maxColorValue = 255)
      ),
      greenorange12 = c(
        rgb(50,162,81, maxColorValue = 255),
        rgb(172,217,141, maxColorValue = 255),
        rgb(255,127,15, maxColorValue = 255),
        rgb(255,185,119, maxColorValue = 255),
        rgb(60,183,204, maxColorValue = 255),
        rgb(152,217,228, maxColorValue = 255),
        rgb(184,90,13, maxColorValue = 255),
        rgb(255,217,74, maxColorValue = 255),
        rgb(57,115,124, maxColorValue = 255),
        rgb(134,180,169, maxColorValue = 255),
        rgb(130,133,59, maxColorValue = 255),
        rgb(204,201,77, maxColorValue = 255)
      ),
      bivariate1 = c(
        rgb(100, 172,190, maxColorValue = 255),
        rgb(98, 127,140, maxColorValue = 255),
        rgb(87, 66,73, maxColorValue = 255),
        rgb(176, 213,223, maxColorValue = 255),
        rgb(173, 158,165, maxColorValue = 255),
        rgb(152, 83,86, maxColorValue = 255),
        rgb(232, 232,232, maxColorValue = 255),
        rgb(228, 172,172, maxColorValue = 255),
        rgb(200, 90,90, maxColorValue = 255)
      ),
      bivariate2 = c(
        rgb(190, 100, 172, maxColorValue = 255),
        rgb(140, 98, 170, maxColorValue = 255),
        rgb(59, 73, 148, maxColorValue = 255),
        rgb(223, 176, 214, maxColorValue = 255),
        rgb(165, 173, 211, maxColorValue = 255),
        rgb(86, 152, 185, maxColorValue = 255),
        rgb(232, 232, 232, maxColorValue = 255),
        rgb(172, 228, 228, maxColorValue = 255),
        rgb(90, 200, 200, maxColorValue = 255)
      ),
      bivariate3 = c(
        rgb(115,174,128, maxColorValue = 255),
        rgb(90,145,120, maxColorValue = 255),
        rgb(42,90,91, maxColorValue = 255),
        rgb(184,214,190, maxColorValue = 255),
        rgb(144,178,179, maxColorValue = 255),
        rgb(86,121,148, maxColorValue = 255),
        rgb(232,232,232, maxColorValue = 255),
        rgb(181,192,218, maxColorValue = 255),
        rgb(108,131,181, maxColorValue = 255)
      ),
      bivariate4 = c(
        rgb(153,114,175, maxColorValue = 255),
        rgb(151,107,130, maxColorValue = 255),
        rgb(128,77,54, maxColorValue = 255),
        rgb(203,184,215, maxColorValue = 255),
        rgb(200,173,160, maxColorValue = 255),
        rgb(175,142,83, maxColorValue = 255),
        rgb(232,232,232, maxColorValue = 255),
        rgb(228,217,172, maxColorValue = 255),
        rgb(200,179,90, maxColorValue = 255)
      )
    )
  x$pub$shapes <-
    list(
      gender = c(
        -0x2640L, # f
        -0x2642L  # m
      ),
      default = c(
        16L, # filled circle
        15L, # filled square
        18L, # filled diamond
        1L, # circle
        0L, # square
        5L, # diamond
          -0x25bc, # black down-pointing triangle
          -0x25b2, # black up-pointing triangle
          -0x25b6, # black right-pointing triangle
          -0x25c0, # black left-pointing triangle
          -0x29d3, # black bowtie
          -0x29d7, # black hourglass
          19L, # circle
         -0x2726, # black four pointed star
          4L, # x (0xd7)
          3L, # plus (0x2b)
          -0x2217, # asterisk operator
          -0x2796, # heavy minus sign
          -0x2759 # medium vertical bar
        ),
      proportions = c(
        -0x25CBL, # White circle
        -0x25DL, # with upper right quadrant black
        -0x25D1L, # with right half black
        -0x25D5L, # # with upper left quadrant black
        -0x25CFL # Black circle
      ),
      arrows = c(
        -0x2193L, # downwards
        -0x2198L, # southeast
        -0x2192L, # rightwards
        -0x2197L, # northeast
        -0x2191L, # upwards
        -0x2196L, # north west
        -0x2190L, # leftwards
        -0x2199L # south west
        )
    )
 x$party <- list()
 x$party$BRA <- c(
    DEM = rgb(0,0,255, maxColorValue = 255),
    PCdoB = rgb(255, 48, 48, maxColorValue = 255),
    PCB = rgb(205, 38, 38, maxColorValue = 255),
    PCO = rgb(139, 0, 0, maxColorValue = 255),
    PDT = rgb(16, 78, 139, maxColorValue = 255),
    PEN = rgb(34, 139, 34, maxColorValue = 255),
    PHS = rgb(30,31,123, maxColorValue = 255),
    PMDB = rgb(255,255,0, maxColorValue = 255),
    PMB = rgb(205,0,12, maxColorValue = 255),
    PMN = rgb(255,0,0, maxColorValue = 255),
    NOVO = rgb(255, 140, 0, maxColorValue = 255),
    PP = rgb(65, 105, 225, maxColorValue = 255),
    PPL = rgb(0,99,47, maxColorValue = 255),
    PPS = rgb(255,0,0, maxColorValue = 255),
    PR = rgb(25, 25, 112, maxColorValue = 255),
    PRB = rgb(34, 139, 34, maxColorValue = 255),
    PROS = rgb(255, 165, 0, maxColorValue = 255),
    PRP = rgb(0,91,171, maxColorValue = 255),
    PRTB = rgb(44,171,63, maxColorValue = 255),
    PSB = rgb(255,44,44, maxColorValue = 255),
    PSC = rgb(44,190,63, maxColorValue = 255),
    PSD = rgb(2, 63, 136, maxColorValue = 255),
    PSDB = rgb(0,143,213, maxColorValue = 255),
    PSDC = rgb(30,31,123, maxColorValue = 255),
    PSL = rgb(65, 68, 81, maxColorValue = 255),
    PSOL = rgb(238, 0, 0, maxColorValue = 255),
    PSTU = rgb(205, 0, 0, maxColorValue = 255),
    PT = rgb(255,39,0, maxColorValue = 255),
    PTB = rgb(10, 10,10, maxColorValue = 255),
    PTC = rgb(72, 118, 255, maxColorValue = 255),
    PTdoB = rgb(0,255,0, maxColorValue = 255),
    PTN = rgb(255,255,0, maxColorValue = 255),
    PV = rgb(119,171,67, maxColorValue = 255),
    REDE = rgb(0,191,255, maxColorValue = 255),

    SD = rgb(255,165,0, maxColorValue = 255),
    SDD = rgb(65, 68, 81, maxColorValue = 255),
    PFL = rgb(0,0,255, maxColorValue = 255) )
  x$party$ARG <- c(
    PJ = rgb(0,191,255, maxColorValue = 255),
    UCR = rgb(255, 0, 0, maxColorValue = 255),
    FG = rgb(238,0,0, maxColorValue = 255),
    PS = rgb(205,0,0, maxColorValue = 255),
    PRO = rgb(255, 215, 0, maxColorValue = 255),
    UCeDe = rgb(72,118,255, maxColorValue = 255),
    PI = rgb(15,15,15, maxColorValue = 255),
    ARI = rgb(0, 104, 139, maxColorValue = 255),
    PDC = rgb(25,25,112, maxColorValue = 255),
    PO = rgb(139,0,0, maxColorValue = 255),
    PCR = rgb(238, 44, 44, maxColorValue = 255)
  )
  x$party$CAN <- c(
    Lib = rgb(255,0,0, maxColorValue = 255),
    Con = rgb(65,105,225, maxColorValue = 255),
    NPD = rgb(255, 140, 0, maxColorValue = 255),
    Bloc = rgb(0,191,255, maxColorValue = 255),
    Green = rgb(0,255,0, maxColorValue = 255)
  )
  x$party$USA <- c(
    Republican = rgb(255, 39, 0, maxColorValue = 255),
    Democratic = rgb(0, 143, 213, maxColorValue = 255),
    Independent = rgb(119, 171, 67, maxColorValue = 255)
  )
  x$continents <- c(
    Africa = rgb(127,59,8, maxColorValue = 255), # 52
    Americas = rgb(165,0,38, maxColorValue = 255), # 25
    Asia = rgb(64,0,75, maxColorValue = 255), # 33
    Europe = rgb(39,100,25, maxColorValue = 255), # 30
    Oceania = rgb(49,54,149, maxColorValue = 255) # 2
  )
  x$regions <- c(
    NO = rgb(16, 78, 139, maxColorValue = 255),
    NE = rgb(110, 139, 61, maxColorValue = 255),
    CO = rgb(154, 50, 205, maxColorValue = 255),
    SE = rgb(255, 193, 37, maxColorValue = 255),
    SU = rgb(255, 193, 37, maxColorValue = 255)
  )
  x$countries <- c()
  ## return
  x
}
NULL


#' @title Political Parties Color Palette (Discrete) and Scales
#' @description An N-color discrete palette for political parties.
#' @inheritParams ggplot2::scale_colour_hue
#' @param palette the palette name, a character string.
#' @param plot logical, if \code{TRUE} a plot is returned.
#' @param hex logical, if \code{FALSE}, the associated color name (label) is returned.
#' @family color party
#' @keywords ggplot2
#' @examples
#' library(scales)
#'
#' # Argentine
#' party_pal("ARG", plot=TRUE, hex=FALSE)
#'
#' show_col(party_pal("ARG")(12))
#'
#' # Brazil
#' party_pal("BRA", plot=TRUE, hex=FALSE)
#'
#' show_col(party_pal("BRA")(20))
#'
#' # Canada
#' party_pal("CAN", plot=TRUE, hex=FALSE)
#'
#' show_col(party_pal("CAN")(10))
#'
#' # US
#' party_pal("USA", plot=TRUE, hex=FALSE)
#'
#' show_col(party_pal("USA")(6))
#'
#' @export
`party_pal` <- function(palette = "BRA", plot=FALSE, hex=FALSE) {
  pal.list <- SciencesPo.Palettes$party
  if (!palette %in% c(names(pal.list), "BRA", "ARG", "CAN", "USA")) {
    stop(sprintf("%s is not a valid palette name", palette))
  }
  if (palette =="BRA") {
    types <- pal.list[["BRA"]][seq(1, 30, by = 1)]
  } else if (palette == "ARG") {
    types <- pal.list[["ARG"]][seq(1, 20, by = 1)]
  } else {
    types <- pal.list[[palette]]
  }

  if (plot){
  # hexs
  colors <- unname(types)[seq_len(length(types))]
  # names
  color_names <- names(types)[seq_len(length(types))]

  ## functions internal to scales::show_col()
  n <- length(colors)
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n/ncol)
  colors <- c(colors, rep(NA, nrow * ncol - length(colors)))
  colors <- matrix(colors, ncol = ncol, byrow = FALSE)
  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colors))
  graphics::plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  graphics::rect(col(colors) - 1, -row(colors) + 1, col(colors), -row(colors), col = colors)
  # add conditional display of hex codes or names
  if (hex) {
    text(col(colors) - 0.5, -row(colors) + 0.5, colors)
  } else {
    text(col(colors) - 0.5, -row(colors) + 0.5, color_names)
  }
}
  else {
    function(n) {
    unname(types)[seq_len(n)]
      }
  }
}
NULL



#' @title Political Parties Color Palette (Discrete) and Scales
#' @description An N-color discrete palette for political parties.
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams party_pal
#' @family color party
#' @param palette the palette name, a character string.
#' @seealso \code{\link{party_pal}} for details and references.
#' @keywords ggplot2
#' @export
#' @rdname color_party
`scale_color_party` <- function(palette = "BRA", ...) {
  if (requireNamespace("scales")) {
   discrete_scale("color", "party", party_pal(palette), ...)
  } else {
    discrete_scale("color", "party", party_pal(palette), ...)
  }
}
NULL




#' @title Political Parties Color Palette (Discrete) and Scales
#' @description An N-color discrete palette for political parties.
#' @inheritParams ggplot2::scale_fill_hue
#' @inheritParams party_pal
#' @param palette the palette name, a character string.
#' @seealso \code{\link{party_pal}} for details and references.
#' @keywords ggplot2
#' @export
#' @rdname fill_party
`scale_fill_party` <- function(palette = "BRA", ...) {
  if (requireNamespace("scales")) {
   discrete_scale("fill", "party", party_pal(palette), ...)
  } else {
    discrete_scale("fill", "party", party_pal(palette), ...)
  }
}
NULL




#' @title Shape scales for theme_pub (discrete)
#'
#' @description Discrete shape scales for \code{theme_pub()}.
#' @param palette the palette name, a character string.
#' @family shape pub
#' @keywords ggplot2
#' @export
#'
`pub_shapes` <- function(palette = "default") {
  if (requireNamespace("scales")) {
  scales::manual_pal(unname(SciencesPo.Palettes$pub$shapes[[palette]]))
  } else {
    scales::manual_pal(unname(SciencesPo.Palettes$pub$shapes[[palette]]))
  }
}
NULL



#' @title Shape scales for theme_pub (discrete)
#'
#' @description See \code{\link{pub_shapes}} for details.
#'
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritParams pub_shapes
#' @param palette the palette name, a character string.
#' @keywords ggplot2
#' @family shape pub
#' @examples
#' library("ggplot2")
#' library("scales")
#'
#' p <- ggplot(mtcars) +
#'      geom_point(aes(x = wt, y = mpg, shape = factor(gear))) +
#'      facet_wrap(~am)
#' p + scale_shape_pub()
#'
#' @export
#' @rdname shape_pub
`scale_shape_pub` <- function(palette = "default", ...) {
  if (requireNamespace("scales")) {
   discrete_scale("shape", "pub", pub_shapes(palette), ...)
  } else {
 discrete_scale("shape", "pub", pub_shapes(palette), ...)
  }
}
NULL


