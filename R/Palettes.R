#' @title Palette data for the themes used by package
#'
#' @description Data used by the palettes in the package.
#'
#' @format A \code{list}.
#'
Palettes <- {
  x <- list()
  x$pub <- list()
  x$pub$colors <-
    list(
      pub12 = c(
        rgb(0, 107, 164, max = 255),
        rgb(255, 128, 14, max = 255),
        rgb(171, 171, 171, max = 255),
        rgb(89, 89, 89, max = 255),
        rgb(95, 158, 209, max = 255),
        rgb(200, 82, 0, max = 255),
        rgb(137, 137, 137, max = 255),
        rgb(162, 200, 236, max = 255),
        rgb(255, 188, 121, max = 255),
        rgb(207, 207, 207, max = 255),
        rgb(48, 147, 67, max = 255),
        rgb(105, 183, 100, max = 255)
      ),
      gray5 = c(
        rgb(96, 99, 106, max = 255),
        rgb(165, 172, 175, max = 255),
        rgb(65, 68, 81, max = 255),
        rgb(143, 135, 130, max = 255),
        rgb(207, 207, 207, max = 255)
      ),
      chalk = c(
        rgb(255, 255, 255, max = 255),
        rgb(194, 197, 190, max = 255),
        rgb(212, 218, 218, max = 255),
        rgb(17, 17, 17, max = 255),
        rgb(109, 136, 117, max = 255),
        rgb(234, 234, 234, max = 255),
        rgb(144, 138, 120, max = 255)
      ),
      tableau20 = c(
        rgb(31, 119, 180, max = 255),
        rgb(174, 199, 232, max = 255),
        rgb(255, 127, 14, max = 255),
        rgb(255, 187, 120, max = 255),
        rgb(44, 160, 44, max = 255),
        rgb(152, 223, 138, max = 255),
        rgb(214, 39, 40, max = 255),
        rgb(255, 152, 150, max = 255),
        rgb(148, 103, 189, max = 255),
        rgb(197, 176, 213, max = 255),
        rgb(140, 86, 75, max = 255),
        rgb(196, 156, 148, max = 255),
        rgb(227, 119, 194, max = 255),
        rgb(247, 182, 210, max = 255),
        rgb(127, 127, 127, max = 255),
        rgb(199, 199, 199, max = 255),
        rgb(188, 189, 34, max = 255),
        rgb(219, 219, 141, max = 255),
        rgb(23, 190, 207, max = 255),
        rgb(158, 218, 229, max = 255),
        rgb(172, 135, 99, max = 255)
      ),
      tableau10medium = c(
        rgb(114, 158, 206, max = 255),
        rgb(255, 158, 74, max = 255),
        rgb(103, 191, 92, max = 255),
        rgb(237, 102, 93, max = 255),
        rgb(173, 139, 201, max = 255),
        rgb(168, 120, 110, max = 255),
        rgb(237, 151, 202, max = 255),
        rgb(162, 162, 162, max = 255),
        rgb(205, 204, 93, max = 255),
        rgb(109, 204, 218, max = 255)
      ),
    fte = c(
        rgb(255, 39, 0, max = 255),
        rgb(0, 143, 213, max = 255),
        rgb(119, 171, 67, max = 255),
        rgb(60, 60, 60, max = 255),
        rgb(210, 210, 210, max = 255),
        rgb(240, 240, 240, max = 255)
      ),
  carnival = c(
      rgb(142,255,0, max = 255),
      rgb(0,215,249, max = 255),
      rgb(241,0,255, max = 255),
      rgb(249,55,0, max = 255),
      rgb(255,170,20, max = 255)
    ),
  seasons = c(
  autumn = rgb(153,0,51, max = 255),
  spring = rgb(54,122,55, max = 255),
  summer = rgb(204,102,0, max = 255),
  winter = rgb(51,51,51, max = 255)
  ),
  gdocs = c(
      rgb(51, 102, 204, max = 255),
      rgb(220, 57, 18, max = 255),
      rgb(255, 153, 0, max = 255),
      rgb(16, 150, 24, max = 255),
      rgb(153, 0, 153, max = 255),
      rgb(0, 153, 198, max = 255),
      rgb(221, 68, 119, max = 255),
      rgb(102, 170, 0, max = 255),
      rgb(184, 46, 46, max = 255),
      rgb(49, 99, 149, max = 255),
      rgb(153, 68, 153, max = 255),
      rgb(34, 170, 153, max = 255),
      rgb(170, 170, 17, max = 255),
      rgb(102, 51, 204, max = 255),
      rgb(230, 115, 0, max = 255),
      rgb(139, 7, 7, max = 255),
      rgb(101, 16, 103, max = 255),
      rgb(50, 146, 98, max = 255),
      rgb(85, 116, 166, max = 255),
      rgb(59, 62, 172, max = 255)
      ),
  manyeyes = c(
    rgb(156, 158, 222, max = 255),
    rgb(115, 117, 181, max = 255),
    rgb(74, 85, 132, max = 255),
    rgb(206, 219, 156, max = 255),
    rgb(181, 207, 107,max = 255),
    rgb(140, 162, 82, max = 255),
    rgb(99, 121, 57,max = 255),
    rgb(231, 203, 148, max = 255),
    rgb(231, 186, 82, max = 255),
    rgb(189, 158, 57, max = 255),
    rgb(140, 109, 49, max = 255),
    rgb(231, 150, 156, max = 255),
    rgb(214, 97, 107, max = 255),
    rgb(173, 73, 74, max = 255),
    rgb(132, 60, 57, max = 255),
    rgb(222, 158, 214, max = 255),
    rgb(206, 109, 189, max = 255),
    rgb(165, 81, 148, max = 255),
    rgb(123, 65, 115, max = 255)
  ),
    trafficlight = c(
        rgb(177,3,24, max = 255),
        rgb(219,161,58, max = 255),
        rgb(48,147,67, max = 255),
        rgb(216,37,38, max = 255),
        rgb(255,193,86, max = 255),
        rgb(105,183,100, max = 255),
        rgb(242,108,100, max = 255),
        rgb(255,221,113, max = 255),
        rgb(159,205,153, max = 255)
      ),
      bluered12 = c(
        rgb(44,105,176, max = 255),
        rgb(181,200,226, max = 255),
        rgb(240,39,32, max = 255),
        rgb(255,182,176, max = 255),
        rgb(172,97,60, max = 255),
        rgb(233,195,155, max = 255),
        rgb(107,163,214, max = 255),
        rgb(181,223,253, max = 255),
        rgb(172,135,99, max = 255),
        rgb(221,201,180, max = 255),
        rgb(189,10,54, max = 255),
        rgb(244,115,122, max = 255)
      ),
      purplegray12 = c(
        rgb(123,102,210, max = 255),
        rgb(166,153,232, max = 255),
        rgb(220,95,189, max = 255),
        rgb(255,192,218, max = 255),
        rgb(95, 90, 65, max = 255),
        rgb(220,95,189, max = 255),
        rgb(180,177,155, max = 255),
        rgb(153,86,136, max = 255),
        rgb(216,152,186, max = 255),
        rgb(171,106,213, max = 255),
        rgb(208,152,238, max = 255),
        rgb(139,124,110, max = 255)
      ),
      greenorange12 = c(
        rgb(50,162,81, max = 255),
        rgb(172,217,141, max = 255),
        rgb(255,127,15, max = 255),
        rgb(255,185,119, max = 255),
        rgb(60,183,204, max = 255),
        rgb(152,217,228, max = 255),
        rgb(184,90,13, max = 255),
        rgb(255,217,74, max = 255),
        rgb(57,115,124, max = 255),
        rgb(134,180,169, max = 255),
        rgb(130,133,59, max = 255),
        rgb(204,201,77, max = 255)
      ),
      cyclic = c(
        rgb(31,131,180, max = 255),
        rgb(22,150,172, max = 255),
        rgb(24,161,136, max = 255),
        rgb(41,160,60, max = 255),
        rgb(84,163,56, max = 255),
        rgb(130,169,63, max = 255),
        rgb(173,184,40, max = 255),
        rgb(216,189,53, max = 255),
        rgb(255,189,76, max = 255),
        rgb(255,176,3, max = 255),
        rgb(255,156,14, max = 255),
        rgb(255,129,14, max = 255),
        rgb(231,87,39, max = 255),
        rgb(210,62,78, max = 255),
        rgb(201,77,140, max = 255),
        rgb(192,74,167, max = 255),
        rgb(180,70,179, max = 255),
        rgb(150,88,177, max = 255),
        rgb(128,97,180, max = 255),
        rgb(111,99,187, max = 255)
      ),
      bivariate1 = c(
        rgb(100, 172,190, max = 255),
        rgb(98, 127,140, max = 255),
        rgb(87, 66,73, max = 255),
        rgb(176, 213,223, max = 255),
        rgb(173, 158,165, max = 255),
        rgb(152, 83,86, max = 255),
        rgb(232, 232,232, max = 255),
        rgb(228, 172,172, max = 255),
        rgb(200, 90,90, max = 255)
      ),
      bivariate2 = c(
        rgb(190, 100, 172, max = 255),
        rgb(140, 98, 170, max = 255),
        rgb(59, 73, 148, max = 255),
        rgb(223, 176, 214, max = 255),
        rgb(165, 173, 211, max = 255),
        rgb(86, 152, 185, max = 255),
        rgb(232, 232, 232, max = 255),
        rgb(172, 228, 228, max = 255),
        rgb(90, 200, 200, max = 255)
      ),
      bivariate3 = c(
        rgb(115,174,128, max = 255),
        rgb(90,145,120, max = 255),
        rgb(42,90,91, max = 255),
        rgb(184,214,190, max = 255),
        rgb(144,178,179, max = 255),
        rgb(86,121,148, max = 255),
        rgb(232,232,232, max = 255),
        rgb(181,192,218, max = 255),
        rgb(108,131,181, max = 255)
      ),
      bivariate4 = c(
        rgb(153,114,175, max = 255),
        rgb(151,107,130, max = 255),
        rgb(128,77,54, max = 255),
        rgb(203,184,215, max = 255),
        rgb(200,173,160, max = 255),
        rgb(175,142,83, max = 255),
        rgb(232,232,232, max = 255),
        rgb(228,217,172, max = 255),
        rgb(200,179,90, max = 255)
      )
    )

  x$pub$shapes <-
    list(
      gender = c(
        -0x2640L, # f
        -0x2642L  # m
      ),
      pub = c(
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
    DEM = rgb(0,0,255, max = 255),
    PCdoB = rgb(255, 48, 48, max = 255),
    PCB = rgb(205, 38, 38, max = 255),
    PCO = rgb(139, 0, 0, max = 255),
    PDT = rgb(16, 78, 139, max = 255),
    PEN = rgb(34, 139, 34, max = 255),
    PHS = rgb(30,31,123, max = 255),
    PMDB = rgb(255,255,0, max = 255),
    PMB = rgb(205,0,12, max = 255),
    PMN = rgb(255,0,0, max = 255),
    NOVO = rgb(255, 140, 0, max = 255),
    PP = rgb(65, 105, 225, max = 255),
    PPL = rgb(0,99,47, max = 255),
    PPS = rgb(255,0,0, max = 255),
    PR = rgb(25, 25, 112, max = 255),
    PRB = rgb(34, 139, 34, max = 255),
    PROS = rgb(255, 165, 0, max = 255),
    PRP = rgb(0,91,171, max = 255),
    PRTB = rgb(44,171,63, max = 255),
    PSB = rgb(255,44,44, max = 255),
    PSC = rgb(44,190,63, max = 255),
    PSD = rgb(2, 63, 136, max = 255),
    PSDB = rgb(0,143,213, max = 255),
    PSDC = rgb(30,31,123, max = 255),
    PSL = rgb(65, 68, 81, max = 255),
    PSOL = rgb(238, 0, 0, max = 255),
    PSTU = rgb(205, 0, 0, max = 255),
    PT = rgb(255,39,0, max = 255),
    PTB = rgb(10, 10,10, max = 255),
    PTC = rgb(72, 118, 255, max = 255),
    PTdoB = rgb(0,255,0, max = 255),
    PTN = rgb(255,255,0, max = 255),
    PV = rgb(119,171,67, max = 255),
    REDE = rgb(0,191,255, max = 255),

    SD = rgb(255,165,0, max = 255),
    SDD = rgb(65, 68, 81, max = 255),
    PFL = rgb(0,0,255, max = 255)
  )
  x$party$ARG <- c(
    PJ = rgb(0,191,255, max = 255),
    UCR = rgb(255, 0, 0, max = 255),
    FG = rgb(238,0,0, max = 255),
    PS = rgb(205,0,0, max = 255),
    PRO = rgb(255, 215, 0, max = 255),
    UCeDe = rgb(72,118,255, max = 255),
    PI = rgb(15,15,15, max = 255),
    ARI = rgb(0, 104, 139, max = 255),
    PDC = rgb(25,25,112, max = 255),
    PO = rgb(139,0,0, max = 255),
    PCR = rgb(238, 44, 44, max = 255)
  )

  x$party$CAN <- c(
    Lib = rgb(255,0,0, max = 255),
    Con = rgb(65,105,225, max = 255),
    NPD = rgb(255, 140, 0, max = 255),
    Bloc = rgb(0,191,255, max = 255),
    Green = rgb(0,255,0, max = 255)
  )

  x$party$USA <- c(
    Republican = rgb(255, 39, 0, max = 255),
    Democratic = rgb(0, 143, 213, max = 255),
    Independent = rgb(119, 171, 67, max = 255)
  )

  x$continents <- c(
    Africa = rgb(127,59,8, max = 255), # 52
    Americas = rgb(165,0,38, max = 255), # 25
    Asia = rgb(64,0,75, max = 255), # 33
    Europe = rgb(39,100,25, max = 255), # 30
    Oceania = rgb(49,54,149, max = 255) # 2
  )

  x$regions <- c(
    NO = rgb(16, 78, 139, max = 255),
    NE = rgb(110, 139, 61, max = 255),
    CO = rgb(154, 50, 205, max = 255),
    SE = rgb(255, 193, 37, max = 255),
    SU = rgb(255, 193, 37, max = 255)
  )
  x$countries <- c()
  ## return
  x
}
NULL


#' @title Color Palettes for Publication (discrete)
#'
#' @description Color palettes for publication-quality graphs. See details.
#' @inheritParams ggplot2::scale_colour_hue
#' @param palette the palette name, a character string.
#' @keywords ggplot2
#'
#' @details The following palettes are available:
#' \itemize{
#' \item {"pub12"}{A 12-color colorblind safe qualitative discrete palette.}
#' \item{"gray5"}{5-tons of gray.}
#' \item {"carnival"}{A 5-color palette inspired in the Brazilian samba schools.}
#' \item {"tableau20"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#' \item {"tableau10"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#'  \item {"tableau10light"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#' \item {"tableau10medium"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#'  \item {"fte"}{fivethirtyeight.com color scales}
#' }
#' @examples
#' library(scales)
#' library(ggplot2)
#'
#' show_col(pub_color_pal("pub12")(12))
#' show_col(pub_color_pal("gray5")(6), labels = FALSE)
#' show_col(pub_color_pal("chalk")(8))
#' show_col(pub_color_pal("carnival")(4))
#' show_col(pub_color_pal("gdocs")(18))
#' show_col(pub_color_pal("tableau20")(20))
#' show_col(pub_color_pal("tableau10")(10))
#' show_col(pub_color_pal("tableau10medium")(10))
#' show_col(pub_color_pal("tableau10light")(10))
#' show_col(pub_color_pal("cyclic")(20))
#' show_col(pub_color_pal("bivariate1")(9))
#'
#' @export
`pub_color_pal` <- function(palette = "pub12") {
  pal.list <- Palettes$pub$colors
  if (!palette %in% c(
    names(pal.list), "pub12", "gray5", "tableau10", "tableau20", "tableau10medium", "tableau10light", "manyeyes", "fte", "greenorange12", "cyclic", "purplegray12", "bluered12", "bivariate1", "bivariate2", "bivariate3", "bivariate4" )) {
    stop(sprintf("%s is not a valid palette name", palette))
  }
  if (palette == "pub12") {
    types <- pal.list[["pub12"]][seq(1, 12, by = 1)]
   } else if (palette == "fte") {
   types <- pal.list[["fte"]][seq(1, 4, by = 1)]
  } else if (palette == "tableau10") {
    types <- pal.list[["tableau20"]][seq(1, 20, by = 2)]
  } else if (palette == "tableau10light") {
    types <- pal.list[["tableau20"]][seq(2, 20, by = 2)]
  } else if (palette == "manyeyes") {
    types <- pal.list[["manyeyes"]][seq(1, 19, by = 1)]
  } else {
    types <- pal.list[[palette]]
  }
  function(n) {
   unname(types)[seq_len(n)]
  }
}



#' @title Publication color scales.
#'
#' @description See \code{\link{pub_color_pal}} for details.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams pub_color_pal
#' @param palette the palette name, a character string.
#' @family color publication
#' @rdname color_pub
#' @keywords ggplot2
#' @export
#' @seealso \code{\link{pub_color_pal}} for references.
#'
scale_color_pub <- function(palette = "pub12", ...) {
  discrete_scale("color", "pub", pub_color_pal(palette), ...)
}



#' @title Publication color scales.
#'
#' @description See \code{\link{pub_color_pal}} for details.
#' @inheritParams ggplot2::scale_fill_hue
#' @inheritParams pub_color_pal
#' @param palette the palette name, a character string.
#' @family color publication
#' @keywords ggplot2
#' @rdname fill_pub
#' @export
`scale_fill_pub` <- function(palette = "pub12", ...) {
  discrete_scale("fill", "pub", pub_color_pal(palette), ...)
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
#' # Brazil
#' show_col(party_color_pal("BRA")(20))
#'
#' # Argentine
#' show_col(party_color_pal("ARG")(12))
#'
#' # US
#' show_col(party_color_pal("USA")(6))
#'
#' # Canada
#' show_col(party_color_pal("CAN")(10))
#'
#' party_color_pal("CAN", plot=TRUE, hex=FALSE)
#'
#' @export
#'
`party_color_pal` <- function(palette = "BRA", plot=FALSE, hex=FALSE) {
  pal.list <- Palettes$party
  if (!palette %in% c(names(pal.list), "BRA", "ARG", "CAN", "USA")) {
    stop(sprintf("%s is not a valid palette name", palette))
  }
  if (palette=="BRA") {
    types <- pal.list[["BRA"]][seq(1, 30, by = 1)]
  } else if (palette=="ARG") {
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
#' @inheritParams party_color_pal
#' @family color party
#' @param palette the palette name, a character string.
#' @seealso \code{\link{party_color_pal}} for details and references.
#' @keywords ggplot2
#' @export
#' @rdname color_party
`scale_color_party` <- function(palette = "BRA", ...) {
  discrete_scale("color", "party", party_color_pal(palette), ...)
}
NULL




#' @title Political Parties Color Palette (Discrete) and Scales
#' @description An N-color discrete palette for political parties.
#' @inheritParams ggplot2::scale_fill_hue
#' @inheritParams party_color_pal
#' @param palette the palette name, a character string.
#' @seealso \code{\link{party_color_pal}} for details and references.
#' @keywords ggplot2
#' @export
#' @rdname fill_party
`scale_fill_party` <- function(palette = "BRA", ...) {
  discrete_scale("fill", "party", party_color_pal(palette), ...)
}
NULL




#' @title Shape scales for theme_pub (discrete)
#'
#' @description Discrete shape scales for \code{theme_pub()}.
#' @param palette the palette name, a character string.
#' @family shape pub
#' @keywords ggplot2
#' @export
#' @examples
#' library(scales)
#'
#' pub_shape_pal("default")(6)
#' pub_shape_pal("proportions")(4)
#' pub_shape_pal("gender")(2)
#'
`pub_shape_pal` <- function(palette = "pub") {
  scales::manual_pal(unname(Palettes$pub$shapes[[palette]]))
}
NULL



#' @title Shape scales for theme_pub (discrete)
#'
#' @description See \code{\link{pub_shape_pal}} for details.
#'
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritParams pub_shape_pal
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
`scale_shape_pub` <- function(palette = "pub", ...) {
  discrete_scale("shape", "pub", pub_shape_pal(palette), ...)
}
NULL



# http://www.colorhexa.com/
# http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
# http://www.personal.psu.edu/cab38/ColorSch/SchHTMLs/CBColorSeqSeq.html
# http://www.personal.psu.edu/cab38/ColorSch/Schemes.html
# http://andywoodruff.com/blog/value-by-alpha-maps/
# http://www.farb-tabelle.de/en/rgb2hex.htm?
