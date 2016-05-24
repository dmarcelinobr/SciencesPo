#' @title Palette data for the themes used by package
#'
#' @description Data used by the palettes in the package.
#'
#' @format A \code{list}.
#'
Palletes <- {
  x <- list()
  x$pub <- list()
  x$pub$colors <-
    list(
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
      pub12 = c(
        rgb(56, 108, 176, max = 255),
        rgb(253, 180, 98, max = 255),
        rgb(127, 201, 127, max = 255),
        rgb(239, 59, 44, max = 255),
        rgb(102, 37, 6, max = 255),
        rgb(166, 206, 227, max = 255),
        rgb(251, 154, 153, max = 255),
        rgb(152, 78, 163, max = 255),
        rgb(255, 255, 51, max = 255)
      ),
      gray5 = c(
        rgb(96, 99, 106, max = 255),
        rgb(165, 172, 175, max = 255),
        rgb(65, 68, 81, max = 255),
        rgb(143, 135, 130, max = 255),
        rgb(207, 207, 207, max = 255)
      ),
    fte = c(
        rgb(255, 39, 0, max = 255),
        rgb(0, 143, 213, max = 255),
        rgb(119, 171, 67, max = 255),
        rgb(60, 60, 60, max = 255),
        rgb(210, 210, 210, max = 255),
        rgb(240, 240, 240, max = 255)
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
      colorblind = c(
        rgb(0, 107, 164, max = 255),
        rgb(255, 128, 14, max = 255),
        rgb(171, 171, 171, max = 255),
        rgb(89, 89, 89, max = 255),
        rgb(95, 158, 209, max = 255),
        rgb(200, 82, 0, max = 255),
        rgb(137, 137, 137, max = 255),
        rgb(162, 200, 236, max = 255),
        rgb(255, 188, 121, max = 255),
        rgb(207, 207, 207, max = 255)
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
      default = c(
        -16L,
        -15L,
        -18L,
        -0x25B3L, # up-pointing triangle
        -0x25BDL, # down-pointing triangle
        -0x25B7L, # right-pointing triangle
        -0x25C1L, # left-pointing triangle
        -0x25B2L, # up-pointing triangle
        -0x25BCL, # down-pointing triangle
        -0x25B6L, # right-pointing triangle
        -0x25C0L # left-pointing triangle
      ),
      proportions = c(
        -0x25CBL, # White circle
        -0x25DL, # with upper right quadrant black
        -0x25D1L, # with right half black
        -0x25D5L, # # with upper left quadrant black
        -0x25CFL # Black circle
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
  x$development <- c(
    autumn = rgb(16, 78, 139, max = 255),
    spring = rgb(110, 139, 61, max = 255),
    summer = rgb(154, 50, 205, max = 255),
    winter = rgb(255, 193, 37, max = 255)
  )
  x$countries <- c()
  ## return
  x
}
NULL


#' @title Color Palettes for Publication (discrete)
#'
#' @description Color palettes for publication-quality graphs. See details.
#' @param palette the palette name.
#'
#' @details The following palettes are available:
#' \itemize{
#' \item {"pub12"}{Default colors of theme_pub}
#' \item {"tableau20"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#' \item {"tableau10"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#' \item {"colorblind"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#'  \item {"tableau10light"}{Based on software \href{http://www.tableausoftware.com/}{Tableau}}
#'  \item {"fte"}{fivethirtyeight.com color scales}
#' }
#' @examples
#' library(scales)
#' show_col(pub_color_pal("pub12")(12))
#' show_col(pub_color_pal("gray5")(6), labels = FALSE)
#' show_col(pub_color_pal("fte")(4))
#' show_col(pub_color_pal("colorblind")(10))
#' show_col(pub_color_pal("tableau20")(20))
#' show_col(pub_color_pal("tableau10")(10))
#' show_col(pub_color_pal("tableau10medium")(10))
#' show_col(pub_color_pal("tableau10light")(10))
#' show_col(pub_color_pal("trafficlight")(9))
#' show_col(pub_color_pal("cyclic")(20))
#' show_col(pub_color_pal("purplegray12")(12))
#' show_col(pub_color_pal("greenorange12")(12))
#' show_col(pub_color_pal("bluered12")(12))
#' show_col(pub_color_pal("bivariate1")(9))
#' show_col(pub_color_pal("bivariate2")(9))
#' show_col(pub_color_pal("bivariate3")(9))
#' show_col(pub_color_pal("bivariate4")(9))
#'
#' @export
`pub_color_pal` <- function(palette = "pub12") {
  pal.list <- Palletes$pub$colors
  if (!palette %in% c(
    names(pal.list), "pub12", "gray5", "tableau10", "tableau20", "tableau10medium", "tableau10light", "colorblind", "fte", "greenorange12", "cyclic", "purplegray12", "bluered12", "bivariate1", "bivariate2", "bivariate3", "bivariate4" )) {
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
  } else if (palette == "colorblind") {
    types <- pal.list[["colorblind"]][seq(1, 10, by = 1)]
  } else {
    types <- pal.list[[palette]]
  }
  function(n) {
    unname(types)[seq_len(n)]
  }
}
NULL


#' @title Publication color scales.
#'
#' @description See \code{\link{pub_color_pal}} for details.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams pub_color_pal
#' @family colour publication
#' @rdname scale_color_pub
#' @export
#' @seealso \code{\link{pub_color_pal}} for references.
#'
scale_color_pub <- function(palette = "pub12", ...) {
  discrete_scale("colour", "pub", pub_color_pal(palette), ...)
}
NULL



#' @title Publication color scales.
#'
#' @description See \code{\link{pub_color_pal}} for details.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams pub_color_pal
#' @family colour publication
#' @rdname scale_fill_pub
#' @export
scale_fill_pub <- function(palette = "pub12", ...) {
  discrete_scale("fill", "pub", pub_color_pal(palette), ...)
}
NULL




#' @title Color Palettes for Political Organizations (discrete)
#'
#' @description Color palettes for political organizations.
#'
#' @param palette the palette name.
#' @param label if \code{TRUE}, the party label associated with the color is returned instead.
#' @family color party
#' @examples
#' library(scales)
#' show_col(party_color_pal("BRA")(20))
#'
#' # Argentine
#' show_col(party_color_pal("ARG")(10))
#'
#' # US
#' show_col(party_color_pal("USA")(4))
#'
#' # Canada
#' show_col(party_color_pal("CAN")(10))
#'
#' @export
#'
`party_color_pal` <- function(palette = "BRA", label=FALSE) {
  pal.list <- Palletes$party
  if (!palette %in% c(names(pal.list), "BRA", "ARG", "CAN", "USA")) {
    stop(sprintf("%s is not a valid palette name", palette))
  }
  if (palette == "BRA") {
    types <- pal.list[["BRA"]][seq(1, 30, by = 1)]
  } else if (palette == "ARG") {
    types <- pal.list[["ARG"]][seq(1, 20, by = 1)]
  } else {
    types <- pal.list[[palette]]
  }
  function(n) {
    unname(types)[seq_len(n)]
  }
}
NULL



#' @title List Political Parties
#' @description Returns a list of available political parties associated with a color.
#' @note Warning: the list can be huge!\cr
#' @family party aeshetics
#' @export
list_parties <- function() {
  sort(names(party))
}
NULL


#' @title Political Parties Color Scales
#'
#' @description Scale color for political parties.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams party_color_pal
#' @family color party
#' @rdname scale_color_party
#' @export
#' @seealso \code{\link{party_color_pal}} for references.
#'
scale_color_party <- function(palette = "BRA", ...) {
  discrete_scale("colour", "party", party_color_pal(palette), ...)
}
NULL


#' @export
#' @rdname scale_color_party
scale_fill_party <- function(palette = "BRA", ...) {
  discrete_scale("fill", "party", party_color_pal(palette), ...)
}
NULL




#' @title Shape scales for theme_pub (discrete)
#'
#' @description Discrete shape scales for theme_pub.
#'
#' @export
#' @param palette Palette name.
#' @family shape pub
#' @examples
#' pub_shape_pal("proportions")(2)
#'
`pub_shape_pal` <- function(palette = "proportions") {
  manual_pal(unname(Palletes$pub$shapes[[palette]]))
}
NULL



#' @title Shape scales for theme_pub (discrete)
#'
#' @description See \code{\link{pub_shape_pal}} for details.
#'
#' @export
#' @inheritParams pub_shape_pal
#' @inheritParams ggplot2::scale_x_discrete
#' @family shape pub
#' @examples
#' library("ggplot2")
#' library("scales")
#' p <- ggplot(mtcars) +
#'      geom_point(aes(x = wt, y = mpg, shape = factor(gear))) +
#'      facet_wrap(~am)
#' p + scale_shape_pub()
#'
`scale_shape_pub` <- function(palette = "default", ...) {
  discrete_scale("shape", "pub", pub_shape_pal(palette), ...)
}
NULL



#' show_shapes(shape_pal()(3), labels=TRUE)
#'


# http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
# http://www.personal.psu.edu/cab38/ColorSch/SchHTMLs/CBColorSeqSeq.html
# http://www.personal.psu.edu/cab38/ColorSch/Schemes.html
# http://andywoodruff.com/blog/value-by-alpha-maps/
# http://color.adobe.com/
# http://www.farb-tabelle.de/en/rgb2hex.htm?
# "#9b59b6", "#3498db", "#95a5a6", "#e74c3c", "#34495e", "#2ecc71" "FF7E0D"
