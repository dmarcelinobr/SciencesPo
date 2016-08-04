## ----setup, echo = FALSE, include=FALSE----------------------------------
knitr::opts_chunk$set(cache=TRUE, echo=TRUE, warning = FALSE, message=FALSE, collapse = FALSE, prompt=FALSE,comment=NA, fig.align="center", fig.width=5, fig.height=4, dpi = 96, fig.show = "hold", fig.keep="last", sanitize=TRUE)

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
library("scales", quietly = TRUE)

show_col(pub_pal("pub12")(12))

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
show_col(pub_pal("gray5")(6), labels = FALSE)

## ---- fig.width = 10.67, fig.height = 5, out.width = 790, out.height = 350----
show_col(pub_pal("manyeyes")(20))

## ---- fig.width = 10.67, fig.height = 5, out.width = 790, out.height = 350----
show_col(pub_pal("tableau20")(20))

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
show_col(pub_pal("tableau10")(10))

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
 show_col(pub_pal("tableau10light")(10))

## ---- fig.width = 10.67, fig.height = 5, out.width = 790, out.height = 350----
show_col(pub_pal("cyclic")(20))

## ---- fig.width = 10.67, fig.height = 5, out.width = 790, out.height = 350----
party_pal("BRA", plot=TRUE)

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
show_col(pub_pal("trafficlight")(9))

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
show_col(pub_pal("bivariate1")(9))

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
show_col(pub_pal("bivariate2")(9))

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
show_col(pub_pal("bivariate3")(9))

## ---- fig.width = 10.67, fig.height = 4, out.width = 790, out.height = 300----
show_col(pub_pal("bivariate4")(9))

## ---- echo=FALSE---------------------------------------------------------
library("devtools", quietly = TRUE)
session_info()

