## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE, echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5)

## ----echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5----

Previewplot() + theme_grey()

## ---- echo=TRUE, echo=TRUE, size='\\tiny'--------------------------------
Previewplot() + 
  theme_538() + 
  align_title_right()


## ---- echo=TRUE, echo=TRUE, size='\\tiny'--------------------------------
Previewplot() + 
  theme_538() + 
  no_y_gridlines()


## ---- echo=TRUE, echo=TRUE, size='\\tiny'--------------------------------

Previewplot() + 
  theme_538() + 
  no_y_gridlines() +
  no_x_gridlines()


