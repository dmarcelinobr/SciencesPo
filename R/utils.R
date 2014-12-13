

RECYCLEWARNING <- NULL
.onLoad <- function(libname, pkgname){
  RECYCLEWARNING <<- gettext(tryCatch( (1:2)+(1:3),warning=function(w) w$message ))
}



.onAttach <- function(libname, pkgname) {
  	pkgEnv = pos.to.env(match('package:SciencesPo', search()))	
}



Restore <- function(invec) {
  if (!"Factor" %in% class(invec)) stop("Wrong class of input.")
  if (is.null(attr(invec, "Input"))) stop("No attribute named 'Input' found.")
  attr(invec, "Input")
}




Factor <- function(invec, levels = list(), store = TRUE, ...) {
  Fac <- factor(invec, ...)
  levels(Fac) <- levels
  if (isTRUE(store)) attr(Fac, "Input") <- invec
  class(Fac) <- c("Factor", class(Fac))
  Fac
}



print.Factor <- function(x, ...) {
  if (!is.null(attr(x, "Input"))) {
    cat("Input values:\n")
    print(attr(x, "Input"))
    attr(x, "Input") <- NULL
    cat("\n")
    cat("Factored output:\n")
    print.factor(x)
  } else {
    cat("Factored output:\n")
    print.factor(x)
  }
}


print.data.frame <- function(x, ...) {
    oWidth <- getOption("width")
    oMaxPrint <- getOption("max.print")
    on.exit(options(width=oWidth, max.print=oMaxPrint))
    options(width=10000, max.print=300)
    base::print.data.frame(x, ...)
}

clc <- function() cat(rep("\n",50))



library(ggplot2)
library(gridExtra)
title <- "Default title"
data(movies)

## Init dialog
require(tkrplot)
if (!exists("slider.env")) slider.env <<- new.env(parent = .GlobalEnv)
require(tcltk)
nt <- tktoplevel()
tkwm.title(nt, title)
tkwm.geometry(nt, "480x600+0+10")
assign("tktop.slider", nt, envir = slider.env)
"relax"
nt.bak <- nt
sl.frame <- tkframe(nt)
gr.frame <- tkframe(nt)
tx.frame <- tkframe(nt)
tkpack(sl.frame, tx.frame, gr.frame, side = "bottom")
## First default plot
newpl <- function(...) {
  dummydf <- data.frame('x'=1:10, 'y'=1:10)
  dummy <- ggplot(dummydf, aes(x=x, y=y)) + geom_point(size=0) + xlim(0, 10) + ylim(0, 100) + 
    geom_text(aes(label='Generating plot...', x=5, y=50), size=9)
  print(dummy)
  }
img <- tkrplot::tkrplot(gr.frame, newpl, vscale = 1, hscale = 1)
tkpack(img, side = "top")
assign("img", img, envir = slider.env)
tkpack(fr <- tkframe(sl.frame), side = 'top')

## Creating slider, textbox and labels
sc <- tkscale(fr, from = 0, to = 5, showvalue = TRUE, resolution = 0.1, orient = "horiz")
tb <- tkentry(fr, width=4)
lab <- tklabel(fr, text = 'Select binwidth ', width = "16")
orlabel <- tklabel(fr, text=' or ', width='4')
tkpack(lab, sc, orlabel, tb, side = 'left')
tkpack(textinfo <- tkframe(tx.frame), side = 'top')


## Creating objects and variables associated with slider and textbox
assign("sc", sc, envir = slider.env)
assign("tb", tb, envir = slider.env)
assign('inputsc', tclVar(2.5), envir=slider.env)
assign('inputtb', tclVar('2.5'), envir=slider.env)
eval(parse(text = "tkconfigure(sc, variable=inputsc)"), envir = slider.env)
eval(parse(text = "tkconfigure(tb, textvariable=inputtb)"), envir = slider.env)

## Function to update the textbox value when the slider has changed
sync_textbox <- function() {
  bwidth_sl <- tclvalue(get('inputsc', envir=slider.env))
  assign('inputtb', tclVar(bwidth_sl), envir=slider.env)
  eval(parse(text = "tkconfigure(tb, textvariable=inputtb)"), envir = slider.env)
}

## Function to update the slider value when the textbox has changed
sync_slider <- function() {
  bwidth_tb <- tclvalue(get('inputtb', envir=slider.env))
  assign('inputsc', tclVar(bwidth_tb), envir=slider.env)
  eval(parse(text = "tkconfigure(sc, variable=inputsc)"), envir = slider.env)
}

## Function to refresh the plot
refresh <- function(bwidth) {
  histplot <- ggplot(data=movies, aes_string(x="rating")) +
     geom_histogram(binwidth=bwidth, 
                    aes(y = ..density..), fill='skyblue') + 
                      theme(axis.title.x=element_text(size=15), axis.title.y=element_text(size=15), 
                            axis.text.x=element_text(size=10, colour='black'),
                            axis.text.y=element_text(size=10, colour='black'))
  print(histplot)
}

## Bindings : association of certain functions to certain events for the slider
## and the textbox

tkbind(sc, "<ButtonRelease>", function(...) {
  bwidth <- as.numeric(tclvalue(get('inputsc', envir=slider.env)))
  tkrreplot(get('img',envir=slider.env),fun=function() { refresh(bwidth); sync_textbox()})
})

tkbind(tb, "<Return>", function(...) {
  bwidth <- as.numeric(tclvalue(get('inputtb', envir=slider.env)))
  tkrreplot(get('img',envir=slider.env),fun=function() { refresh(bwidth); sync_slider()})
})


set.seed(123)
colors<- c( rep("yellow", 5), rep("blue", 5), rep("green", 5) )
shapes<- c("circle", "star", "oblong")
numbers<-sample(1:15,replace=T)
group<-sample(LETTERS, 15, replace=T)
mydf <-data.frame(colors,shapes,numbers,group)
mydf






adjustedTableLaTeX <- function(x, y, caption="", label="", ...) {
	t1 = prop.table(table(x, y, useNA='no'), 2) * 100
	t1 = rbind(t1, NA)
	row.names(t1)[nrow(t1)] = 'Missing'
	t1['Missing',1:ncol(t1)] = NA
	t2 = prop.table(table(x, y, useNA='ifany'), 2) * 100
	row.names(t2)[is.na(row.names(t2))] = 'Missing'
	t = cbind(t2, t1)
	cols = c()
	for(i in 1:(ncol(t)/2)) { cols = c(cols, i, (i + (ncol(t)/2))) }
	t = t[,cols]
	t = as.data.frame(t)
	addtorow = list()
	addtorow$pos = list()
	addtorow$pos[[1]] = c(-1)
	addtorow$command = paste(c("\\hline ",
		paste( '& \\multicolumn{2}{c}{', names(t)[seq(1, ncol(t), by=2)], '}', sep='' ),
		"\\\\ \\cline{2-", (ncol(t) + 1), "} ",
		rep(" & Percent & Adjusted", (ncol(t)/2)),
		" \\\\"), collapse="" )
	xt = xtable(t, caption=caption, label=label, align=c('l', rep('r', ncol(t))))
	print(xt, hline.after=c(-1, nrow(xt)), add.to.row=addtorow, include.colnames=FALSE, ...)
}


#' Returns a factor variable using the given breaks and labels. Will calculate the
#' age based upon the calcDate if given, otherwise the ageCol will be used.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getAgeGroups <- function (dateCol, calcDate, breaks, labels, 
		ageCol = NULL) {
    if (is.null(ageCol)) {
        age = getAge(dateCol, calcDate)
    }
    else {
        age = ageCol
    }
    cut(age, breaks = breaks, labels = labels)
}

#' Returns the age groupings used by IPEDS.
#' @author Jason Bryer <jbryer@@excelsior.edu>
#' @export
getAgeGroupsIPEDS <- function (dateCol, calcDate, ageCol = NULL) {
	breaks = c(0, 17, 19, 21, 24, 29, 34, 39, 49, 64, Inf)
	labels = c("<18", "18-19", "20-21", "22-24", "25-29", 
		"30-34", "35-39", "40-49", "50-64", "65+")
    if (is.null(ageCol)) {
        r = getAgeGroups(dateCol, calcDate, breaks, labels)
    } else {
        r = getAgeGroups(breaks=breaks, labels=labels, ageCol=ageCol)
    }
    return(r)
}



'
#'
#' @export
demographics <- function(df, useNA='ifany') {
	results = data.frame(Category=character(), Frequency=numeric(), Percentage=numeric())
	
	for(i in seq_len(ncol(df))) {
		t = as.data.frame(table(df[,i], useNA=useNA), stringAsFactors=FALSE)
		t$Var1 = as.character(t$Var1)
		names(t) = c('Category', 'Frequency')
		t$Percentage = t$Frequency / sum(t$Frequency) * 100
		m = which(is.na(t$Category))
		if(length(m) > 0) {
			t[m,'Category'] = 'Missing'
		}
		results = rbind(results, 
						data.frame(Category=names(df)[i], Frequency=NA, Percentage=NA),
						t)
	}
	class(results) = c("demographics", "data.frame")
	return(results)
}


#'
#'
#' @S3method xtable demographics
#' @export
xtable.demographics <- function(x, caption = NULL, label = NULL, align = NULL,
								digits = NULL, display = NULL, ...) {
	require(xtable)
	cat('using xtable...')
	
}


ggplot_size_legend <- function(p, legend_size=0.1, legend_text_size=10) { 
	Layout <- grid.layout(nrow = 1, ncol = 2, widths = unit(c(1-legend_size, legend_size), 
	c("null", "null")), heights = unit(1, "null"))
	vplayout <- function(...) { 
		grid.newpage() 
		pushViewport(viewport(layout = Layout)) 
	} 
	subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) 
	#create two plots, one with legend, one without 
	pl <- p + opts(legend.position = "none") 
	pn <- p + theme_grey(legend_text_size) + opts(keep = "legend_box") 
	#print the plot 
	vplayout() 
	print(pl, vp = subplot(1, 1)) 
	print(pn, vp = subplot(1, 2)) 
}


mean.n   <- function(df, n) {
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, means, NA)
}

#' Analyze Likert type items.
#'
#' This function will provide various statistics about a set of likert
#' items. The resulting object will have the following items:
#' 
#' \itemize{
#'    \item \code{results} - this data frame will contain a column 'Item', 'Group' (if a 
#'          grouping variable was specified, and a column for each level of the
#'          items (e.g. agree, disagree, etc.). The value within each cell corresponds
#'          to the percentage of responses for that level and group.
#'    \item \code{items} - a copy of the original items data frame.
#'    \item \code{grouping} - a copy of the original grouping vector.
#'    \item \code{nlevels} - the number of levels used in the calculations.
#' }
#'
#' @export
#' @param items data frame containing the likert based items. The variables
#'        in the data frame should be factors.
#' @param summary a pre-summarized data frame. The first column must be the
#'        items and the remaining columns are the levels (e.g. strongly disagree,
#'        disagree, etc).
#' @param grouping (optional) should the results be summarized by the given
#'        grouping variable.
#' @param importance a data frame of the same dimensions as items containing
#'        an importance rating for each item. The order of columns should match
#'        and the names from items will be used.
#' @param nlevels number of possible levels. Only necessary if there are missing levels.
#' @return a likert class with the following elements: results, items, grouping,
#'        nlevels, and summary.
#' @seealso plot.likert
#' @seealso summary.likert
#' @examples
#' data(pisaitems)
#' items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
#' names(items29) <- c("Magazines", "Comic books", "Fiction", 
#'                    "Non-fiction books", "Newspapers")
#' l29 <- likert(items29)
#' summary(l29)
#' plot(l29)



#' Convert Factor Levels into Strings
#' 
#' @param x a factor whose levels will be converted.
#' @export
#' @examples
#' mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
#' myvar <- factor(sample(mylevels[1:5], 10, replace=TRUE))
#' unclass(test) # For testing order
#' destring(myvar)
#Say I don't want to mess around with changing classes after reading data into R. If I want one column to be character and the others as the default class, I can use readLines to quickly read the first line of the .csv (i.e. the column header line, if present) and set up a vector to be passed to the colClasses argument of read.csv
sapply(read.csv('cats.csv'), class)
cc1 <- col.classes('cats.csv', 'length', 'numeric')
rr1 <- read.csv('cats.csv', colClasses = cc1)
sapply(rr1, class)

col.classes <- function(csv, col, class){
    g <- readLines(csv, n = 1)
    n <- unlist(strsplit(g, ","))
    col.classes <- ifelse(n %in% col, class, NA)
    return(col.classes)
}