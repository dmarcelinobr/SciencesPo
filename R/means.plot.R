#' @title Mean plot
#' @description Mean plot with ggplot2
#'
#' @param .data The data frame.
#' @param xvar The name of column containing x variable. Default value is NULL.
#' @param yvar The name of column containing y variable.
#' @param group.var The name of column containing group variable. This variable is used to color plot according to the group.
#' @param plot.type The graph type (line.plot or bar.plot).
#' @param error.bars Type of the error bars (standard error, standard deviation, confidence interval)
#' @param level The confidence level (error.bars = conf.int can be applied).
#' @param bar.width The width of the bars.
#' @param bar.color The colour of the bar.
#' @param verbose If \code{TRUE}, a table with means is shown.
#' @param paired.sample A logical in case the sample groups are paried, default is \code{FALSE}.
#' @param id The column containing the unique identification of the observations/subjects.Must be provided when \code{paired.sample=TRUE}.
#' @param position To adjust the error bar position in the barplot.
#' @export
means.plot<-function(.data, xvar,
                     yvar,
                     group.var=NULL,
                            plot.type=c('lineplot', 'barplot'),
                            error.bars = c("se", "sd","conf.int", "none"), level = 0.95,
                            bar.width=0.2, bar.color=NULL,
                            verbose=TRUE,
                            paired.sample=FALSE, id=NULL,
                            position=position_dodge(0.9),
                            ...){
  .data=.data.frame(.data)
  .data[,xvar]=factor(.data[,xvar])

  if(paired.sample){
    # Treat subject ID as a factor
    if(is.null(group.var)) df <- summarySEwithin(.data, measurevar=yvar,
                                                 withinvars=c(xvar), idvar=id,conf.interval=level)
    else df <- summarySEwithin(.data, measurevar=yvar,idvar=id,
                               withinvars=c(group.var,xvar),conf.interval=level)

  }
  else{
    # summarySE provides the standard deviation, standard error of the mean,
    # and a (default 95%) confidence interval
    if(is.null(group.var)) df <- summarySE(.data, measurevar=yvar, groupvars=c(xvar),conf.interval=level)
    else df <- summarySE(.data, measurevar=yvar, groupvars=c(xvar,group.var),
conf.interval=level)
  }
  #calulation of error bar
  #satandard deviation
  ymin=NULL
  ymax=NULL
  if(error.bars[1]=='sd'){
    ymin=df[,yvar]-df[,'sd']
    ymax=df[,yvar]+df[,'sd']
  }
  #standard error
  else if(error.bars[1]=='se'){
    ymin=df[,yvar]-df[,'se']
    ymax=df[,yvar]+df[,'se']
  }
  #confidence interval
  else if(error.bars[1]=='conf.int'){
    ymin=df[,yvar]-df[,'ci']
    ymax=df[,yvar]+df[,'ci']
  }
  if(error.bars[1]=='none') .data=df
  else  .data=cbind(df, ymin=ymin, ymax=ymax)
  #lineplot
  if(plot.type[1]=='lineplot'){

    p<-line.plot(.data=.data, xvar=xvar, yvar=yvar,
                        group.var=group.var, ...)
    #add error bar
    if(error.bars[1]!='none'){
      if(is.null(bar.color))p<-p+ggplot2::geom_errorbar(ggplot2::aes_string(ymin='ymin', ymax='ymax'),
                                                   width=bar.width, ...)
      else p<-p+ggplot2::geom_errorbar(ggplot2::aes_string(ymin='ymin', ymax='ymax'),
                              width=bar.width,colour=bar.color,...)
    }
  }
  #barplot
  else if(plot.type[1]=='barplot'){
    p<-bar.plot(.data=.data, xvar=xvar, yvar=yvar,
                       group.var=group.var, position="dodge",...)
    #add error bar
    if(error.bars[1]!='none'){
      if(is.null(bar.color))p<-p+ggplot2::geom_errorbar(ggplot2::aes_string(ymin='ymin', ymax='ymax'),
                                                   width=bar.width,position=position, ...)
      else p<-p+ggplot2::geom_errorbar(ggplot2::aes_string(ymin='ymin', ymax='ymax'),
                              width=bar.width,colour=bar.color,
                              position=position,...)
    }
  }#end of barplot

  if(verbose){
    cat("\n\n----------------------------------------------\nSummary of Data\n----------------------------------------------\n")
    print(.data, digits=1)
    cat("\n----------------------------------------------\n\n")
  }

  p

}
