#' @title Linear model to equation style
#'
#' @description Produces a text equation style to be added in plots.
#'
#' @param .data The data.frame object.
#' @param x The independent variable(s).
#' @param y The dependent variable.
#' @param spaced A logical value indicating if spaces should be added; default is TRUE.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' lm2eqn("mtcars","wt","mpg")
#'
#' require(ggplot2)
#' mtcars$name=rownames(mtcars)
#' mtcars$selected=ifelse(mtcars$name %in% c("Toyota Corolla","Merc 240D"),1,0)
#' selected=mtcars[mtcars$selected==1,]
#' mylabel=lm2eqn("mtcars","wt","mpg")
#'
#' ggplot(mtcars,aes(x=wt,y=mpg,colour=selected))+
#'  geom_text(data=selected,aes(label=name),hjust=-0.1)+
#'  geom_smooth(method=lm)+
#'  geom_point()+
#'  annotate(geom='text',x=4,y=30,size=7,label=mylabel,family='Times',fontface='italic')+
#'  scale_colour_gradientn(colours=c("black","red"))+
#'  theme_pub()
#'
#' @export
`lm2eqn` <- function(.data, x, y, spaced=TRUE){
  fit=eval(parse(text=paste0("glm(",y,"~",x,", na.action=na.exclude, data=",.data,")")))
  intercept=round(stats::coef(fit)[1],1)
  slope=round(stats::coef(fit)[2],1)
  if(spaced) equation=paste0("y = ",slope,"x",ifelse(intercept>=0,' + ',' - '),abs(intercept))
   else equation=paste0("y==",slope,"*x",ifelse(intercept>=0,'+','-'),abs(intercept))
  p=round(summary(fit)$coeff[2,4],3)
  if(p==0) equation=paste(equation,"(p < 0.001)")
  else equation=paste(equation,"(p =",p,")")
  equation
}
NULL
