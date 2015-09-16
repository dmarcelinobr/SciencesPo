#' @title Reliability Procedure
#'
#' @description Simulates the SPSS procedure Reliability
#'
#' @param \dots The parameters.
#'
#' @param vars A list of variables separated by comma.
#'
#' @examples
#' reliability(attitude)
#' reliability(attitude[,-6])
#' reliability(attitude$rating, attitude$complaints,
#' attitude$privileges, attitude$learning, attitude$raises)
#' @export
reliability = function(...)
{
  x = data.frame(...)
  if (ncol(x) < 2)
  {
    cat('Error: Number of variables must be greater 1!\n')
  }
  else
  {
    x = x[stats::complete.cases(x),]
    if ((ncol(x) > 1) & (nrow(x) > 2))
    {
      n = nrow(x)
      p = ncol(x)
      alpha = (p/(p-1))*(1-sum(diag(var(x)))/var(rowSums(x)))
      sd.alpha = (p/(p-1))*(1-sum(diag(var(scale(x))))/var(rowSums(scale(x))))
      scalemean.i.d = NULL
      scalevar.i.d = NULL
      r.itc = NULL
      alpha.i.d = NULL
      if (p > 2)
      {
        for(i in 1:p)
        {
          scalemean.i.d[i] = mean(rowSums(x[,-i]))
          scalevar.i.d[i] = var(rowSums(x[,-i]))
          r.itc[i] = cor(x[,i],rowSums(x[,-i]))
          alpha.i.d[i] = ((p-1)/(p-2))*
            (1-sum(diag(var(x[,-i])))/var(rowSums(x[,-i])))
        }
        res=cbind(scalemean.i.d,scalevar.i.d,r.itc,alpha.i.d)
        colnames(res)=c("scale.mean","scale.var","r.itc","alpha")
        rownames(res)=colnames(x)
        list(Statistics.if.item.deleted=res,
             alpha=alpha,standardized.item.alpha=sd.alpha,n.valid=n,items=p)
      }
      else
      {
        list(alpha=alpha,standardized.item.alpha=sd.alpha,n.valid=n,items=p)
      }
    }
    else
    {
      cat('Error: Number of valid cases must be greater 2!\n')
    }
  }
}
NULL
