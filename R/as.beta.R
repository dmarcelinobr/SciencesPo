as.beta <-
function (mod, stdev=1) 
    {
        coef <- summary(mod)$coef[-1, 1]
        sd.x <- sapply(mod$model[-1], sd)
        sd.y <- sapply(mod$model[1], sd)
		if(stdev==1){
        beta <- coef*(sd.x/sd.y)
        return(beta)
		}else{
	    beta <- coef*((stdev*sd.x)/(stdev*sd.y))
		}
    }
