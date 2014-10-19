get.ci <-
function(vector, ci = 0.95){
	a <- mean(vector)
	s <- sd(vector)
	n <- length(vector)
	error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
	return(c(upper = a + error, mean = a, lower = a - error))
}
