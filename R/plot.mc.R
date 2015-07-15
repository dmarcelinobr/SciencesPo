#' @title Plot Markov Chains
#'
#' @description A plotting method for markov chains.
#'
#' @param reps the number of replications
#' @param prob the probability matrix
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#'  A <-matrix(c(.9,.8,0,0,.1,.2,0,0,0,0,.5,.6,0,0,.5,.4), nrow=4)
#'  B <-matrix(rep(.3,16), nrow=4); diag(B)<-.1
#'  C <-matrix(c(.9,0,0,0, .1,.9,0,0 ,0,.1,.8,.1, 0,0,.2,.9), nrow=4)
#'  D <-matrix(c(.9,0,0,.1,.1,.9,0,0,0,.1,.9,0,0,0,.1,.9), nrow=4)
#' mc.plot(100, B)
#'
#' @export
mc.plot <-function(reps, prob){
  graphics::plot(1:reps, mc(prob, reps), "l", xlab="Replications", ylab="Random Values")
}
