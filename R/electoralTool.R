#' @encoding UTF-8
#' @title A Toolkit to analyze electoral data 
#' \code{electoralTool} can be used to analyze both aggregate data and individual voting data.
#' @param party identifies the variable containing party labels. It should be used only when response variable is a frequency variable containing number of votes at the aggregate level.
#' @param district identifies districts. It is required when there are more than one district.
#' @param seats  this option can be used to tell the program the number of seats by party. If used, the program will compute proportionality and parliamentary fragmentation.
#' @param polar identifies variables to compute polarization among groups, such as polarization by ideology. Up to five variables are allowed. If you use more than one variable, polarization will be computed using averaged polarization over the whole set of variables. Absolute and Euclidean measures of polarization are reported.
#' @param time tells the program that the data contains more than one election. This option identifies the date of the election or the order in which elections take place. Using this option means that the program will compute electoral and parliamentary volatitlity between elections.
#' @param blocks tells the program that parties are grouped into blocks to compute inter and intra blocks volatility. You can only use it if you set time(varname) previously. If you are using time(varname), but do not set blocks(varname), all the parties are suppossed to belong to the same block. Then, inter-blocks volatility will be equal to 0, and intra-blocks volatility will be equal to total volatility.
#' @param verbose if \code{verbose=TRUE}, tells the program to print the output.
#' 
#' @export
electoralTool <- function(party, district, seats, polar, time, blocks, verbose=TRUE){
# if(verbose)
}
NULL
