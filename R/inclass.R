#' Proportion of Rolls
#'
#' @description A function that simulates rolling a pair of fair dice. The goal
#'   of the function is to empirically calculate the proportion of times the sum
#'   of the dice take on certain numbers, given a specified number of rolls.
#' @param Rolls The number of times you roll the pair of dice
#' @param DiceSum A numeric vector, these are possible values for the sum of the
#'   dice. Elements of the vector can take any integer value between 2 and 12.
#'   The function will calculate the proportion of rolls for which the sum of
#'   the dice equals one of the specified integers.
#' @details The output should be the proportion of times the sum of the dice
#'   take on any of the values specified in your numeric vector input among the
#'   simulated rolls.
#' @return a numeric value
#' @examples
#' proportionofrolls(Rolls=100,DiceSum=c(8,9,10,11,12))

proportionofrolls<-function(Rolls=100,DiceSum=c(3,10,11)){

  d1<-c(1:6)
  d2<-c(1:6)
  vectout<-rep(NA,Rolls)

for(i in 1:Rolls){
  temp1<-sample(d1,1)
  temp2<-sample(d2,1)
  tempsum<-temp1+temp2
if (tempsum %in% DiceSum) tempindictor<-1 else tempindictor<-0
  vectout[i]<-tempindictor
  }
  proportionrolls<-sum(vectout)/Rolls
  return(proportionrolls)
}



#' Log-Transform a Numeric Vector
#' @description This is an unnecessary function I created for the purposes of
#'   instruction
#' @param NumericVector A numeric vector you would like to log-transform
#' @details This function is pretty self explanatory
#' @return A list of two objects:
#'   \item{InputVector}{Input numeric vector}
#'   \item{logTransformVec}{Input numeric vector log-transformed}
#' @examples
#' saveout<-logtransformed(NumericVector = c(5.21, 2.03, 1.49, 13.28,
#'                                         474.10, 21.81, 3.19, 1.53))
#' saveout$InputVector
#' saveout$logTransformVec

logtransformed<-function(NumericVector=NULL){
  inputvect<-NumericVector
  transformed<-log(NumericVector)
  output<-list(InputVector=inputvect,logTransformVec=transformed)
  return(output)
}

