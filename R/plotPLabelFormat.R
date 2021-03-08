#' plotPLabelFormat
#'
#' Create beautiful multiple pages plot from ggplot object.
#'
#' @param n pvalue vector
#' @param threshold label "< threshold" if lower than threshold
#' @param digits reserve n digits
#' 
#' @return a vector for labeling p value
#'
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @keywords plot
#'
#' @examples
#' out <- plotPLabelFormat(pvalue)
#' @import
#' data.table
#' @export
#' 
#' 
#'

plotPLabelFormat <- function(n,threshold=2.2e-16,digits=2) {
    n <-  data.frame(n=n) %>% data.table
    n[n<threshold,o:="<2.2e-16"]
    n[n>=0.01,o:=paste("==",round(n,digits=2),sep="")]
    n[n<0.01 & n>threshold,o:=paste("==",formatC(n,digits=digits,format="e"),sep="")]
    output <- sub("e", "%*%10^", n$o) #Replace e with 10^
    output <- sub("\\+0?", "", output) #Remove + symbol and leading zeros on expoent, if > 1
    output <- sub("-0?", "-", output) #Leaves - symbol but removes leading zeros on expoent, if < 1
    output
}
