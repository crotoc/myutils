#' formatPvalue
#'
#' format pvalue
#'
#' @param x a vector
#' @param digits reserve how many digits. default is 2
#' @param n The threshold of the scientific notion
#' @param mode When set to min, format as < Min; When set to min_digits, set to Min/10
#' @param Min the Min threshold to change fmt
#' 
#' 
#' @return formated vector
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @keywords format pvalue
#'
#' @examples
#' formatPvalue()
#'
#' @export
#' @import data.table
#' 

formatPvalue <- function(x, digits=2, n=3, mode="min_digits",Min=1e-6){
    tb <- data.frame(x=x) %>% data.table
    tb[x>= 10^(-n),fmt:=format(x,digits=digits,drop0trailing = T)]
    tb[x< 10^(-n),fmt:=format(x,digits=digits,drop0trailing = T,scientific = T)]
    if(mode == "min")  tb[x< Min,fmt:=paste("<",Min,sep="")]
    else if(mode == "min_digits")  tb[x< Min,fmt:=Min/10]
    #if(!is.na(mode) && mode != "min" && mode != "min_digits") stop("wrong mode")
    return(tb$fmt)
}
