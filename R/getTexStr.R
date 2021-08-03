#' getTexStr
#'
#' Description: format tab to Tex string
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#'
#' @return None
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @references 
#' @seealso 
#' @keywords 
#'
#' @examples
#' getTexStr
#'
#' @export
#' @import data.table
#' @import xtable
#'
#'


getTexStr <- function(dt,width=0,height=0,splitN=4,tablestr="sidewaystable"){
    dt <- lapply(split(2:length(dt),ceiling(seq_along(2:length(dt))/splitN)),function(y)dt[,c(1,y),with=F])
    npages <- split(1:length(dt),ceiling(seq_along(1:length(dt))/4))
    out <- lapply(npages,function(x){sapply(x,function(y){capture.output(print(xtable(dt[[y]]))) %>% .[c(-1:-4,-length(.))] %>% paste(., collapse = "\n")}) %>% unlist  %>% paste(.,collapse = " \n \\newline \n \\vspace{5mm} \\newline \n")})
    out <- lapply(1:length(out),FUN=function(x)paste("\\begin{",tablestr,"}  \n  ",out[[x]],"  \n \\end{",tablestr,"};",sep = "")) %>% unlist %>% paste(., collapse = "\n \\clearpage \n")
    return(out)
}

