#' replaceNAwithZero
#'
#' Description: 
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
#' replaceNAwithZero
#'
#' @export
#' @import data.table
#' 
#'
replaceZerowithNA = function(DT) {
    for (i in names(DT))
        DT[get(i)==0, (i):=NA]
}


replaceNAwithZero = function(DT) {
    for (i in names(DT))
        DT[is.na(get(i)), (i):=0]
}
