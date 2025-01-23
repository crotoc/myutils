#' viewNoName
#'
#' view table without row and colnames
#'
#' @param table data.frame, matrix, data.table ...
#' @param n how many columns to display
#' 
#' @return None
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @keywords view 
#'
#' @examples
#' viewNoName(dt)
#'
#' @export
#' 
#'
#' 
viewNoName <- function(table, n=5){
    rownames(table) <- NULL
    colnames(table) <- NULL
    print(table[1:ifelse(nrow(table)<n,nrow(table),n),1:ifelse(ncol(table)<n,ncol(table),n)])
}
