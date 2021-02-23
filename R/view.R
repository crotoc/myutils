#' view
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param table data.frame, matrix, data.table ...
#' @param n how many columns to display
#' 
#' @return None
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @keywords plot
#'
#' @examples
#' view(dt)
#'
#' @export
#' 
#'
#' 
view <- function(table, n=5){
    print(table[1:ifelse(nrow(table)<n,nrow(table),n),1:ifelse(ncol(table)<n,ncol(table),n)])
}
