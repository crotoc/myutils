#' normalize output directory
#'
#' Description: normalize directory
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
#' normalize output directory
#'
#' @export
#' @import
#' @importFrom 
#'
#'

normalizeOutput <- function(opt){
    if(!grepl("^/",opt$output,perl = T))
        opt$output <- paste(opt$dir_out,"/",opt$output,sep = "")
    if(!dir.exists(dirname(opt$output)))
        dir.create(dirname(opt$output),showWarnings=F,recursive=T)
    opt$output
}
