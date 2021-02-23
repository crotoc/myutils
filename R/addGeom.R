#' addGeom
#'
#' add geom on the existing plot
#'
#' @param no param need.
#' 
#' @return None
#'
#' @author Rui Chen (crotoc@gmail.com)
#'
#' @examples
#' addGeom(x_plot,type,opt)
#'
#' 
#' @export
#' 


addGeom <- function(x_plot,type,opt,...){
    parms=list(...)
    if(type == "xintercept"){
        linetype <- match.arg(linetype)
        if(is.null(linetype)){
            linetype = "dashed"
        }
        alpha <- match.arg(alpha)
        if(is.null(linetype)){
            alpha = 0.6
        }
        color <- match.arg(colar)
        if(is.null(color)){
            color = "black"
        }
        #x_plot <- x_plot + geom_vline(xintercept=opt[[type]],color="black",linetype = linetype ,alpha = alpha)
    }
}
