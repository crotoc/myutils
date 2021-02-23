#' opendev
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param opt opt list from optparse
#' opt$ext pdf, jpeg, any device
#' opt$width
#' opt$height
#' opt$res resolution
#' opt$output output name
#' 
#' @return None
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @keywords plot
#'
#' @examples
#' opendev()
#'
#' @export
#' 

opendev <- function(opt){
    if(is.null(opt$ext) || is.na(opt$ext))
        opt$ext <- "pdf"
    
    if (is.null(opt$output) || is.na(opt$output) )
        opt$output <- "out"
    
    if (is.null(opt$width) || is.na(opt$width) )
        opt$width <- 8
    
    if (is.null(opt$height) || is.na(opt$height))
        opt$height <- 8


    if (is.null(opt$res) || is.na(opt$res))
        opt$res <- 300
    
    dev <- sprintf("%s(file=\"%s.%s\",width=%s,height=%s",opt$ext,opt$output,opt$ext,opt$width,opt$height)
    if(opt$ext != "pdf"){
        if(is.na(opt$res))
            stop("wrong options:ext")
        dev=sprintf("%s,units=\"in\",res=opt$res",dev,opt$res)
    }
    dev <- paste(dev,",bg = \"white\")",sep="")
    #print(dev)
    eval(parse(text=dev))
    return(dev)
}
