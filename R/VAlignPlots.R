#' VAlignPlots
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param plots.list The list of ggplot2
#' @param keepLegends Whether to keep legends
#' @param margin.unit The margins of the plot
#' @param last whehter add margin
#' 
#' @return a grob list
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @seealso AlignPlots
#' @keywords plot
#'
#' @examples
#' groblist <- VAlignPlots(plots.list=list(p.snp,p.highc,p.tx),margin.unit=c(0.1,0,0.1,0),keepLegends = TRUE)
#' 
#'
#' @export
#'
#' 
VAlignPlots <- function(plots.list = plot.list,
                        keepLegends = FALSE,
#                        nb.columns = 1,
                        margin.unit = c(0, 0, 0, 0),
                        last = TRUE
                        ) {
        
    ## Retrieve the list of plots to align
    margin  <-  theme(plot.margin = unit(margin.unit,"lines"), axis.text.x = element_blank(), axis.title.x = element_blank())

    if(!keepLegends){
        plots.list <- lapply(1:(length(plots.list)),function(x){ plots.list[[x]] + theme(legend.position="none") })
    }


    print(length(plots.list))
    plots.list.revise <- list()
    if ( length( plots.list) > 1 )
        plots.list.revise <- lapply(1:(length(plots.list)-1),function(x){ plots.list[[x]] + margin })

    
    if( !last ) {
        plots.list.revise[[length(plots.list)]]  <- plots.list[[length(plots.list)]] + margin 
    } else {
        plots.list.revise[[length(plots.list)]]  <- plots.list[[length(plots.list)]]
    }

    print(length(plots.list.revise))
    plots.list  <- plots.list.revise

    grobs.list <- lapply(plots.list, ggplotGrob)
    
    ## Get the max width
    widths.list <- do.call(grid::unit.pmax, lapply(grobs.list, "[[", 'widths'))

  # Assign the max width to all grobs
  grobs.list <- lapply(grobs.list, function(x) {
    x[['widths']] = widths.list
    x})

    ## Create the gtable and display it
  # An alternative is to use arrangeGrob that will create the table without
  # displaying it
  #g <- do.call(arrangeGrob, c(grobs.list, ncol = nb.columns))

  return(grobs.list)
}


#' g_legend
#'
#' Return a legend from a ggplot
#'
#' @param a.gplot
#' 
#' @return A grob of legend
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @keywords Graphics hplot
#' 
#'
#' @examples
#' g_legend()
#'
#' @export
#' 
#' 
#'
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
