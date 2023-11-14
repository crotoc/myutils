#' prettyPaginate
#'
#' Create beautiful multiple pages plot from ggplot object.
#'
#' @param myplot a ggplot object
#' @param opt options from optparse
#' opt$type boxploth/pointh or others
#' opt$rowfacet
#' opt$facet_cname
#' opt$ncol
#' opt$nrow
#' opt$scale
#' opt$space
#' opt$x.variable
#' opt$facet_grid  formular
#' 
#' @return a grob list
#'
#' @import
#' ggforce
#' ggplot2
#' 
#' @author Rui Chen (crotoc@gmail.com)
#' @keywords plot
#'
#' @examples
#' out <- prettyPaginate(x_plot,opt)
#'
#' @export
#' 
#' 
#'
prettyPaginate = function (myplot,opt){
    pdf(NULL)
    out  <-  list()

    if(opt$facet){
        if(opt$rowfacet){
            tmp.plot <- myplot   + facet_wrap_paginate(as.formula(paste(opt$facet_cname,"~.","  ")),ncol=opt$ncol,nrow=opt$nrow,scale=opt$scale,drop=opt$drop, page=1)
        }else{
            tmp.plot <- myplot   + facet_wrap_paginate(as.formula(paste(".~",opt$facet_cname,"  ")),ncol=opt$ncol,nrow=opt$nrow,scale=opt$scale,drop=opt$drop,page=1)
        }
        total_pages <- n_pages(tmp.plot)
        
        cat("total pages:",total_pages,"\n")
        for(i in seq_len(total_pages)){
            cat("Total pages: boxploth ",i,"\n")
            if(opt$rowfacet){
                query.plot <- myplot   + facet_wrap_paginate(as.formula(paste(opt$facet_cname,"~.","  ")),ncol=opt$ncol,nrow=opt$nrow,scale=opt$scale,drop=opt$drop, page=i,strip.position = opt$strip_position)
            }else{
                query.plot <- myplot   + facet_wrap_paginate(as.formula(paste(".~",opt$facet_cname,"  ")),ncol=opt$ncol,nrow=opt$nrow,scale=opt$scale,drop=opt$drop,page=i, strip.position = opt$strip_position)
            }
            if(opt$ggobject){
                out[[i]] <- query.plot
            }
            else{
                out[[i]] <- ggplotGrob(query.plot)
            }
        }
    }else if(opt$facet_grid!=0){
        ## calculate how many pages in total
        cat("nrow: ",opt$nrow," ncol: ",opt$ncol,"\n")
        tmp.plot <- myplot   + facet_grid_paginate(as.formula(opt$facet_grid),ncol=opt$ncol,nrow=opt$nrow,scale=opt$scale,space=opt$space,drop=opt$drop, page=1)
        total_pages <- n_pages(tmp.plot)
        ## iterate every page
        cat("total pages:",total_pages,"\n")
        for(i in seq_len(total_pages)){
            if(opt$facet_grid!=0){
                cat(opt$facet_grid,"\n")
                query.plot <- myplot   + facet_grid_paginate(as.formula(opt$facet_grid),ncol=opt$ncol,nrow=opt$nrow,scale=opt$scale,space=opt$space,drop=opt$drop, page=i)
            }
            if(opt$ggobject){
                out[[i]] <- query.plot
            }
            else{
                out[[i]] <- ggplotGrob(query.plot)
            }
        }
    }

    ## Insert space to last page
    ## lastpage_nrow <- ceiling(length(grep("panel",out[[length(out)]]$layout$name))/opt$ncol)
    ## cat("Row in last page: ",lastpage_nrow,"\n")
    ## cat(length(out),"\n")
    ## stripidx <- out[[length(out)]]$layout$t[grep("strip",out[[length(out)]]$layout$name)[1]]

    ## stripsize <- out[[length(out)]]$height[stripidx]

    ## panelspacesize <- out[[length(out)]]$height[1]

    ## cat(lastpage_nrow,stripidx,stripsize,panelspacesize,"\n")
    ## if((opt$nrow-lastpage_nrow)>0){ ## Insert empty row when necessary
    ##    for(i in 1:(opt$nrow-lastpage_nrow)){
    ##        out[[length(out)]] <- gtable_add_rows(out[[length(out)]], stripsize,-1)
    ##        out[[length(out)]] <- gtable_add_rows(out[[length(out)]], unit(1, "null"), -1)
    ##        out[[length(out)]] <- gtable_add_rows(out[[length(out)]], panelspacesize, -1)
    ##    }
    ## }

    dev.off()

    ## To format the last page, I need to insert empty rows to match the total rows specified. In facet object, I need to insert a panel space and a  strip and a empty panel. The info is obtained from gtable.
    ## The following is the understanding of gtable:
    ## 1. After covert ggplot object to ggplotGrob, the z column is the plot order.
    ## 2. cells are the grid where each grob is located in. (1-18, 1-11) means the grob span from grid row 1 to 18 and column 1 to 11.
    ## 3. name are #TODO: he grob names. A typle facet gtable has backgound, panel, (t)op, (b)elow, (l)eft, (r)ight axis, top strip, xlab, ylab, guide-box, subtitle,title, caption.
    ## 4. g$layout contains the layout info of each grob.
    ## 5. g$height and g$width contain the heights the widths of each grid. The number is corresponding size of each grid.
    ## 6. To understand the concept of grid, look at package gird.
    ## 7. To manupulate gtable, refer to package gtable.

    out

}
