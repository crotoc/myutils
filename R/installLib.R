#' installLib
#'
#' Description: install all libs that used in my R script
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
#' installLib(type=c("all"))
#' installLib(type=c("general","plot"))
#'
#' @export
#' @import
#' devtools
#' @importFrom BiocManager install
#' 
#'

installLib <-  function(type){
    if(length(grep("all",type))>0){
        type <- c("general","plot","iRIGS","analysis")
    }

    libname <- c()
    if(length(grep("general",type))>0){
        libname <-c(libname,
                    "data.table",
                    "dplyr",
                    "extrafont",
                    "magrittr",
                    "optparse",
                    "reshape2"
                    )
    }
    
    if(length(grep("plot",type))>0){
        libname <-c(libname,
                    "ggplot2",
                    "GGally",
                    "ggdendro",
                    "dendextend",
                    "ggforce",
                    "ggnewscale",
                    "ggpubr",
                    "ggrepel",
                    "ggstance",
                    "gplots",
                    "graph",
                    "grid",
                    "gridExtra",
                    "gtools",
                    "igraph",
                    "RColorBrewer",
                    "Rgraphviz",
                    "rtracklayer",
                    "superheat",
                    "VennDiagram",
                    "network"
        )
    }
    
    if(length(grep("iRIGS",type))>0){
        devtools::install_github("crotoc/iRIGS")
    }

    if(length(grep("analysis",type))>0){
        libname <- c(libname,
                     "DESeq2",
                     "EnsDb.Hsapiens.v75",
                     "GenomicRanges",
                     "GO.db",
                     "org.Hs.eg.db",
                     "pathview",
                     "scater",
                     "scran",
                     "TxDb.Hsapiens.UCSC.hg19.knownGene",
                     "cmapR",
                     "ggbio",
                     "WGCNA",
                     "Homo.sapiens",
                     "clusterProfiler",
                     "biovizBase",
                     "DOSE",
                     "limma"
                     )
    }
    using(libname)
}


