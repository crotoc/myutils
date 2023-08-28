#' addArg
#'
#' Description: An wrapper function that add options for multiple categories
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
#' parser <- addArg(parser,"General")
#' @import
#' argparse
#' @export

addArg <- function(parser,cat = c("General","Plot")){
    fun <- paste0("addArg",cat)
    for(eachfun in fun){
        if(exists(eachfun,mode = "function")){
            tempfun <- match.fun(FUN=eachfun)
            parser <- tempfun(parser)
        }
    }
    return(parser)
}

#' addArgGeneral 
#'
#' Description: Add options for general purpose through argparse package
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
#' parser <- ArgumentParser()
#' parser <- addArgGeneral(parser)
#'
#' @export
#' 

addArgGeneral <- function(parser){
    parser$add_argument("-i","--input", default="1",dest="input",help = "input is [default \"%(default)s\"]")

    parser$add_argument("--input2", default="2",dest="input2",help = "input2 is [default \"%(default)s\"]")

    parser$add_argument("--input3", default="3",dest="input3",help = "input3 is [default \"%(default)s\"]")


    parser$add_argument("--input4", default="4",dest="input4",help = "input4 is [default \"%(default)s\"]")

    parser$add_argument("--step", default="1",dest="step",help = "step is [default \"%(default)s\"]")

    parser$add_argument("-o","--output", default="1",dest="output",help = "output is [default \"%(default)s\"]")

    parser$add_argument("--dir_out", default="out",dest="dir_out",help = "dir_out is [default \"%(default)s\"]")

    parser$add_argument("--header", action="store_true", default=FALSE,help="--header  default is [default %(defaults)s]")

    parser$add_argument("-n", "--number", type="integer", default=1, help="number [default %(default)s]")

    parser$add_argument("--example", action="store_true", default=FALSE,help="--example  default is [default %(defaults)s]")


    parser$add_argument("--save_RData", action="store_true", default=FALSE,help="--save_RData  default is [default %(defaults)s]")


    parser$add_argument("--verbose", action="store_false", default=FALSE,help="--verbose  default is [default %(defaults)s]")

    parser$add_argument("--progress_bar", action="store_true", default=FALSE,help="--progress_bar  default is [default %(defaults)s]")

    parser$add_argument("--digits", type="integer", default=3, help="digits [default %(default)s]")

    parser$add_argument("--threads", type="integer", default=5, help="threads [default %(default)s]")


    parser$add_argument("--rds", default="1",dest="rds",help = "rds is [default \"%(default)s\"]")

    parser$add_argument("--threshold", default=1, type="double", dest="threshold", help="threshold is  [default %(default)s]")

    parser$add_argument("--condaEnv", default="/fs0/chenr6/chenr6/opt/condaEnv/AccreLibEnv",dest="condaEnv",help = "condaEnv is [default \"%(default)s\"]")

    parser$add_argument("--eval", action="store_true", default=FALSE,help="--eval  default is [default %(defaults)s]")

    parser$add_argument("--opt", default="1",dest="opt",help = "opt is [default \"%(default)s\"]")
    parser$add_argument("--yaml", default="1",dest="yaml",help = "yaml is [default \"%(default)s\"]")
    parser$add_argument("--test", action="store_true", default=FALSE,help="--test  default is [default %(defaults)s]")
    parser$add_argument("--ext", default="pdf",dest = "ext", help="--ext Output file [default: %default]")
    parser$add_argument("--readr", action="store_false", default=FALSE,help="--readr  default is [default %(defaults)s]")
    return(parser)
}

#' addArgGwas
#'
#' Description: Add options related to GWAS file
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
#' parser <- addArgGwas(parser)
#'
#' @export


addArgGwas <- function(parser){
    parser$add_argument("--gwas", default="~/chenr6/gibbs/withoutDNM/out/SCZ_new_98_loci.bed",dest = "gwas", help="--gwas  [default: %default]")
    parser$add_argument("--col_gwas_chr", type="integer", default=2, help="--col_gwas_chr column of gwas chr [default: %default]")
    parser$add_argument("--col_gwas_start", type="integer", default=3, help="--col_gwas_start start position of gwas loci [default: %default]")
    parser$add_argument("--col_gwas_end", type="integer", default=3, help="--col_gwas_end end position of gwas loci [default: %default]")
    parser$add_argument("--col_gwas_snp", type="integer", default=1, help="--col_gwas_snp snp id of gwas loci [default: %default]")
    parser$add_argument("--col_gwas_p", type="integer", default=4, help="--col_gwas_p snp id of gwas loci [default: %default]")

    return(parser)
}

#' addArgPlot
#'
#' Description: Add options related to ggplot2 ploting
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
#' parser <- addArgPlot(parser)
#'
#' @export

addArgPlot <- function(parser){
    parser$add_argument("--grp", type="integer", default=0, help="--grp  grp col [default: %default]")
    parser$add_argument("--x","-x", type="integer", default=1, help="--x number options [default: %default]")
    parser$add_argument("--y","-y", type="integer", default=2, help="--y number options [default: %default]")
    parser$add_argument("--value", type="integer", default=12, help="--value  value col [default: %default]")
    parser$add_argument("--table", action="store_false", default=TRUE, help="--table Output file [default: %default]")
    parser$add_argument("--returnObject", action="store_true", default=FALSE, help="--returnObject  [default: %default]")
    parser$add_argument("--ggobject", action="store_true", default=FALSE, help="--ggobject  [default: %default]")
    parser$add_argument("--plot2file", action="store_false", default=TRUE, help="--plot2file  [default: %default]")
    parser$add_argument("--strip_position", default="top",dest = "strip_position", help="--strip_position  [default: %default]")
    parser$add_argument("--scale_x_log2", default=0,dest = "scale_x_log2", help="--scale_x_log2  [default: %default]")
    parser$add_argument("--scale_y_log2", default=0,dest = "scale_y_log2", help="--scale_y_log2  [default: %default]")
    parser$add_argument("--facetsort", default="increasing",dest = "facetsort", help="--facetsort  [default: %default]")
    parser$add_argument("--xsort", default="increasing",dest = "xsort", help="--xsort  [default: %default]")
    parser$add_argument("--strip_text_y_angle", default=90, type="double",dest = "strip_text_y_angle", help="--strip_text_y_angle  [default: %default]")
    parser$add_argument("--rowfacet", action="store_false", default=TRUE, help="--rowfacet  [default: %default]")
    parser$add_argument("--annotation", type="integer", default=1, help="--annotation  [default: %default]")
    parser$add_argument("--xintercept", default="NULL",dest = "xintercept", help="--xintercept Horizonal line [default: %default]")
    parser$add_argument("--xintercept_x", type="integer", default=1, help="xintercept_x [default %(default)s]")
    parser$add_argument("--xintercept_value", type="integer", default=2, help="xintercept_value [default %(default)s]")
    parser$add_argument("--yintercept", default="NULL",dest = "yintercept", help="--yintercept Vertical line [default: %default]")
    parser$add_argument("--yintercept_x", type="integer", default=1, help="yintercept_x [default %(default)s]")
    parser$add_argument("--yintercept_value", type="integer", default=2, help="yintercept_value [default %(default)s]")
    parser$add_argument("--abline", action="store_true", default=FALSE, help="--abline  [default: %default]")
    parser$add_argument("--slope", default=0, type="double",dest = "slope", help="--slope  [default: %default]")
    parser$add_argument("--intercept", default=0, type="double",dest = "intercept", help="--intercept  [default: %default]")
    parser$add_argument("--observation", default="NULL",dest = "observation", help="--observation  [default: %default]")
    parser$add_argument("--observation_x", type="integer", default=1, help="observation_x [default %(default)s]")
    parser$add_argument("--observation_value", type="integer", default=2, help="observation_value [default %(default)s]")
    parser$add_argument("--fontsize", type="integer", default=2, help="--fontsize  [default: %default]")
    parser$add_argument("--fontfamily", default="Times",dest = "fontfamily", help="--fontfamily  [default: %default]")
    parser$add_argument("--pointshape", type="integer", default=21, help="--pointshape  [default: %default]")
    parser$add_argument("--pointsize", type="integer", default=2, help="--pointsize  [default: %default]")
    parser$add_argument("--lollipop", action="store_true", default=FALSE, help="--lollipop  [default: %default]")
    parser$add_argument("--wrap", action="store_false", default=TRUE, help="--wrap TRUE is facet_wrap and FALSE is facet_grid [default: %default]")
    parser$add_argument("--ignore_col", default="NULL",dest = "ignore_col", help="--ignore_col  [default: %default]")
    parser$add_argument("--mark_col", default="NULL",dest = "mark_col", help="--mark_col Output file [default: %default]")
    parser$add_argument("--mark_text_col", type="integer", default=0, help="--mark_text_col text label col [default: %default]")
    parser$add_argument("--legend_position", default="bottom",dest = "legend_position", help="--legend_position bottom, top, left, right, NULL [default: %default]")
    parser$add_argument("--legend_direction", default="vertical",dest = "legend_direction", help="--legend_direction legend direction: vertical, horizontal [default: %default]")
    parser$add_argument("--legend_ncol", type="integer", default=1, help="--legend_ncol  [default: %default]")
    parser$add_argument("--legend_nrow", type="integer", default=3, help="--legend_nrow  [default: %default]")
    parser$add_argument("--color", type="integer", default=0, help="--color  [default: %default]")
    parser$add_argument("--bin", type="integer", default=30, help="--bin number options [default: %default]")
    parser$add_argument("--vertical_line", default="NULL",dest = "vertical_line", help="--vertical_line  [default: %default]")
    parser$add_argument("--mark_tissue", default="NULL",dest = "mark_tissue", help="--mark_tissue Output file [default: %default]")
    parser$add_argument("--mark_panel", default="NULL",dest = "mark_panel", help="--mark_panel Output file [default: %default]")
    parser$add_argument("--ncol", type="integer", default=10, help="--ncol number options [default: %default]")
    parser$add_argument("--nrow", type="integer", default=3, help="--nrow number options [default: %default]")
    parser$add_argument("--jitter", action="store_true", default=FALSE, help="--jitter number options [default: %default]")
    parser$add_argument("--quantile", default=0.05, type="double",dest = "quantile", help="--quantile number options [default: %default]")
    parser$add_argument("--type", default="boxplot",dest = "type", help="--type Output file [default: %default]")
    parser$add_argument("--facet", type="integer", default=0, help="--facet Output file [default: %default]")
    parser$add_argument("--facet_grid", default=0,dest = "facet_grid", help="--facet_grid Output file [default: %default]")
    parser$add_argument("--scale", default="fixed",dest = "scale", help="--scale fixed free free_x free_y [default: %default]")
    parser$add_argument("--space", default="fixed",dest = "space", help="--space fixed free free_x free_y [default: %default]")
    parser$add_argument("--drop", action="store_true", default=FALSE, help="--drop  [default: %default]")
    parser$add_argument("--pal", default="Set2",dest = "pal", help="--pal  [default: %default]")
    parser$add_argument("--col_point", type="integer", default=1, help="--col_point  [default: %default]")
    parser$add_argument("--col_error", type="integer", default=2, help="--col_error  [default: %default]")
    parser$add_argument("--col_perror", type="integer", default=2, help="--col_perror  [default: %default]")
    parser$add_argument("--col_nerror", type="integer", default=2, help="--col_nerror  [default: %default]")
    parser$add_argument("--ycol", type="integer", default=1, help="--ycol  [default: %default]")
    parser$add_argument("--xcol", type="integer", default=1, help="--xcol  [default: %default]")
    parser$add_argument("--ylim", default="NULL",dest="ylim",help = "ylim is [default \"%(default)s\"]")
    parser$add_argument("--xlim", default="NULL",dest="xlim",help = "xlim is [default \"%(default)s\"]")
    parser$add_argument("--alpha", default=0.5, type="double", dest="alpha", help="alpha is  [default %(default)s]")
    return(parser)
}

#' addArgRmd
#'
#' Description: Add options related to runRmd.R script
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
#' parser <- addArgRmd(parser)
#'
#' @export

addArgRmd <- function(parser){
    parser$add_argument("--rmd", default="1",dest="rmd",help = "rmd is [default \"%(default)s\"]")

    parser$add_argument("--asRmdPart", action="store_false",dest="asRmdPart", default=TRUE,help="asRmdPart is [default %(default)s]")

    parser$add_argument("--clean", action="store_false",dest="clean", default=TRUE,help="clean is [default %(default)s]")
    return(parser)
}

#' addArgScenic
#'
#' Description: Add options related to run SCENIC
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
#' parser <- addArgScenic(parser)
#'
#' @export

addArgScenic <- function(parser){
    parser$add_argument("--org", default="hgnc",dest="org",help = "org is [default \"%(default)s\"]")

    parser$add_argument("--dbDir", default="/fs0/chenr6/Database_fs0/SCENIC/",dest="dbDir",help = "dbDir is [default \"%(default)s\"]")
    return(parser)
}


#' addArgSinglecell
#'
#' Description: Add options related to single cell analysis using Seurat
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
#' parser <- addArgSinglecell(parser)
#'
#' @export
#' 
addArgSinglecell <- function(parser){
    parser$add_argument("--objectname", default="mca",dest = "objectname", help="--objectname  [default: %default]")
    parser$add_argument("--nFeature_RNA", default=500, type="double",dest = "nFeature_RNA", help="--nFeature_RNA  [default: %default]")
    parser$add_argument("--nCount_RNA", default=500, type="double",dest = "nCount_RNA", help="--nCount_RNA  [default: %default]")
    parser$add_argument("--percent.mt", default=5, type="double",dest = "percent.mt", help="--percent.mt  [default: %default]")
    return(parser)
}

#' addArgSnp
#'
#' Description: Add options reltead to snp file parsing
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
#' parser <- addArgSnp(parser)
#'
#' @export

addArgSnp <- function(parser){
    parser$add_argument("--col_chr", type="integer", default=2, help="--col_chr  [default: %default]")
    parser$add_argument("--col_pos", type="integer", default=3, help="--col_pos  [default: %default]")
    parser$add_argument("--col_snp", type="integer", default=1, help="--col_snp  [default: %default]")
    parser$add_argument("--maf", default=0.1, type="double",dest = "maf", help="--maf +/- maf [default: %default]")

    return(parser)
}

#' addArgSumstas
#'
#' Description: Add options related to summary statistics of GWAS
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
#' parser <- addArgSumstas(parser)
#'
#' @export

addArgSumstats <- function(parser){
    parser$add_argument("--summary_mode", default="scz",dest = "summary_mode", help="--summary_mode  [default: %default]")
    parser$add_argument("--summary_stats", default="/gpfs23/scratch/cgg/chenr6/Database/PGC/scz2/scz2.snp.results.txt.RData",dest = "summary_stats", help="--summary_stats  [default: %default]")
    parser$add_argument("--col_sum_chr", type="integer", default=1, help="--col_sum_chr chr col of summary [default: %default]")
    parser$add_argument("--col_sum_start", type="integer", default=5, help="--col_sum_start start pos of summary [default: %default]")
    parser$add_argument("--col_sum_end", type="integer", default=5, help="--col_sum_end end pos of summary [default: %default]")
    parser$add_argument("--col_sum_p", type="integer", default=9, help="--col_sum_p p value of summary [default: %default]")
    parser$add_argument("--col_sum_snp", type="integer", default=2, help="--col_sum_snp p value of summary [default: %default]")

    return(parser)
}


#' addArgIrigs
#'
#' Description: add options related to iRIGS
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
#' parser <- addArgIrigs(parser)
#'
#' @export

addArgIrigs <- function(parser){
    parser$add_argument("--gibbsrank", default="/gpfs23/scratch/cgg/chenr6/gibbs/6feature/out/SCZ_gibbs_sampling_go_rp_0.3_frequency.fmt.rank.pprank_disrank",dest = "gibbsrank", help="--gibbsrank  [default: %default]")
    parser$add_argument("--col_gibbsrank_snp", type="integer", default=3, help="--col_gibbsrank_snp col of snp id of gibbs result [default: %default]")
    parser$add_argument("--col_gibbsrank_gene", type="integer", default=1, help="--col_gibbsrank_gene gene col of gibbs result [default: %default]")
    parser$add_argument("--col_gibbsrank_p", type="integer", default=2, help="--col_gibbsrank_p p col of gibbs result [default: %default]")
    parser$add_argument("--col_gibbsrank_ensembl", type="integer", default=15, help="--col_gibbsrank_ensembl ensembl id of gibbs result [default: %default]")
    return(parser)
}


#' addArgHighc
#'
#' Description: add HighC related options
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
#' addArgHighc(parser)
#'
#' @export

addArgHighc <- function(parser){
    parser$add_argument("--highc_mode", default="NULL",dest = "highc_mode", help="--highc_mode  [default: %default]")
    parser$add_argument("--highc", default="~/chenr6/gibbs/HiC/scz_cp_pooled.txt",dest = "highc", help="--highc Input file [default: %default]")
    parser$add_argument("--col_bait_chr", type="integer", default=1, help="--col_bait_chr Chromosome [default: %default]")
    parser$add_argument("--col_bait_start", type="integer", default=2, help="--col_bait_start The bait start position [default: %default]")
    parser$add_argument("--col_bait_end", type="integer", default=3, help="--col_bait_end The bait end position [default: %default]")
    parser$add_argument("--col_region_chr", type="integer", default=1, help="--col_region_chr Chromosome [default: %default]")
    parser$add_argument("--col_region_start", type="integer", default=4, help="--col_region_start Start position of interacting region [default: %default]")
    parser$add_argument("--col_region_end", type="integer", default=5, help="--col_region_end End position of interacting region [default: %default]")
    parser$add_argument("--col_bait_p", type="integer", default=6, help="--col_bait_p  p value of the interaction[default: %default]")
    parser$add_argument("--col_bait_ensembl", type="integer", default=7, help="--col_bait_ensembl  [default: %default]")
    parser$add_argument("--col_bait_gene", type="integer", default=8, help="--col_bait_gene  [default: %default]")
    return(parser)
}


#' addArgEnhancer
#'
#' Description: Add Enhancer related options
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
#' addArgEnhancer(parser)
#'
#' @export
#' 
addArgEnhancer <- function(parser){
    parser$add_argument("--col_enh_chr", type="integer", default=1, help="--col_enh_chr Chromosome [default: %default]")
    parser$add_argument("--col_enh_start", type="integer", default=2, help="--col_enh_start The bait start position [default: %default]")
    parser$add_argument("--col_enh_end", type="integer", default=3, help="--col_enh_end The bait end position [default: %default]")
    parser$add_argument("--col_enh_p", type="integer", default=6, help="--col_enh_p correlation of ehn and tss [default: %default]")
    parser$add_argument("--col_tss_chr", type="integer", default=1, help="--col_tss_chr Chromosome [default: %default]")
    parser$add_argument("--col_tss_start", type="integer", default=4, help="--col_tss_start Start position of interacting region [default: %default]")
    parser$add_argument("--col_tss_end", type="integer", default=5, help="--col_tss_end End position of interacting region [default: %default]")
    return(parser)
}



#' addArgHeatmap
#'
#' Description: add heatmap options
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
#' addArgHeatmap(parser)
#'
#' @export
#' 
addArgHeatmap <- function(parser){
    parser$add_argument("--row_cluster", default="cor",dest = "row_cluster", help="--row_cluster  [default: %default]")
    parser$add_argument("--col_cluster", default="cor",dest = "col_cluster", help="--col_cluster  [default: %default]")
    parser$add_argument("--border", default="none",dest = "border", help="--border  [default: %default]")
    parser$add_argument("--valuename", default="scaled",dest = "valuename", help="--valuename  [default: %default]")
    parser$add_argument("--scaled", default="scalemax",dest = "scaled", help="--scaled  [default: %default]")
    parser$add_argument("--preprocessing", default="none",dest = "preprocessing", help="--preprocessing  [default: %default]")
    parser$add_argument("--row_k", type="integer", default=5, help="--row_k  [default: %default]")
    parser$add_argument("--col_k", type="integer", default=2, help="--col_k  [default: %default]")
    parser$add_argument("--midpoint", default="median",dest = "midpoint", help="--midpoint  [default: %default]")
    parser$add_argument("--rowmeta", default="NULL",dest = "rowmeta", help="--rowmeta  [default: %default]")
    parser$add_argument("--rowmeta_header", action="store_false", default=TRUE, help="--rowmeta_header  [default: %default]")
    parser$add_argument("--rowmeta_cols", default="NULL",dest = "rowmeta_cols", help="--rowmeta_cols  [default: %default]")
    parser$add_argument("--rowmeta_sort_by", type="integer", default=0, help="--rowmeta_sort_by  [default: %default]")
    parser$add_argument("--rowmeta_ycol", type="integer", default=1, help="--rowmeta_ycol  [default: %default]")
    parser$add_argument("--rowmeta_width", default=1, type="double",dest = "rowmeta_width", help="--rowmeta_width  [default: %default]")
    parser$add_argument("--rowmeta_clustercol", default=0, type="double",dest = "rowmeta_clustercol", help="--rowmeta_clustercol  [default: %default]")
    parser$add_argument("--rowmetatext", default="NULL",dest = "rowmetatext", help="--rowmetatext  [default: %default]")
    parser$add_argument("--rowmetatext_header", action="store_false", default=TRUE, help="--rowmetatext_header  [default: %default]")
    parser$add_argument("--rowmetatext_cols", default="NULL",dest = "rowmetatext_cols", help="--rowmetatext_cols  [default: %default]")
    parser$add_argument("--rowmetatext_sort_by", type="integer", default=0, help="--rowmetatext_sort_by  [default: %default]")
    parser$add_argument("--rowmetatext_ycol", type="integer", default=1, help="--rowmetatext_ycol  [default: %default]")
    parser$add_argument("--rowmetatext_fillcol", type="integer", default=0, help="--rowmetatext_fillcol  [default: %default]")
    parser$add_argument("--rowmetatext_width", default=2, type="double",dest = "rowmetatext_width", help="--rowmetatext_width  [default: %default]")
    parser$add_argument("--rowmetatext_legend_nrow", type="integer", default=0, help="--rowmetatext_legend_nrow  [default: %default]")
    parser$add_argument("--rowmetatext_legend_ncol", type="integer", default=0, help="--rowmetatext_legend_ncol  [default: %default]")
    parser$add_argument("--colmeta", default="NULL",dest = "colmeta", help="--colmeta  [default: %default]")
    parser$add_argument("--colmeta_header", action="store_false", default=TRUE, help="--colmeta_header  [default: %default]")
    parser$add_argument("--colmeta_cols", default="NULL",dest = "colmeta_cols", help="--colmeta_cols  [default: %default]")
    parser$add_argument("--colmeta_sort_by", type="integer", default=0, help="--colmeta_sort_by  [default: %default]")
    parser$add_argument("--colmeta_ycol", type="integer", default=1, help="--colmeta_ycol  [default: %default]")
    parser$add_argument("--colmeta_grp", type="integer", default=2, help="--colmeta_grp  [default: %default]")
    parser$add_argument("--row_dd_width", type="integer", default=10, help="--row_dd_width  [default: %default]")
    parser$add_argument("--row_key_width", type="integer", default=1, help="--row_key_width  [default: %default]")
    parser$add_argument("--heatmap_width", type="integer", default=20, help="--heatmap_width  [default: %default]")
    parser$add_argument("--col_dd_height", type="integer", default=10, help="--col_dd_height  [default: %default]")
    parser$add_argument("--col_key_height", type="integer", default=1, help="--col_key_height  [default: %default]")
    parser$add_argument("--heatmap_height", type="integer", default=20, help="--heatmap_height  [default: %default]")
    parser$add_argument("--clusterplot", action="store_true", default=FALSE, help="--clusterplot  [default: %default]")
    parser$add_argument("--gmtfile", default="/fs0/chenr6/Database_fs0/GO/is_a.partof.regulates/fmt/go.biological_process",dest = "gmtfile", help="--gmtfile  [default: %default]")

    return(parser)
}
