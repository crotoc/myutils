#' install_my_local
#'
#' Description: Used to quickly install my pakages after modifications. 
#'
#' @param path the path of the package
#' @param pkg the package name
#'
#' @return None
#'
#' @author Rui Chen (crotoc@gmail.com)
#' @examples
#' install_my_local
#'
#' @export
#' @import
#' devtools
#' 
#'
install_my_local <- function(path,pkg){
    setwd(paste(path,pkg,sep="/"))
    devtools::document()
    devtools::install()
    setwd(path)
    library(package=pkg,character.only =T)
}
