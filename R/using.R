#' using
#'
#' Install package if not installed. From BiocManager.
#'
#' @param packages names
#' 
#' @return None
#'
#' @importFrom BiocManager install
#' @importFrom devtools install_github
#' @author Rui Chen (crotoc@gmail.com)
#' @examples
#' using("ggplot2")
#'
#' @export
#'
#'

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]

    mypkg <- need[grepl("myutils|iRIGS",need,perl=TRUE)]
    need <- need[!grepl("myutils|iRIGS",need,perl=TRUE)]
    n<-length(need)
    if(n>0){
        libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
        print(libsmsg)
        if(n>1){
            libsmsg<-paste(libsmsg," and ", need[n],sep="")
        }
        print(paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse=""))
        BiocManager::install(need)
        lapply(need,require,character.only=TRUE)
    }

    m <- length(mypkg)
    if(m>0){
        libsmsg<-if(n>2) paste(paste(mypkg[1:(n-1)],collapse=", "),",",sep="") else mypkg[1]
        print(libsmsg)
        if(n>1){
            libsmsg<-paste(libsmsg," and ", mypkg[n],sep="")
        }
        libsmsg<-paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse="")
        mypkg <- paste("crotoc/",mypkg,sep = "")
        print(mypkg)
        lapply(mypkg,FUN=function(x){devtools::install_github(x)})
    }
    
}

