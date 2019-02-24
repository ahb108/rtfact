
#' @title Convert BP dates to BC/AD format 
#' @description Converts calibrated BP dates to BC/AD dates, omitting 'year 0' 
#' @param x A numerical vector (currently no checks that these numbers are in a sensible range). 
#' @return A vector with BC/BCE dates expressed as negative numbers and AD/CE dates as positive ones.
#' @examples
#' BPtoBCAD(4200)
#' @export
BPtoBCAD <- function(x){
    res <- matrix(c(x, rep(NA,length(x))), ncol=2)
    res[x < 1950,2] <- 1950-res[x < 1950,1]
    res[x >= 1950,2] <- 1949-res[x >= 1950,1]
    return(res[,2])
}

#' @title Convert BC/AD dates to BP format
#' @description Converts BC/AD dates to BP fromat while handling the absence of 'year 0' 
#' @param x A numerical vector (currently no checks that these numbers are in a sensible range).
#' @return A vector with BC/BCE dates expressed as negative numbers and AD/CE dates as positive ones.
#' @examples
#' BCADtoBP(-1268)
#' @export
BCADtoBP <- function(x){
    res <- matrix(c(x, rep(NA,length(x))), ncol=2)
    res[x > 0,2] <- abs(res[x > 0,1] - 1950)
    res[x < 0,2] <- abs(res[x < 0,1] - 1949)
    return(res[,2])
}
