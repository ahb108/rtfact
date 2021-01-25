#' @title Calculate the volume of a simple solid of revolution
#'
#' @description Given a left or right-hand half of the interior of a simple (no reversions) vessel shape, calculate the likely capacity if it were swept 360 degres about its middle axis, via the piecewise cylinders methods
#' 
#' 
#' @param intprofile An object of class SpatialPolygons* delineating the outline of the left- or right-hand side of the interior of an object.
#' @param left Is it the left or the right-hand side.
#' 
#' @return A single numeric value for the capacity in the cubed units of the input.
#' @examples
#' data(dolium)
#' plot(dolium, col="grey", border=NA, axes=TRUE)
#' plot(doliumint, border="red", add=TRUE)
#' volsr(doliumint) ##Input units are metres, capacity is 0.128 m3 or 128L
#' @export
#' 
volsr <- function(intprofile, left=TRUE){
    coords <- intprofile@polygons[[1]]@Polygons[[1]]@coords
    x <- coords[,1]
    y <- coords[,2]
    ps <- 0
    for (d in 1:length(y)){
        if (d>1){
            h <- y[d] - (y[d-1])
            if (left){
                dX1 <- max(x) - x[d]
                dX2 <- max(x) - x[d-1]
            } else {
                dX1 <- x[d] - min(x)
                dX2 <- x[d-1] - min(x) 
            }
            ps <- ps + (pi*dX1*dX2*h)
        }
    }
    return(ps)
}

#' @title Find the interior profile of a vessel
#'
#' @description Given a drawn outline of a vessel, find the interior profil only and create a new polygon around the left or right half of the interior cavity. 
#' 
#' @param x A vector of numeric values specifying the approximate start dates for the production of each artefact.
#' @param rimoffset A single numeric value specifying how much (in the input units) to go down from the tow of the vessel rim before creating the internal shape. Default is 0.
#' @param left Logical whether the input and output are the left-hand or right-hand profiles.
#' 
#' @return An object of class SpatialPolygons showing the interior left or right hand cavity of a vessel.
#'
#' #' @examples
#' data(dolium)
#' realheight <- 0.72
#' plot(dolium, col="grey", border=NA, axes=TRUE)
#' doliumint1 <- findInterior(dolium, rimoffset=realheight/10)
#' plot(doliumint1, border="red", add=TRUE)
#' 
#' @export
findInterior <- function(x, rimoffset=0, left=TRUE){
    profile <- x@polygons[[1]]@Polygons[[1]]@coords
    profile <- as.data.frame(profile[1:(nrow(profile)-1),])
    names(profile) <- c("X","Y")
    profile$Top <- (nrow(profile) - rank(profile$Y) +1)/nrow(profile)*100
    profile$Left <- rank(profile$X)/nrow(profile)*100
    profile$Bottom <- rank(profile$Y)/nrow(profile)*100
    profile$Right <- (nrow(profile) - rank(profile$X) +1)/nrow(profile)*100
    profile$TRscore <- profile$X + profile$Y
    profile$Row <- 1:nrow(profile)
    top5 <- profile[profile$Top <= 5,]
    startpt <- profile[row.names(top5[which.max(top5$TRscore),]),c("X","Y")]
    bottom20right5 <- profile[profile$Right <= 5 & profile$Bottom < 20,]
    endpt <- profile[row.names(bottom20right5[which.max(bottom20right5$TRscore),]),c("X","Y")]
    start <- which(row.names(profile)==row.names(startpt))
    end <- which(row.names(profile)==row.names(endpt))
    if (end>start){
        interior <- profile[start:end,]
    } else {
        interior <- rbind(profile[start:nrow(profile),],profile[1:end,])
    }
    interior <- interior[interior$Y <= (max(interior$Y)-rimoffset),]
    interior <- Polygon(cbind(c(interior[,1],interior[nrow(interior),1],interior[1,1]), c(interior[,2],interior[1,2], interior[1,2])))
    interior <- Polygons(list(interior), "1")
    interior <- SpatialPolygons(list(interior), 1:1)
    return(interior)
}
