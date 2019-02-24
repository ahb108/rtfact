#' @title Find the interior profile of a vessel
#'
#' @description Given a drawn outline of a vessel, find the interior profil only and create a new polygon around the left or right half of the interior cavity. 
#' 
#' @param x An object of class SpatialPolygons* delineating the profile of a vessel.
#' @param rimoffset A single numeric value specifying how much (in the input units) to go down from the top of the vessel rim before creating the internal shape. Default is 0.
#' @param left Is it the left or the right-hand side profile.
#' @param interior Logical for whether to plot the interior cavity as well.
#' @param col.interior Colour of the interior
#' @param col.exterior Colour of the exterior
#' @param ... passed to par3d().
#'
#' @import rgl 
#' @export
#'
plot3dSR <- function(x, rimoffset=0, left=TRUE, interior=TRUE, col.interior="red", col.exterior="darkgoldenrod3", windowRect=c(20, 30, 800, 800), userMatrix=rotationMatrix(180/pi*50, 1, 1, 0), ...){
    bodyxy <- x@polygons[[1]]@Polygons[[1]]@coords
    x1 <- bodyxy[,2]
    y1 <- bodyxy[,1]
    y1 <- abs(y1-max(y1))
    mod <- turn3d(x1, y1, n=36, smooth=TRUE)
    mod <- rotate3d(mod,pi/2,0,1,0)
    int <- findInterior(x, rimoffset=rimoffset)
    intxy <- int@polygons[[1]]@Polygons[[1]]@coords
    x1 <- intxy[,2]
    y1 <- intxy[,1]
    y1 <- abs(y1-max(y1))
    intmod <- turn3d(x1, y1, n=36, smooth=TRUE)
    intmod <- rotate3d(intmod,pi/2,0,1,0)
    par3d(windowRect=windowRect, userMatrix=userMatrix)
    shade3d(mod, col=col.exterior)
    shade3d(intmod, col=col.interior)
}
