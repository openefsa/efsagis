#' @export
postProcessMap <- function(map,crs=NULL,extent=NULL) {
    if(!is.null(crs))
        map <- sp::spTransform(map,crs)
    if (!is.null(extent))
        map <- raster::crop(map,extent)
    map
}


#' @export
worldCountries_layer <- function(spdf=countries_01M_2013,
                                crs= sp::CRS("+proj=longlat +ellps=WGS84"),
                                extent=NULL,
                                alpha=1,fill.col="#E3DEBF",borders.lwd=0.1,borders.col="grey10") {
    spdf <- postProcessMap(spdf,crs,extent)
    tmap::tm_shape(spdf) +
        tmap::tm_fill(col = fill.col,alpha=alpha) +
        tmap::tm_borders(lwd=borders.lwd,col=borders.col,alpha=alpha)
    
    
}

#' @export
cgms25grid_layer <- function(spdf=cgms25grid,
                            crs= sp::CRS("+proj=longlat +ellps=WGS84"),
                            extent=NULL,
                            alpha=1) {
    grid25 <-  postProcessMap(spdf,crs,extent)
    tmap::tm_shape(grid25) +
        tmap::tm_polygons(alpha = alpha)
    
}
