#' @export
retainEU28 <- function(nutsSpdf) {
    nutsSpdf <- nutsSpdf[!grepl("TR.*|MT.*|MK.*|ME.*|CH.*",nutsSpdf@data$NUTS_ID),]

}

#' @importFrom dplyr %>%
#' @export
nutsLevels <- function(year=2013) {
    nutsLevels <- switch(as.character(year),
                        `2010` = nuts2010Codes,
                        `2013` = nuts2013Codes, NULL)
    
                        
    names(nutsLevels)[names(nutsLevels)=="NUTS-Code"] <- "NUTS.Code"
    nutsLevels <- nutsLevels %>% dplyr::select(Level,NUTS.Code,Description)
    nutsLevels.gr <- nutsLevels %>%
        dplyr::filter(grepl("^EL.*",NUTS.Code)) %>%
        tidyr::separate(Description,into=c("Description.countryLang","Description.latin"),sep="[\\(|\\)]",remove = F,extra='drop',fill='right') %>%
        dplyr::mutate(Description.countryLang=stringr::str_trim(Description.countryLang),
                      Description.latin=stringr::str_trim(Description.latin))
    
    nutsLevels.noGR <- nutsLevels  %>%
        dplyr::filter(!grepl("^EL.*",NUTS.Code)) %>%
        dplyr::mutate(Description.countryLang=Description,
                      Description.latin=Description)
    dplyr::bind_rows(nutsLevels.gr,nutsLevels.noGR)
    
}
 #' @export
getNuts3Areas <-  function() {
    EU_NUTS.3 <- NUTS_01M_2013[NUTS_01M_2013@data$STAT_LEVL_==3,]

    nuts.data <- dplyr::data_frame(NUTS.Code=EU_NUTS.3@data$NUTS_ID,
                                  id=row.names(EU_NUTS.3@data))
    polygon_data <- dplyr::data_frame(id=sapply(slot(EU_NUTS.3, "polygons"), slot, "ID"),
                                     Shape_Area= sapply(slot(EU_NUTS.3, "polygons"), slot, "area"))
    nuts.area <- dplyr::left_join(nuts.data,polygon_data)
    nuts.area
}

