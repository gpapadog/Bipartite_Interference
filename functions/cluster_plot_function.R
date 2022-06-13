
##----- Interactive visualization with leaflet

cluster_plot <- function(l, icon_path = NULL) {
  
  # https://stackoverflow.com/questions/33045388/projecting-my-shapefile-data-on-leaflet-map-using-r
  
  shiny_cluster_buffer <- spTransform(l$cluster_buffer, CRS("+proj=longlat +ellps=GRS80"))
  shiny_ch_coords <- spTransform(l$ch_coords, CRS("+proj=longlat +ellps=GRS80"))
  shiny_linked_zip <- spTransform(l$linked_zip_coords, CRS("+proj=longlat +ellps=GRS80"))
  
  coordinates(l$cl) = ~Fac.Longitude + Fac.Latitude
  proj4string(l$cl) <- CRS("+init=epsg:4269")
  shiny_cl <- spTransform(l$cl, CRS("+proj=longlat +ellps=GRS80"))
  
  ppIcon <- iconList(
    pp = makeIcon(paste0(icon_path, "pp.png"), iconWidth = 36, iconHeight = 36)
  )
  
  cluster_centroid <- gCentroid(shiny_cluster_buffer) # get buffer centroid to center map display
  
  leaflet()  %>% addTiles() %>%
    setView(lng = cluster_centroid@coords[, 1],
            lat = cluster_centroid@coords[, 2],
            zoom = 7) %>%
    addPolygons(data = shiny_cluster_buffer, weight = 5, col = "red") %>%
    addMarkers(lng = shiny_ch_coords@coords[, 1],
               lat = shiny_ch_coords@coords[, 2],
               popup = "Convex hull") %>%
    addCircleMarkers(lng = shiny_linked_zip@coords[, 1],
                     lat = shiny_linked_zip@coords[, 2],
                     radius = 5) %>%
    addMarkers(lng = shiny_cl@coords[, 1],
               lat = shiny_cl@coords[, 2], icon = ppIcon)
}
