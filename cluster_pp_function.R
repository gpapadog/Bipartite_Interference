#' @param pp_data Data including power plants. Must include columns 'cluster'
#' for cluster id, and 'Fac.Longitude', 'Fac.Latitude' for coordinates.
cluster_pp <- function(pp_data, cluster_id = 1, buffer_meters = 10000) {
  
  cl <- pp_data[cluster == cluster_id][, cluster := NULL]
  
  ##----- Calculate the convex hull of the cluster
  
  # https://stackoverflow.com/questions/25606512/create-convex-hull-polygon-from-points-and-save-as-shapefile
  
  ch <- chull(cl)
  ch_coords <- cl[c(ch, ch[1]), ]  # closed polygon
  
  # plot(cl, pch = 19)
  # lines(ch_coords, col = "red")
  
  # https://stackoverflow.com/questions/25411251/buffer-geospatial-points-in-r-with-gbuffer
  
  ##----- Transform into spatial objects
  
  # https://rstudio.github.io/leaflet/projections.html
  # EPSG:2163 (US National Atlas Equal Area projection)
  pj <- '+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs' # to use meters in buffer
  
  cluster_poly <- SpatialPolygons(list(Polygons(list(Polygon(ch_coords)), ID = cluster_id)))
  proj4string(cluster_poly) <- CRS("+init=epsg:4269") # default CRS NAG83
  cluster_poly <- spTransform(cluster_poly,  CRS(pj)) # reproject to be able to use km
  
  ##----- Create a xx km buffer around the convex hull
  
  # https://gis.stackexchange.com/questions/85300/units-of-gbuffer-from-rgeos-package
  
  cluster_buffer <- gBuffer(cluster_poly, width = buffer_meters, byid = TRUE)
  
  ##----- Load ZIP code data
  
  data("zipcode")
  z <- na.omit(zipcode) # remove entries with missing info
  coordinates(z) = ~longitude + latitude
  proj4string(z) <- CRS("+init=epsg:4269")  # default CRS NAG83
  z <- spTransform(z, CRS(pj)) 
  
  ##----- Calculate the spatial overlay between the buffered convex hull and the zip code centroids
  
  o <- over(cluster_poly, z, TRUE)
  
  ##----- Merge back to get back ZIP code information
  
  linked_zip <- merge(o[[1]], zipcode, by = "zip")
  
  linked_zip_coords <- linked_zip
  coordinates(linked_zip_coords) = ~longitude + latitude
  proj4string(linked_zip_coords) <- CRS("+init=epsg:4269")  # default CRS NAG83
  linked_zip_coords <- spTransform(linked_zip_coords, CRS(pj))
  
  coordinates(ch_coords) = ~ Fac.Longitude + Fac.Latitude
  proj4string(ch_coords) <- CRS("+init=epsg:4269")  # default CRS NAG83
  ch_coords <- spTransform(ch_coords, CRS(pj))
  
  # plot(cluster_buffer)
  # points(m, col = "blue", pch = 3)
  # points(ch_coords, col = "red", pch = 19, cex = 2)
  
  
  return(list(linked_zip = linked_zip, cluster_buffer = cluster_buffer, linked_zip_coords = linked_zip_coords, ch_coords = ch_coords, cl = cl))
  
}

