#' Spatial index distance matrix
#' 
#' Function acquired from https://github.com/czigler/arepa
#'
#' @param \code{data1} Dataset 1
#' @param \code{lat1} Latitude variable in dataset 1
#' @param \code{lon1} Longitude variable in dataset 1
#' @param \code{id1} Unique key in dataset 1
#' @param \code{data2} Dataset 1
#' @param \code{lat2} Latitude variable in dataset 2
#' @param \code{lon2} Longitude variable in dataset 2
#' @param \code{id2} Unique key in dataset 2
#' @param \code{within} Link \code{within} kilometers
#' @param \code{closest} Closest within Boolean (unique key is id2, so typically zip code)
#' 
#' @return This function returns a data table of pairwise distances \code{within}
#' kilometers with column names \code{id1}, \code{id2} and \code{Distance}.
#' 
#' @examples
#' years <- 2000:2002 # possible range 1990-2018
#' within_km <- 9.656 # 6 miles
#' parameter_code <- 81102 # for PM 10
#' ##----- Get PM10 annual data and set unique Monitor key
#' # Download AQS data in Data_AQS/all.
#' # get_AQS_data_annual(years)
#' PM <- load_annual_average(years)
#' PM <- subset(PM, Parameter.Code == parameter_code)
#' ##----- Zip codes
#' ZIP <- get_zip_codes()
#' ##----- Spatial index to join monitors and Zip codes.  May take a couple of minutes
#' Link_PM_Zip_Index <- spatial_link_index(PM, "Latitude", "Longitude", "Monitor",
#'                                         ZIP, "Latitude.zip", "Longitude.zip", "zip",
#'                                         within = within_km) 
spatial_link_index <- function(data1, lat1, lon1, id1, data2, lat2, lon2, id2,
                               within = NULL, closest = FALSE) {
  if (!is.data.table(data1))
    data1 <- data.table(data1)
  if (!is.data.table(data2))
    data1 <- data.table(data2)
  data1u <- unique(data1, by = id1)
  data2u <- unique(data2, by = id2)
  spData1 <- data1u
  spData2 <- data2u
  coordinates(spData1) <- c(lon1, lat1)
  coordinates(spData2) <- c(lon2, lat2)
  pairwise_distances <- data.frame(spDists(spData1, spData2, longlat = TRUE))
  names(pairwise_distances) <- data2u[[id2]]
  sId1 <- id1
  sId2 <- id2
  id1 <- data1u[[id1]]
  pairwise_distances_with_index <- data.table(cbind(id1, pairwise_distances))
  melted_pairwise_distances <- melt(pairwise_distances_with_index, id.vars = "id1")
  setnames(melted_pairwise_distances, old = names(melted_pairwise_distances), new = c(sId1, sId2, "Distance"))
  if (!is.null(within))
    melted_pairwise_distances <- subset(melted_pairwise_distances, Distance < within)
  if (closest) {
    melted_pairwise_distances[, Include.in.Average := 0]
    setkeyv(melted_pairwise_distances, c("Distance"))
    setkeyv(melted_pairwise_distances, sId2)
    melted_pairwise_distances[melted_pairwise_distances[, .I[1], by = key(melted_pairwise_distances)]$V1, 
                              Include.in.Average := 1]
    melted_pairwise_distances <- subset(melted_pairwise_distances, Include.in.Average == 1)
    melted_pairwise_distances[, Include.in.Average := NULL]
  }
  return(melted_pairwise_distances)
}