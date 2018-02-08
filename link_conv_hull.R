## TODO: test with 1 or 2 pp only
## DONE: set leaflet view on polygon centroid and zoom out
## DONE: also display the PP that are inside the convex hull
## DONE: better projection

##----- Load libraries
setwd('~/Github/Bipartite_Interference/')

library(data.table)
library(sp)
library(rgdal)
library(proj4)
library(rgeos)
library(zipcode)
library(leaflet)

##----- Load dataset

load("~/Dropbox/ARP/Projects/Bipartite_Interference_Paper/Bipartite_IPW/pp_data.Rdata")
pp_dta <- pp_dta[, c('Fac.Longitude', 'Fac.Latitude', 'neigh')]
setnames(pp_dta, 'neigh', 'cluster')
# write.csv(pp_cluster, "pp_cluster.csv")

source('~/Github/Bipartite_Interference/cluster_pp_function.R')
source('~/Github/Bipartite_Interference/cluster_plot_function.R')

##----- Example

# link cluster ID 20, buffer 10km
l <- cluster_pp(pp_data = pp_dta, cluster_id = 29, buffer_meters = 3000)
head(l$linked_zip)
l$cl
cluster_plot(l)

