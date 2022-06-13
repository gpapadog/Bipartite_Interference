
library(data.table)
library(sp)
library(rgdal)
library(proj4)
library(rgeos)
library(zipcode)
library(leaflet)

setwd('~/Github/Bipartite_Interference/')

load_path <- 'Data/'
source_path <- 'functions/'

##----- Load dataset

load(paste0(load_path, 'pp_data_all_clusters.Rdata'))
pp_dta <- pp_dta[, c('Fac.Longitude', 'Fac.Latitude', 'neigh')]
setnames(pp_dta, 'neigh', 'cluster')


source(paste0(source_path, 'cluster_pp_function.R'))
source(paste0(source_path, 'cluster_plot_function.R'))

##----- Example

# link cluster ID 20, buffer 10km
l <- cluster_pp(pp_data = pp_dta, cluster_id = 20, buffer_meters = 3000)
head(l$linked_zip)
l$cl
cluster_plot(l, icon_path = '~/Github/Bipartite_Interference/')

