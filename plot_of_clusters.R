# Plotting the map including power plant treatment and zip codes in cluster.

find_hull <- function(x, long_col, lat_col) {
  x[chull(x[, long_col], x[, lat_col]), ]
}

library(rgdal)
library(raster)
library(proj4)
library(Interference)
library(lme4)
library(data.table)
library(gplots)
library(gridExtra)
library(ggplot2)
library(grDevices)
library(plyr)
library(RColorBrewer)
library(ggmap)


load('~/Dropbox/ARP/Projects/Bipartite_Interference_Paper/Bipartite_IPW/pp_data.Rdata')
load('~/Dropbox/ARP/Projects/Bipartite_Interference_Paper/Bipartite_IPW/zip_dta_complete_coords.Rdata')
setnames(zip_dta_complete_coords, 'neigh', 'cluster')

us.dat <- map_data("state")


df <- pp_dta[, list(Fac.Latitude, Fac.Longitude, neigh, Trt)]
setnames(df, 'neigh', 'cluster')
df <- as.data.frame(df)

n_neigh <- length(unique(df$cluster))
cols <- gray.colors(n_neigh, start = 0.1, end = 0.1, gamma = 2.2, alpha = NULL)


# Power plant data.

df$cluster <- as.factor(df$cluster)
df$Trt <- as.factor(df$Trt)
hulls <- ddply(df, "cluster", function(x) find_hull(x, long_col = 2, lat_col = 1))


# Zip code data.
zipp <- as.data.frame(zip_dta_complete_coords)
zipp$cluster <- as.factor(zipp$cluster)
zipp_hulls <- ddply(zipp, "cluster", function(x) find_hull(x, long_col = 2, lat_col = 3))

ggplot() + 
  geom_polygon(aes(long, lat, group = group), color = 'grey55', fill = 'grey90',
               data = us.dat) +
  geom_polygon(aes(Fac.Longitude, Fac.Latitude, fill = cluster), colour = 'grey65',
               data = hulls, alpha=.2) +
  geom_polygon(aes(long, lat, fill = cluster), colour = 'grey25',
               data = zipp_hulls, alpha=.2) +
#   geom_point(data = x, aes(Fac.Longitude, Fac.Latitude, colour = cluster,
#                            fill = cluster, shape = Trt), size = 2) +
#   scale_shape_manual(values=c(17, 10), labels = c("Other", "SCR/SNCR")) +
#   geom_point(data = zipp, aes(long, lat), size = 1, shape = 2, alpha = 0.1,
#              color = 'black') +
  geom_point(data = df, aes(Fac.Longitude, Fac.Latitude, colour = cluster,
                            fill = cluster), size = 1) +
  scale_color_manual('', values = cols) +
  guides(colour = 'none', fill = 'none') +
  scale_fill_manual("", values = cols) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.text = element_text(size = 17)) +
  xlab('') + ylab('') +
  guides(shape = guide_legend(title = ""))









