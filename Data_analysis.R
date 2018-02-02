# Potential changes:
# - We might want to allow for covariates of the zip codes to play a role in ps.

library(rgdal)
library(raster)
library(proj4)
library(Interference)
library(lme4)
library(data.table)
library(gplots)
library(gridExtra)
library(ggplot2)
library(arepa)

within_km <- 300
buffer_meters <- 10000


load('~/Dropbox/ARP/Projects/Bipartite_Interference_Paper/Bipartite_IPW/pp_data.Rdata')
link_pp_dta <- pp_dta[, c('Fac.Longitude', 'Fac.Latitude', 'neigh')]
setnames(link_pp_dta, 'neigh', 'cluster')
n_neigh <- max(pp_dta$neigh)


source('~/Github/Bipartite_Interference/cluster_pp_function.R')
source('~/Github/Bipartite_Interference/cluster_plot_function.R')

# Cluster propensity score based on interventional units.

cov_cols <- 5 : 22
cov_names <- names(pp_dta)[cov_cols]
glm_form <- paste('Trt ~ (1 | neigh) +', paste(cov_names, collapse = ' + '))
glmod <- glmer(as.formula(glm_form), data = pp_dta, family = 'binomial',
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)))

phi_hat <- list(coefs = summary(glmod)$coef[, 1],
                re_var = as.numeric(summary(glmod)$varcor))



# Generate a data set of zip codes with some outcome:

zip_dta <- data.table(neigh = numeric(0), long = numeric(0), lat = numeric(0),
                      zipcode = character(0))
# using Christine's function to get the zipcodes within each neighborhood
# instead of generating them.
for (nn in 1 : n_neigh) {
  l <- cluster_pp(pp_data = link_pp_dta, cluster_id = nn,
                  buffer_meters = buffer_meters)
  zip_dta <- rbind(zip_dta, cbind(neigh = nn, long = l$linked_zip$longitude,
                                  lat = l$linked_zip$latitude,
                                  zipcode = l$linked_zip$zip))
}
setorder(zip_dta, 'neigh')
zip_dta[, lat := as.numeric(lat)]
zip_dta[, long := as.numeric(long)]
zip_dta[, value := rnorm(nrow(zip_dta))]
zip_dta[, closest_int := NA]


# Identifying which power plant is closest to each zip code.

link <- spatial_link_index(pp_dta, "Fac.Latitude", "Fac.Longitude", "id",
                           zip_dta, "lat", "long", "zipcode",
                           within = within_km, closest = FALSE)
link[, zipcode := as.character(zipcode)]
link[, id := as.character(id)]
setorder(link, Distance)


for (zz in 1 : nrow(zip_dta)) {
    link_zz <- subset(link, zipcode == zip_dta$zipcode[zz])
    neigh_zz <- zip_dta$neigh[zz]
    allowed_pp <- subset(pp_dta, neigh == neigh_zz)$id
    link_zz <- subset(link_zz, id %in% allowed_pp)
    zip_dta$closest_int[zz] <- link_zz$id[1]
}



# Getting the range of alphas.

obs_alpha <- pp_dta[, mean(Trt), by = neigh]$V1
alpha_range <- quantile(obs_alpha, probs = c(0.2, 0.8))
alpha <- seq(alpha_range[1], alpha_range[2], length.out = 40)


# Estimating the average potential outcomes.

trt_col <- which(names(pp_dta) == 'Trt')
out_col <- which(names(zip_dta) == 'value')


yhat_group <- BipartiteGroupIPW(int_dta = pp_dta, out_dta = zip_dta, 
                                cov_cols = cov_cols, phi_hat = phi_hat,
                                alpha = alpha, trt_col = trt_col,
                                out_col = out_col)

scores <- CalcScore(dta = pp_dta, neigh_ind = NULL, phi_hat = phi_hat,
                    cov_cols = cov_cols, trt_name = 'Trt')

ypop <- Ypop(ygroup = yhat_group, ps = 'estimated', scores = scores,
             dta = pp_dta)

yhat_pop <- ypop$ypop
yhat_pop_var <- ypop$ypop_var

# --------- Direct effect ----------- #
de <- DE(ypop = yhat_pop, ypop_var = yhat_pop_var, alpha = alpha)


# --------- Indirect effect ----------- #
ie <- IE(ygroup = yhat_group[, 1, ], ps = 'estimated', scores = scores)



# ------------  PLOTTING ------------ #

de_plot <- data.frame(alpha = alpha, de = de[1, ], low = de[3, ],
                      high = de[4, ])
g_de <- ggplot() + geom_line(aes(alpha, de), data = de_plot) +
  geom_ribbon(data = de_plot, aes(x = alpha, ymin = low, ymax = high),
              alpha=0.3) +
  geom_abline(intercept = 0, slope = 0, linetype = 2) +
  ylab(expression(DE(alpha))) + xlab(expression(alpha))


a1 <- c(1, 10, 20, 30, 40)
g_ie <- NULL

for (ii in a1) {
  
  ie_plot <- as.data.frame(t(ie[c(1, 3, 4), ii, ]))
  names(ie_plot) <- c('ie', 'low', 'high')
  
  g_ie1 <- ggplot() + geom_line(aes(alpha, ie), data = ie_plot) +
    geom_ribbon(data = ie_plot, aes(x = alpha, ymin = low, ymax = high),
                alpha = 0.3) +
    geom_abline(intercept = 0, slope = 0, linetype = 2) +
    ylab(paste0('IE(', round(alpha[ii], 3), ',', expression(alpha), ')')) +
    xlab(expression(alpha))
  
  g_ie[[length(g_ie) + 1]] <- g_ie1
} 


grid.arrange(grobs = append(list(g_de), g_ie[c(2, 4)]), ncol = 3)



