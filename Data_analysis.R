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


load('~/Dropbox/ARP/Projects/Bipartite_Interference_Paper/Bipartite_IPW/pp_data.Rdata')
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

n_zip <- 5000
n_neigh <- max(pp_dta$neigh)
zip_dta <- data.table(neigh = sample(1 : n_neigh, n_zip, replace = TRUE),
                      value = rnorm(n_zip), long = NA, lat = NA,
                      closest_int = NA)

# Generating locations near the power plants in the same cluster.
for (zz in 1 : n_zip) {
  neigh_zz <- zip_dta$neigh[zz]
  long_range <- range(subset(pp_dta, neigh == neigh_zz)$Fac.Longitude)
  lat_range <- range(subset(pp_dta, neigh == neigh_zz)$Fac.Latitude)
  zip_dta$long[zz] <- runif(1, long_range[1], long_range[2])
  zip_dta$lat[zz] <- runif(1, lat_range[1], lat_range[2])
}


# using Christine's function to get the neighborhood instead of using the one
# I generated.



setorder(zip_dta, 'neigh')

zip_dta[, zip_id := as.character(1 : n_zip)]


# Identifying which power plant is closest to each zip code.

link <- spatial_link_index(pp_dta, "Fac.Latitude", "Fac.Longitude", "id",
zip_dta, "lat", "long", "zip_id",
within = within_km, closest = FALSE)
link[, zip_id := as.character(zip_id)]
link[, id := as.character(id)]
setorder(link, Distance)


for (zz in 1 : n_zip) {
    link_zz <- subset(link, zip_id == zip_dta$zip_id[zz])
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



