# Plotting results.
library(ggplot2)
library(gridExtra)

load('~/Dropbox/ARP/Projects/Bipartite_Interference_Paper/Bipartite_IPW/results1.dat')
attach(res)


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

