library(rjdfilters)
library(ggplot2)
library(reshape2)
library(patchwork)

replaceTrailing0 <- function(x){
  i <- length(x)
  remove_i <- NULL
  while ((x[i] == 0) && i > 0) {
    remove_i <- c(i, remove_i)
    i <- i - 1
  }
  if(is.null(remove_i)){
    x
  } else{
    x[remove_i] <- NA
  }
  x
}
format_data <- function(coefs){
  # coefs = all_filters[[i]]
  data_coefs <- reshape2::melt(apply(coefs,2, replaceTrailing0))
  colnames(data_coefs) <- c("x", "label","y")
  data_coefs$x <- factor(data_coefs$x,levels = rownames(coefs), ordered = TRUE)
  data_coefs$label <- as.character(data_coefs$label)

  lags <- ncol(coefs)-1
  jfilters <- lapply(1:ncol(coefs), function(i) fst(coefs[,i],lags = lags))
  gain <- sapply(jfilters, `[[`, "filters.gain")
  phase <- sapply(jfilters, `[[`, "filters.phase")
  x_values = seq(0, pi, length.out = nrow(gain))
  gain <- as.data.frame(cbind(x_values,gain))
  phase <- as.data.frame(cbind(x_values,phase))
  colnames(gain) <- colnames(phase) <- c("x", colnames(coefs))
  data_gain <- reshape2::melt(gain, id.vars = "x")
  data_phase <- reshape2::melt(phase, id.vars = "x")
  colnames(data_gain) <- colnames(data_phase) <- c("x", "label","y")
  list(coefs = data_coefs, gain = data_gain, phase = data_phase)
}
plot_coefs <- function(data){
  ggplot(data = data, aes(x = x, y = y, group = label,
                          colour = label)) +
    geom_line(size = 0.7) +
    geom_point(size = 1) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = "Coefficients")
}

plot_comp <- function(data, ylab = "Gain",
                      xlim = c(0,1), nxlab = 7){
  x_lab_at <- seq(xlim[1], xlim[2], length.out = nxlab)
  data = data[(data$x >= xlim[1]*pi) & data$x <= xlim[2]*pi,]
  xlabel <- function(x, symbol = "pi"){
    x <- x
    fracs <- strsplit(attr(MASS::fractions(x), "fracs"), "/")  # convert to fractions
    labels <- sapply(fracs, function(i)
      if (length(i) > 1) { paste(i[1], "*", symbol, "/", i[2]) }
      else { paste(i, "*", symbol) })
    labels <- sub("0 * pi", "0", labels, fixed = TRUE)
    labels <- sub("1 * pi", " pi", labels, fixed = TRUE)
    labels
  }

  ggplot(data = data, aes(x = x, y = y, group = label,
                          colour = label)) +
    geom_line(size = 0.7) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5)) +
    labs(x = NULL, y = ylab) +
    scale_x_continuous(NULL,
                       breaks = x_lab_at*pi,
                       labels = parse(text=xlabel(x_lab_at))) +
    guides(colour = "none")
}
plot_graph <- function(coefs){
  data <- format_data(coefs)
  plot_coefs(data$coefs)
}


lp <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
  lp_filter(
    horizon = 6,
    degree = 3,
    kernel = "Henderson",
    endpoints = endpoints,
    ic = 3.5,
    tweight = 0,
    passband = pi/12
  )$filters.coef
})
names(lp) <- c("LC", "QL", "CQ", "DAF")

range_d = range(sapply(lp, range))
all_p = lapply(names(lp),
       function(x){
         plot_graph(lp[[x]]) + ggtitle(x) +ylim(range_d[1], range_d[2])
       })
patchwork::wrap_plots(all_p, ncol = 2)
ggsave(filename = "2022 - 09 - D2E/img/coefs_lp.pdf",
       patchwork::wrap_plots(all_p, ncol = 2),
       width = 8, height = 5)


rkhs = readRDS("C:/Users/zw20hj/github/turningPointsDetection/filters/rkhs_rw_p3.RDS")[["h=6"]]
rkhs = lapply(rkhs, function(x){
  colnames(x) <- colnames(lp[[1]])
  x
})
names(rkhs) <- c("Min. révisions", "Min. Gain", "Min. Déphasage")

range_d = range(sapply(rkhs, range))
all_p = lapply(names(rkhs),
               function(x){
                 plot_graph(rkhs[[x]]) + ggtitle(x) +ylim(range_d[1], range_d[2])
               })
patchwork::wrap_plots(all_p, ncol = 2)



ggsave(filename = "2022 - 09 - D2E/img/coefs_rkhs.pdf",
       patchwork::wrap_plots(all_p, ncol = 2),
       width = 8, height = 5)
