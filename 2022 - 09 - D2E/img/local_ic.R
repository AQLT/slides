library(rjdfilters)
h <- 6
q <- 6
X_gen <- function(d = 1, p = 6, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}
gen_MM <- function(p=6, q=p, d=2){
  k = rjdfilters::get_kernel("Henderson", h = h)
  k
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  k
  K = diag(k)
  X = X_gen(d=d, p = h, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  cbind(M1, M2, M3)
}
MM2 = sapply(0:6, function(x) c(gen_MM(d=3,q=x)[,2], rep(0, h-x)))

h_filter = lp_filter(horizon = h,endpoints = "DAF")$filters.coef
henderson_coef = h_filter[,ncol(h_filter)]

s = "../turningPointsDetection/data_simul/byseries/mediumvariability2.RDS"
# new_f = sprintf("../data_simul/byseriescv/%s", basename(s))

tp = readRDS("../turningPointsDetection/data_simul/tp_simul1.RDS")
data <- readRDS(s)
data_info <- readRDS(sub("byseries", "byseriesinfo", s))
sigma_2 = sapply(data, function(x){
  mean((x - henderson(x,length = 2*h+1,musgrave = F))^2,na.rm = TRUE)
})
sigma_2 = sigma_2/(1- 2*henderson_coef["t"] + sum(henderson_coef^2))
icr = ts(sapply(data_info,`[`,"icr-13"), start = 1962, frequency = 12)
rapport_act = 2/(sqrt(pi) * icr)
rapport_act = rapport_act^2
x = data[[length(data)]]
estim_deriv = lapply(0:h, function(q) jasym_filter(x, MM2[,q+1], -6))
estim_local_rapport = lapply(estim_deriv, function(x) x^2 / ts(sigma_2, start= start(na.omit(x)), frequency = 12))

lc_tp = readRDS("../turningPointsDetection/results_simul/lp/mediumvariability2_henderson_lc_tp.RDS")

data_est = do.call(ts.union, estim_local_rapport)
colnames(data_est) <- sprintf("q=%s", 0:6)
data = ts.intersect(estim_local_rapport[[7]], rapport_act)
colnames(data) = c("Estimation locale", "Estimation à partir ratio I/C")
library(ggplot2)

p_p = ggplot() + geom_vline(data = data.frame(xintercept = unlist(tp)),
                            aes(xintercept = xintercept))
AQLTools::hc_stocks(data_est,digits = 2)

p = AQLTools::graph_ts(data,n_xlabel = 10,size = 1,prec_plot = p_p) + theme_bw() +
  theme(legend.background = element_rect(fill = alpha("gray99", 0.4),
                                         colour = "gray80", linetype = "solid"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.key = element_blank(), legend.title = element_blank())
p
ggsave(filename = "2022 - 09 - D2E/img/local_ic_tp.pdf",
       p ,
       width = 8, height = 2.5)

library(rjdfilters)
library(ggplot2)
library(highcharter)
library(zoo)
library(patchwork)
X_gen <- function(d = 1, p = 6, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}
gen_MM <- function(p=6, q=p, d=2){
  k = rjdfilters::get_kernel("Henderson", h = p)
  k
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  k
  K = diag(k)
  X = X_gen(d=d, p = p, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  cbind(M1, M2, M3)
}
d2 = gen_MM(d=2)
d3 = gen_MM(d=3)
ylim_m2 = range(c(d2[,2], d2[,3]))
plot_coef_ggplot <- function(data){
  data$date <- factor(rownames(data), levels = rownames(data),ordered = TRUE)
  dataGraph <- reshape2::melt(data)

  ggplot(data = dataGraph, aes(x = date, y = value, group = variable,
                               colour = variable)) +
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

MM_D3 = data.frame(gen_MM(d=3,q=6)[,2], gen_MM(d=3,q=6)[,3])
rownames(MM_D3) <- rjdfilters:::coefficients_names(-6,6)
colnames(MM_D3) <- c("Est. pente", "Est. concavité")
plot_coef_ggplot(MM_D3)

MM_d3_pente = data.frame(sapply(0:6, function(x) c(gen_MM(d=3,q=x)[,2], rep(NA, 6-x))))
rownames(MM_d3_pente) <-  rjdfilters:::coefficients_names(-6,6)
colnames(MM_d3_pente) <- sprintf("q=%i",0:6)

MM_d3_deriv2 = data.frame(sapply(0:6, function(x) c(gen_MM(d=3,q=x)[,3], rep(NA, 6-x))))
rownames(MM_d3_deriv2) <-  rjdfilters:::coefficients_names(-6,6)
colnames(MM_d3_deriv2) <- sprintf("q=%i",0:6)
p = (plot_coef_ggplot(MM_d3_pente) + ggtitle("MM utilisées pour estimer la pente") +
    theme(legend.position="none")
) + (plot_coef_ggplot(MM_d3_deriv2) + ggtitle("MM utilisées pour estimer la concavité"))

p
ggsave(filename = "2022 - 09 - D2E/img/local_ic_mm.pdf",
       p ,
       width = 8, height = 2.5)
