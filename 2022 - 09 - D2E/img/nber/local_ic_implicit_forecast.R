library(dplyr)
library(ggplot2)
library(plot3D)
library(magrittr)

series_with_tp <- readRDS("../turningPointsDetection/results/series_to_study.RDS")
length_info <-
  sapply(sprintf("../turningPointsDetection/data/byseriesinfo/%s.RDS", series_with_tp),
         function(f){
           info <- readRDS(f)
           info[[1]][1]
         })
length_info <- data.frame(series = series_with_tp,
                          length = as.numeric(length_info))

troughs_detected <- t(sapply(list.files("../turningPointsDetection/results/compile_tp",pattern = "troughs",full.names = TRUE), function(f){
  all_t <- readRDS(f)
  !apply(all_t[,1:35],2,\(x) all(is.na(x)))
}))

peaks_detected <- t(sapply(list.files("../turningPointsDetection/results/compile_tp",pattern = "peaks",full.names = TRUE), function(f){
  all_t <- readRDS(f)
  !apply(all_t[,1:34],2,\(x) all(is.na(x)))
}))


selected_tp <- c("X2001.83333333333", "X2009.41666666667", "X2020.25", "X2001.16666666667",
                 "X2007.91666666667", "X2020.08333333333")
not_selected_tp <- c("X1854.91666666667", "X1858.91666666667", "X1861.41666666667",
                     "X1867.91666666667", "X1870.91666666667", "X1879.16666666667",
                     "X1885.33333333333", "X1888.25", "X1891.33333333333", "X1894.41666666667",
                     "X1897.41666666667", "X1900.91666666667", "X1904.58333333333",
                     "X1908.41666666667", "X1912", "X1914.91666666667", "X1919.16666666667",
                     "X1921.5", "X1924.5", "X1927.83333333333", "X1933.16666666667",
                     "X1938.41666666667", "X1945.75", "X1949.75", "X1954.33333333333",
                     "X1958.25", "X1961.08333333333", "X1970.83333333333", "X1975.16666666667",
                     "X1980.5", "X1982.83333333333", "X1991.16666666667", "X1857.41666666667",
                     "X1860.75", "X1865.25", "X1869.41666666667", "X1873.75", "X1882.16666666667",
                     "X1887.16666666667", "X1890.5", "X1893", "X1895.91666666667",
                     "X1899.41666666667", "X1902.66666666667", "X1907.33333333333",
                     "X1910", "X1913", "X1918.58333333333", "X1920", "X1923.33333333333",
                     "X1926.75", "X1929.58333333333", "X1937.33333333333", "X1945.08333333333",
                     "X1948.83333333333", "X1953.5", "X1957.58333333333", "X1960.25",
                     "X1969.91666666667", "X1973.83333333333", "X1980", "X1981.5",
                     "X1990.5")
not_selected_tp_grep <- paste(sprintf("(%s)", not_selected_tp), collapse = "|")
select_var <- function(x){
  # x = x[x$series %in% series_with_tp_ul,
  #       grep(not_selected_tp_grep, colnames(x), invert = TRUE)]
  # x = merge(x, length_info, all.x = TRUE, all.y = FALSE)
  x = select_series(x)
  x = x[,
        grep(not_selected_tp_grep, colnames(x), invert = TRUE)]
  toNA <- apply(x[,grep("^X", colnames(x))],2, function(x_){
    res = x_ > x$length
    res[is.na(res)] <- FALSE
    res
  } )
  x[,grep("^X", colnames(x))][toNA] <- NA
  x
}
select_series <- function(x){
  # x = x[x$series %in% series_with_tp_ul,]
  x = merge(x, length_info, all.x = TRUE, all.y = FALSE)
  x
}
nb_obs <- function(x){
  sum(!is.na(x))
}

select_mae <- function(x){
  x %>%
    filter(Group == "total",
           stats == "MAE") %>%
    select(!c(Group, stats))
}

plot_est <- function (data, date_tp =2020.25, titre = NULL, sous_titre = NULL, limits_y = NULL) {
  cex = 0.4;
  x_lab = y_lab= NULL; x_lab_month = TRUE;
  outDec = ",";  n_xlabel = 6 ;n_ylabel = 4;
  size = 0.7; size_label = 2

  dataGraph <- format_data_plot(data)
  data_legend = dataGraph %>%
    group_by(variable) %>%
    filter(date == max(date)) %>% data.frame()
  p <- ggplot(data = dataGraph) +
    geom_vline(xintercept = date_tp, linetype = "dotted") +
    geom_line(mapping = aes(x = date, y = value, group = variable,
                            colour = variable), size = size) +
    labs(title = titre, subtitle = sous_titre, x = x_lab,
         y = y_lab) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
                       labels = \(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month,
                                                                 outDec = outDec)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
                       labels = function(x) format(x, decimal.mark = outDec),
                       limits = limits_y) +
    AQLTools:::theme_aqltools()
  p +
    geom_text(aes(x = date, y = value, label =variable, colour = variable), data = data_legend,
              check_overlap = TRUE, hjust = 0, nudge_x = 0.01,
              size = size_label) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size=8, angle=20,
                                     vjust=1.1, hjust=1),
          axis.text.y = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,face="italic"))
}
format_data_plot  <- function(data){
  time <- time(data)
  freq <- frequency(data)
  dataGraph <- data.frame(cbind(time, data))
  colnames(dataGraph) <- c("date", colnames(data))
  reshape2::melt(dataGraph, id = "date") |> na.omit()
}
plot_prevs <- function (data, data_prevs, date_tp =2020.25, titre = NULL, sous_titre = NULL, limits_y = NULL,
                        linetype_prev = "dashed") {
  cex = 0.4;
  x_lab = y_lab= NULL; x_lab_month = TRUE;
  outDec = ",";  n_xlabel = 6 ;n_ylabel = 4;
  size = 0.7; size_label = 2


  dataGraph <- format_data_plot(data)
  dataGraph_prevs <- format_data_plot(data_prevs)
  data_legend = dataGraph_prevs  %>%
    group_by(variable) %>%
    filter(date == max(date)) %>% data.frame()
  p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable,
                                    colour = variable)) +
    geom_vline(xintercept = date_tp, linetype = "dotted") +
    geom_line(size = size) +
    geom_line(data = dataGraph_prevs, aes(x = date, y = value, group = variable,
                                          colour = variable), size = 0.6, linetype = linetype_prev)+
    labs(title = titre, subtitle = sous_titre, x = x_lab,
         y = y_lab) +
    coord_cartesian(ylim=limits_y) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
                       labels = \(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month,
                                                                 outDec = outDec)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
                       labels = function(x) format(x, decimal.mark = outDec)) +
    AQLTools:::theme_aqltools()
  p +
    geom_text(aes(x = date, y = value, label =variable, colour = variable), data = data_legend,
              check_overlap = TRUE, vjust = 1, nudge_y = -0.01,
              size = size_label) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size=8, angle=20,
                                     vjust=1.1, hjust=1),
          axis.text.y = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,face="italic"))
}
extract_est_data <- function(method = "lc", kernel = "henderson", nb_est = 10,
                             series = "RETAILx",
                             tp_date = 2020.25,
                             nb_dates_before = 6){
  # print(method)
  sep = "_"
  if(method %in% c("lc","ql", "cq", "daf")){
    dir <- "lp"
    full_name <- sprintf("%s_%s", kernel, method)
  }else{
    if(length(grep("arima", method)) >0){
      dir <- "arima"
      full_name <- sep <- ""
    }else if (length(grep("_local_ic", method)) >0){
      dir <- "localic_daf"
      if (length(grep("ql", method)) > 0) {
        full_name <- "ql_d3"
      } else {
        full_name <- "lc_d3"
      }

    } else {
      dir <- "rkhs"
      full_name <- method
    }
  }
  file = sprintf("../turningPointsDetection/results_nber/%s/%s%s%s.RDS", dir, series, sep, full_name)
  data <- readRDS(file)
  data <- do.call(ts.union, data)
  colnames(data) <- as.character(zoo::as.yearmon(as.numeric(colnames(data))))
  column_to_keep <- !apply(window(data, start = tp_date),2, \(x) all(is.na(x)))
  data <- data[,column_to_keep]
  data <- data[,1:nb_est]
  last_date_est = zoo::na.trim(data[,ncol(data)], sides = "left")
  last_date <- time(last_date_est)[which(is.na(last_date_est))[1] - 1]
  window(data, start = tp_date - nb_dates_before/frequency(data),
         end = last_date)
}
library(patchwork)

library(patchwork)
library(rjdfilters)
library(AQLThesis)

series = "RETAILx"
s <- sprintf("../turningPointsDetection/data/byseries_nber/%s.RDS", series)
tp_keep = "2020.25"
tp_date = as.numeric(tp_keep)
nb_est = 10
nb_dates_before <- 6
data <- readRDS(s)
data <- data[names(data) >= tp_keep][1:nb_est]
data_info <- readRDS(sub("byseries_nber", "byseriesinfo_nber", s))
data_info <- data_info[names(data_info) >= tp_keep][1:nb_est]


names(data) <- as.character(zoo::as.yearmon(as.numeric(names(data))))

arima_prevs = do.call(ts.union, lapply(data, function(y){
  prevs = forecast::auto.arima(y, max.Q = 0, max.D = 0, max.P = 0) |>
    forecast::forecast(6)
  ts(c(tail(y,1), prevs$mean), start = tail(time(y),1),
     frequency = frequency(y))
}))

prev_inter = lapply(1:ncol(arima_prevs),function(i){
  x = na.omit(arima_prevs[,i])
  data_all = ts(c(window(data[[i]], end = time(x)[1] -1/frequency(data[[i]])),
                  x),
                start = start(data[[i]]),
                frequency = frequency(data[[i]]))
  henderson(data_all,length = 13,musgrave = FALSE)
})
tp_arima = lapply(prev_inter,function(x){
  AQLThesis::turning_points(x)
})

names(prev_inter) <- names(data)
names(tp_arima) <- sapply(prev_inter, \(x) tail(time(na.omit(x)),1))

prev_inter <- window(do.call(ts.union, prev_inter),
                     start = tp_date - nb_dates_before/frequency(data[[1]]))

rkhs_f <- readRDS("../turningPointsDetection/filters/rkhs_rw_p3.RDS")$`h=6`
rkhs_f

prevs_imp_h_lp <-
  lapply(rkhs_f, function(coefs){
    do.call(ts.union, lapply(data, function(y){
      prevs = implicit_forecast(y=y, coefs = coefs)
      prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                   frequency = frequency(y))
      prevs_a
    }))
  })
names(prevs_imp_h_lp) <- c("frf", "gain", "phase")
list_kernel <- c("Henderson")
list_method <- c("LC","QL", "CQ", "DAF")
prevs_imp_lp <- do.call(c,
                        lapply(list_kernel, function(kernel){
                          lapply(list_method, function(method){
                            do.call(ts.union, lapply(seq_along(data), function(i){
                              y <- data[[i]]
                              l = 13
                              icr = data_info[[i]][sprintf("icr-%s", l)]
                              lp_coef = lp_filter(horizon = (l-1)/2,
                                                  kernel = kernel,
                                                  endpoints = method,
                                                  ic = icr)$filters.coef
                              prevs = implicit_forecast(y=y, coefs = lp_coef)
                              prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                                           frequency = frequency(y))
                              prevs_a
                            }))
                          })
                        }))
names(prevs_imp_lp) <- sprintf("%s_%s", rep(tolower(list_kernel), each = length(list_method)),
                               rep(tolower(list_method),length(list_kernel)))

lp_filter2 <- function(icr, method = "LC", h = 6, kernel = "Henderson"){
  all_coef = lapply(icr, function(ic){
    lp_filter(horizon = h,
              kernel = kernel,
              endpoints = method,
              ic = ic)$filters.coef
  })
  sym = all_coef[[1]][,h+1]
  final_coef = cbind(sapply(h:1, function(i){
    q=h + 1-i
    all_coef[[i]][,q]
  }), sym)
  colnames(final_coef) <- colnames(all_coef[[1]])
  final_coef
}

data_info_localic <- sub("byseries_nber", "byseriespente_daf_nber", s) |> readRDS()
data_info_localic <- data_info_localic[names(data_info_localic) >= tp_keep][1:nb_est]
d=3

prevs_imp_local_ic_daf <- lapply(c("LC","QL"), function(method){
  do.call(ts.union, lapply(seq_along(data), function(i){
    y <- data[[i]]
    print(i)
    data_t = data_info_localic[[i]][[method]]
    ratio = data_t[[sprintf("d=%i", d)]] / sqrt(data_t[["sigma2"]])
    icr = 2/(sqrt(pi) * ratio)
    # icr[1:3] <- 2.5
    lp_coef = lp_filter2(ic = icr,method = method)
    prevs = implicit_forecast(y=y, coefs = lp_coef)
    prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                 frequency = frequency(y))
    prevs_a
  }))
})
names(prevs_imp_local_ic_daf) <- sprintf("%s_localic_daf",
                                         tolower(c("LC","QL")))




data_ <- do.call(ts.union, data)
data_ <- window(data_, start = tp_date - nb_dates_before/frequency(data_))

all_prevs = c(prevs_imp_lp, prevs_imp_h_lp,
              prevs_imp_local_ic_daf)
all_prevs = lapply(all_prevs, function(x){
  colnames(x) <- colnames(data_)
  x
})


all_titles = list(lc_h = list(title = "Linear-Constant~(LC)~(Musgrave)", subtitle = "Déphasage de 3 mois"),
                  ql_h = list(title = "Quadratic-Linear~(QL)", subtitle = "Déphasage de 2 mois"),
                  cq_h = list(title = "Cubic-quadratic~(CQ)", subtitle = "Déphasage de 2 mois"),
                  daf_h = list(title = "DAF", subtitle = "Déphasage de 2 mois"),
                  rkhs_frf = list(title = "b['q, '] [Gamma]",
                                  subtitle = "Déphasage de 3 mois"),
                  rkhs_gain = list(title = "b['q, '] [G]",
                                   subtitle = "Déphasage de 2 mois"),
                  rkhs_phase = list(
                    title = "b['q, '] [phi]", subtitle = "Déphasage de 6 mois"),
                  lc_localic_daf = list(
                    title = "(LC)~local~IC", subtitle = "Déphasage de 3 mois"),
                  ql_localic_daf = list(
                    title = "(QL)~local~IC", subtitle = "Déphasage de 2 mois")
                  )


data_plots <- all_prevs[c("henderson_lc", "henderson_ql", "henderson_cq", "henderson_daf", "frf", "gain", "phase",
                          "lc_localic_daf", "ql_localic_daf")]

all_ranges = range(c(sapply(c(data_plots), range,na.rm = TRUE), range(data_, na.rm = TRUE)))
all_ranges = range(c(sapply(data_plots[c("henderson_lc", "henderson_ql", "lc_localic_daf")], range,na.rm = TRUE), range(data_, na.rm = TRUE)))

all_range_l = list(NULL, all_ranges, NULL, NULL, NULL, NULL, NULL, NULL, all_ranges)
plots <- lapply(seq_along(data_plots),
                \(x,...)plot_prevs(
                  data_,
                  data_prevs = data_plots[[x]],
                  titre = parse(text = all_titles[[x]][[1]]),
                  limits_y = all_range_l[[x]],
                  # sous_titre = all_titles[[x]][[2]],
                  ...)
                )
wrap_plots(plots[c(1,2,8,9)],ncol = 2)



ggsave("../slides/2022 - 09 - D2E/img/nber/retailx_localic_implicit_forecast.pdf",
       plot = wrap_plots(plots[c(1,2,8,9)],ncol = 2),
            width = 7,height = 5)
