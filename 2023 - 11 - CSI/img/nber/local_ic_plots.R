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
    scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
                       labels = \(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month,
                                                                 outDec = outDec)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
                       labels = function(x) format(x, decimal.mark = outDec),
                       limits = limits_y) +
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

all_tp <- merge(readRDS("../turningPointsDetection/results_nber/compile_tp_norev/troughs_lp.RDS"),
                readRDS("../turningPointsDetection/results_nber/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var()

all_tp_rkhs <-
  merge(readRDS("../turningPointsDetection/results_nber/compile_tp_norev/troughs_rkhs.RDS"),
        readRDS("../turningPointsDetection/results_nber/compile_tp_norev/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()

all_tp_rkhs <- rbind(all_tp %>% mutate(article = "lpp"),
                     all_tp_rkhs %>% mutate(article = "rkhs"))

all_tp_arima <-
  merge(readRDS("../turningPointsDetection/results_nber/compile_tp_norev/troughs_arima.RDS"),
        readRDS("../turningPointsDetection/results_nber/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var() %>% mutate(article = "arima")

all_tp_lic <-
  merge(readRDS("../turningPointsDetection/results_nber/compile_tp_norev/troughs_localic_daf.RDS"),
        readRDS("../turningPointsDetection/results_nber/compile_tp_norev/peaks_localic_daf.RDS"),
        by=c("series","h", "kernel", "degree", "method")) %>%
  select_var()%>%
  filter(h == "h6", degree == "d3") |>
  mutate(article = "local ic",
         method = paste0(method, "_local_ic"))|>
  select(!c(h, degree))
all_tp_lic <- all_tp_lic[,colnames(all_tp_rkhs)]


all_tp <- rbind(all_tp_rkhs, all_tp_arima, all_tp_lic) %>%
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase","auto_arima", "lc_local_ic",
                                           "ql_local_ic"),
                         ordered = TRUE),
         kernel = tolower(kernel)) |>
  filter(kernel == "henderson")


series <- "RETAILx"
tp_keep = "2020.25"
column_to_remove <- grep(tp_keep, grep("^X", colnames(all_tp),value = TRUE), value = TRUE, invert = TRUE)
all_tp_ =
  all_tp %>% filter(series %in% !!series) %>%
  mutate(method2 = recode(method, lc = "Linear-Constant~(LC)",
                          ql = "Quadratic-Linear~(QL)",
                          cq = "Cubic-quadratic~(CQ)", daf = "DAF",
                          frf = "b['q, '] [Gamma]",
                          gain = "b['q, '] [G]",
                          phase = "b['q, '] [phi]",
                          lc_local_ic = "LC~avec~IC~local",
                          ql_local_ic = "QL~avec~IC~local",)) %>%
  mutate(title = method2,
         subtitle = sprintf("Déphasage de %i mois",
                            round(X2020.25))) # %>%
  # select(!c(!!column_to_remove, article))




all_mod <- list(lc_h = list(kernel = "henderson",
                            method = "lc"),
                ql_h = list(kernel = "henderson",
                            method = "ql"),
                cq_h = list(kernel = "henderson",
                            method = "cq"),
                daf_h = list(kernel = "henderson",
                             method = "daf"),
                rkhs_frf = list(kernel = "henderson",
                                method = "frf"),
                rkhs_gain = list(kernel = "henderson",
                                 method = "gain"),
                rkhs_phase = list(kernel = "henderson",
                                  method = "phase"),
                arima = list(kernel = "henderson",
                             method = "auto_arima"),
                lc_local_ic = list(kernel = "henderson",
                                method = "lc_local_ic"),
                ql_local_ic = list(kernel = "henderson",
                                method = "ql_local_ic")
)
all_titles = lapply(all_mod, function(x){
  title = all_tp_ %>% filter(kernel == x$kernel,
                            method == x$method) %>%
    select(c(title, subtitle))
  # gsub(" ","~", title)
  title
})
all_mod_est[[1]] - all_mod_est[[9]]

all_mod_est <- lapply(all_mod, \(x) do.call(extract_est_data, x))
all_range <- range(sapply(all_mod_est, range,na.rm = TRUE))

plots <- lapply(names(all_mod_est),
                \(x,...)plot_est(all_mod_est[[x]],
                                 titre = parse(text = all_titles[[x]][,1] |> as.character() ),
                                 sous_titre = all_titles[[x]][2],
                                 ...),
                limits_y = all_range)
wrap_plots(plots[1:4])
wrap_plots(plots[(5:8)])
wrap_plots(plots[-(1:8)])


ggsave("../slides/2022 - 09 - D2E/img/nber/retailx_localic.pdf",
            plot = wrap_plots(plots[c(1,2, 9, 10)]),
            width = 7,height = 5)

