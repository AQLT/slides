source("../turningPointsDetection/R_simul/4-utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)

all_tp <- merge(readRDS("../turningPointsDetection/results_simul/compile_tp_norev/troughs_lp.RDS"),
                readRDS("../turningPointsDetection/results_simul/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var() |>
  filter(method %in% c("lc", "ql"))  %>% mutate(article = "lpp")
all_rev_fe <- readRDS("../turningPointsDetection/results_simul/compile_revisions/lp_fe_rev.RDS") |>
  select_series() |>
  select_mae() |>
  filter(method %in% c("lc", "ql"))  %>% mutate(article = "lpp")
all_rev_ce <- readRDS("../turningPointsDetection/results_simul/compile_revisions/lp_ce_rev.RDS") |>
  select_series() |>
  select_mae() |>
  filter(method %in% c("lc", "ql"))  %>% mutate(article = "lpp")

all_tp_arima <-
  merge(readRDS("../turningPointsDetection/results_simul/compile_tp_norev/troughs_arima.RDS"),
        readRDS("../turningPointsDetection/results_simul/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()  %>% mutate(article = "arima")
all_rev_arima_fe <- readRDS("../turningPointsDetection/results_simul/compile_revisions/arima_fe_rev.RDS") |>
  select_series() |>
  select_mae() %>% mutate(article = "arima")
all_rev_arima_ce <- readRDS("../turningPointsDetection/results_simul/compile_revisions/arima_ce_rev.RDS") |>
  select_series() |>
  select_mae() %>% mutate(article = "arima")

all_tp_lic_daf <- merge(readRDS("../turningPointsDetection/results_simul/compile_tp_norev/troughs_localic_daf.RDS"),
                        readRDS("../turningPointsDetection/results_simul/compile_tp_norev/peaks_localic_daf.RDS"),
                        by=c("series", "kernel", "h", "degree", "method")) %>%
  select_var()%>%
  mutate(variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE)) |>
  filter(h == "h6", degree == "d3") |>
  mutate(article = "local ic",
         method = paste0(method, "_local_ic"))|>
  select(!c(h, degree))
all_rev_fe_lic <- readRDS("../turningPointsDetection/results_simul/compile_revisions/localic_daf_fe_rev.RDS") |>
  select_series() |>
  select_mae() |>
  filter(h == "h6", degree == "d3") |>
  mutate(article = "local ic",
         method = paste0(method, "_local_ic"))|>
  select(!c(h, degree))
all_rev_ce_lic <- readRDS("../turningPointsDetection/results_simul/compile_revisions/localic_daf_ce_rev.RDS") |>
  select_series() |>
  select_mae()  |>
  filter(h == "h6", degree == "d3") |>
  mutate(article = "local ic",
         method = paste0(method, "_local_ic"))|>
  select(!c(h, degree))

all_tp <- rbind(all_tp,
                all_tp_arima,
                all_tp_lic_daf) %>%
  mutate(method = factor(method,levels =  c("lc", "lc_local_ic","ql", "ql_local_ic", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel)) %>%  filter(kernel == "henderson")
all_rev_fe <- rbind(all_rev_fe,
                    all_rev_arima_fe,
                    all_rev_fe_lic) %>%
  mutate(method = factor(method,levels = c("lc", "lc_local_ic","ql", "ql_local_ic", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel)) %>%  filter(kernel == "henderson")
all_rev_ce <- rbind(all_rev_ce,
                    all_rev_arima_ce,
                    all_rev_ce_lic) %>%
  mutate(method = factor(method,levels = c("lc", "lc_local_ic","ql", "ql_local_ic", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel)) %>%  filter(kernel == "henderson")

normalise_rev <- function(x, ref = "lc", suff = "^(rev|X)"){
  ref_lc = x[(x$method == "lc") & (x$kernel == "henderson"),grep(suff,colnames(x)) ]
  ref_ql = x[(x$method == "ql") & (x$kernel == "henderson"),grep(suff,colnames(x)) ]
  for(k in unique(x$kernel)){
    for(m in unique(x$method)){
      if(nrow(x[x$method == m & x$kernel == k,grep(suff,colnames(x))]) > 0){
        if(!(m == "lc" & k == "henderson"))
          x[x$method == m & x$kernel == k,grep(suff,colnames(x))] <-
            x[x$method == m& x$kernel == k,grep(suff,colnames(x))] / ref_lc
      }
    }
  }
  x
}
summarise_ref <- function(x, normalise = FALSE){
  if(normalise){
    x = x %>% normalise_rev()
    digits = 1
  } else{
    digits = 2
  }
  x %>%
    group_by(variability, kernel, method) %>%
    summarise(across(
      .cols = where(is.numeric),
      .fns = list(Mean = \(x) round(mean(x),digits)),
      .names = "{col}"
    )) %>%
    select(!c(rev.q6:rev.q10, length)) %>%
    data.frame()   %>%
    select(!c(kernel))
}
rev_tot = rbind(all_rev_fe %>% summarise_ref(normalise = FALSE),
                all_rev_ce %>% summarise_ref(normalise = FALSE)
) |> filter(method != "auto_arima") %>%
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         lc_local_ic = "LC avec IC local",
                         ql_local_ic = "QL avec IC local")) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Méthode` = method)

rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE),
                all_rev_ce %>% summarise_ref(normalise = TRUE)
) |> filter(method != "auto_arima") %>%
  mutate(method = recode(method, lc = "LC", ql = "QL (rel)",
                         lc_local_ic = "LC avec IC local (rel)",
                         ql_local_ic = "QL avec IC local (rel)")) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Méthode` = method)

format_table <- function(x, var) {
  x = x %>% filter(variability == var)%>%
    select(!c(variability))
  x %>%
    kable(format.args = list(digits = 2,
                             decimal.mark = ","),
          align = "c", booktabs = T, row.names = FALSE,
          escape = FALSE,format = "latex") %>%
    kable_styling(latex_options=c(#"striped",
      "hold_position")) %>%
    pack_rows(index = c("\\textbf{MAE entre $q$\\ieme{} et la dernière estimation}"= nrow(x)/2,
                        "\\textbf{MAE entre $q$\\ieme{} et la $q+1$\\ieme{} estimation}"=nrow(x)/2),
              escape = FALSE)
}

rev_rel %>%
  format_table(var = "mediumvariability")

rev_rel %>%
  format_table(var = "lowvariability")

rev_rel %>%
  format_table(var = "highvariability")




# Graphique sur le dephasage
format_table_tp <- function(x){
  x %>%
    tidyr::pivot_longer(
      cols = starts_with("x"),
      names_to = "name",
      values_to = "value"
    )%>% filter(kernel == "henderson") %>%
    select(!length) |>
    unique_series_pivot() %>%
    mutate(variability = recode(variability,
                                lowvariability = "Faible variabilité",
                                mediumvariability = "Variabilité moyenne",
                                highvariability = "Forte variabilité")) %>%
    na.omit()
}
data_tp <- all_tp %>% format_table_tp() %>%
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         lc_local_ic = "LC~avec~IC~local",
                         ql_local_ic = "QL~avec~IC~local",
                         auto_arima = "ARIMA"))

data_tp=data_tp[data_tp$value<=30,]
p = ggplot(data_tp ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + AQLTools:::theme_aqltools() +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())
p
ggsave("../slides/2022 - 09 - D2E/img/simul_phase_shift_localic_norev.pdf",
       plot =p,
       width = 8,height = 5)
