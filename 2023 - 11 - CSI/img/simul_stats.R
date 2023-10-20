source("../turningPointsDetection/R_simul/4-utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)
library(kableExtra)

all_tp <- merge(readRDS("../turningPointsDetection/results_simul/compile_tp_norev/troughs_lp.RDS"),
                readRDS("../turningPointsDetection/results_simul/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var()
all_rev_fe <- readRDS("../turningPointsDetection/results_simul/compile_revisions/lp_fe_rev.RDS") |>
  select_series() |>
  select_mae()
all_rev_ce <- readRDS("../turningPointsDetection/results_simul/compile_revisions/lp_ce_rev.RDS") |>
  select_series() |>
  select_mae()

all_tp_rkhs <-
  merge(readRDS("../turningPointsDetection/results_simul/compile_tp_norev/troughs_rkhs.RDS"),
        readRDS("../turningPointsDetection/results_simul/compile_tp_norev/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_rev_rkhs_fe <- readRDS("../turningPointsDetection/results_simul/compile_revisions/rkhs_fe_rev.RDS") |>
  select_series() |>
  select_mae()
all_rev_rkhs_ce <- readRDS("../turningPointsDetection/results_simul/compile_revisions/rkhs_ce_rev.RDS") |>
  select_series() |>
  select_mae()
all_tp_rkhs <- rbind(all_tp %>% mutate(article = "lpp"),
                     all_tp_rkhs %>% mutate(article = "rkhs"))
all_rev_rkhs_fe <- rbind(all_rev_fe %>% mutate(article = "lpp"),
                         all_rev_rkhs_fe %>% mutate(article = "rkhs"))
all_rev_rkhs_ce <- rbind(all_rev_ce %>% mutate(article = "lpp"),
                         all_rev_rkhs_ce %>% mutate(article = "rkhs"))

all_tp_arima <-
  merge(readRDS("../turningPointsDetection/results_simul/compile_tp_norev/troughs_arima.RDS"),
        readRDS("../turningPointsDetection/results_simul/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_rev_arima_fe <- readRDS("../turningPointsDetection/results_simul/compile_revisions/arima_fe_rev.RDS") |>
  select_series() |>
  select_mae()
all_rev_arima_ce <- readRDS("../turningPointsDetection/results_simul/compile_revisions/arima_ce_rev.RDS") |>
  select_series() |>
  select_mae()

all_tp <- rbind(all_tp_rkhs,
                all_tp_arima %>% mutate(article = "arima")) %>%
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))
all_rev_fe <- rbind(all_rev_rkhs_fe,
                    all_rev_arima_fe %>% mutate(article = "rkhs")) %>%
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))
all_rev_ce <- rbind(all_rev_rkhs_ce,
                    all_rev_arima_ce %>% mutate(article = "rkhs")) %>%
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))

normalise_rev <- function(x, ref = "lc", suff = "^(rev|X)"){
  ref = x[(x$method == "lc") & (x$kernel == "henderson"),grep(suff,colnames(x)) ]
  for(k in unique(x$kernel)){
    for(m in unique(x$method)){
      if(nrow(x[x$method == m & x$kernel == k,grep(suff,colnames(x))]) > 0){
        if(!(m == "lc" & k == "henderson"))
          x[x$method == m & x$kernel == k,grep(suff,colnames(x))] <-
            x[x$method == m& x$kernel == k,grep(suff,colnames(x))] / ref
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
    data.frame() %>%  filter(kernel == "henderson")  %>%
    select(!c(kernel))
}
rev_tot = rbind(all_rev_fe %>% summarise_ref(),
                all_rev_ce %>% summarise_ref()) %>%
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "$b_{q,\\Gamma}$",
                         gain = "$b_{q,G}$",
                         phase = "$b_{q,\\varphi}$",
                         auto_arima = "ARIMA")) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Méthode` = method)
rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE),
                all_rev_ce %>% summarise_ref(normalise = TRUE)) %>%
  mutate(method = recode(method, lc = "LC", ql = "QL (rel)",
                         cq = "CQ (rel)", daf = "DAF (rel)",
                         frf = "$b_{q,\\Gamma}$ (rel)",
                         gain = "$b_{q,G}$ (rel)",
                         phase = "$b_{q,\\varphi}$ (rel)",
                         auto_arima = "ARIMA (rel)")) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Méthode` = method)

rev_table <- rev_tot %>% filter(variability == "mediumvariability")%>%
  select(!c(variability))

format_table <- function(x, var) {
  x %>% filter(variability == var)%>%
    select(!c(variability)) %>%
    kable(format.args = list(digits = 2,
                             decimal.mark = ","),
          align = "c", booktabs = T, row.names = FALSE,
          escape = FALSE,format = "latex") %>%
    kable_styling(latex_options=c(#"striped",
      "hold_position")) %>%
    pack_rows(index = c("\\textbf{MAE entre $q$\\ieme{} et la dernière estimation}"=8,
                        "\\textbf{MAE entre $q$\\ieme{} et la $q+1$\\ieme{} estimation}"=8),
              escape = FALSE)
}

rev_rel %>%
  format_table(var = "mediumvariability")

rev_rel %>%
  format_table(var = "lowvariability")

rev_rel %>%
  format_table(var = "highvariability")

# Graphique sur le dephasage
pivot_data <- all_tp %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "name",
    values_to = "value"
  )

data_tp <- all_tp %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "name",
    values_to = "value"
  )%>% filter(kernel == "henderson") %>%
  unique_series_pivot() %>%
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "b['q, '] [Gamma]",
                         gain = "b['q, '] [G]",
                         phase = "b['q, '] [phi]"),
         variability = recode(variability,
                              lowvariability = "Faible variabilité",
                              mediumvariability = "Variabilité moyenne",
                              highvariability = "Forte variabilité")) %>%
  na.omit()
data_tp_rel <- all_tp %>%
  normalise_rev() %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "name",
    values_to = "value"
  )%>% filter(kernel == "henderson") %>%
  unique_series_pivot() %>%
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "b['q, '] [gamma]",
                         gain = "b['q, '] [G]",
                         phase = "b['q, '] [phi]",
                         auto_arima = "ARIMA"),
         variability = recode(variability,
                              lowvariability = "Faible variabilité",
                              mediumvariability = "Variabilité moyenne",
                              highvariability = "Forte variabilité")) %>%
  na.omit()
p = ggplot(data_tp ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + AQLTools:::theme_aqltools() +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())
p
ggsave("../slides/2022 - 09 - D2E/img/simul_phase_shift_norev.pdf",
       plot =p,
       width = 8,height = 5)
