load_simul <- function(x){
  data <- readRDS(sprintf("../turningPointsDetection/data_simul/byseries/%s.RDS", x))
  data <- data[[length(data)]]
  data
}
data = cbind("lowvariability2" |> load_simul(),
         "mediumvariability2" |> load_simul(),
         "highvariability2" |> load_simul())
colnames(data) <- paste("Variabilité", c("faible", "moyenne", "forte"))

library(ggplot2)
ggsave(filename = "2022 - 09 - D2E/img/simul_data.pdf",
       AQLTools::graph_ts(data,n_xlabel = 10),
       width = 8, height = 5)
