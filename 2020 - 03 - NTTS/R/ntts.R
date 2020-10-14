library(RJDemetra)
library(rjdfilters)
library(patchwork)
library(ggplot2)
library(zoo)
library(Hmisc)
setwd("2020 - 03 - NTTS")
source(file = "R/functions.R",encoding = "UTF-8")

ipi_fr <- readRDS("R/ipi_ntts.RDS")
AQLTools::hc_stocks(ipi_fr)
rkhs <- readRDS("R/rkhs.RDS")
fst_coef <- readRDS("R/fst.RDS")

y = ipi_fr[,"CZ"]

data_daf <- ts.union( asymmetric_lp(y,6, ic = 3.5, q = 6),
                      asymmetric_lp(y,6, ic = 3.5, q =0,endpoints = "DAF"),
                      asymmetric_lp(y,6, ic = 3.5, q =1,endpoints = "DAF"),
                      asymmetric_lp(y,6, ic = 3.5, q =2,endpoints = "DAF"),
                      asymmetric_lp(y,6, ic = 3.5, q =3,endpoints = "DAF")
)
data_lc <- ts.union( asymmetric_lp(y,6, ic = 3.5, q = 6),
                     asymmetric_lp(y,6, ic = 3.5, q =0,endpoints = "LC"),
                     asymmetric_lp(y,6, ic = 3.5, q =1,endpoints = "LC"),
                     asymmetric_lp(y,6, ic = 3.5, q =2,endpoints = "LC"),
                     asymmetric_lp(y,6, ic = 3.5, q =3,endpoints = "LC")
)
data_ql <- ts.union( asymmetric_lp(y,6, ic = 3.5, q = 6),
                     asymmetric_lp(y,6, ic = 3.5, q =0,endpoints = "QL"),
                     asymmetric_lp(y,6, ic = 3.5, q =1,endpoints = "QL"),
                     asymmetric_lp(y,6, ic = 3.5, q =2,endpoints = "QL"),
                     asymmetric_lp(y,6, ic = 3.5, q =3,endpoints = "QL")
)

data_rkhs_phase <- ts.union( asymmetric_lp(y,6, ic = 3.5, q = 6),
                             rkhs_apply(y,"phase", q =0),
                             rkhs_apply(y,"phase", q =1),
                             rkhs_apply(y,"phase", q =2),
                             rkhs_apply(y,"phase", q =3)
)
data_fst <- ts.union(asymmetric_lp(y,6, ic = 3.5, q = 6),
                     fst_apply(y, q =0),
                     fst_apply(y, q =1),
                     fst_apply(y, q =2),
                     fst_apply(y, q =3)
)

colnames(data_daf) <- colnames(data_lc) <-
  colnames(data_ql) <-colnames(data_rkhs_phase)<-colnames(data_fst) <- 
  c("Symmetric filter", paste0("q=",0:3))
AQLTools::hc_stocks(window(data_daf, start = 2019),digits = 1)
AQLTools::hc_stocks(window(data_lc, start = 2019),digits = 1)
AQLTools::hc_stocks(window(data_ql, start = 2019),digits = 1)
AQLTools::hc_stocks(window(data_rkhs_phase, start = 2019),digits = 1)


data_lpp <- ts.union(y,
                     localpolynomials(y,6, ic = 3.5,endpoints = "DAF"),
                     localpolynomials(y,6, ic = 3.5,endpoints = "LC"),
                     localpolynomials(y,6, ic = 3.5,endpoints = "QL"),
                     rkhs_full(y, "phase"),
                     fst_full(y))
colnames(data_lpp) <- c("ipi","DAF", "LC", "QL", "b['q, '] [phi]", "FST")
AQLTools::hc_stocks(window(data_lpp, start = 2019),digits = 1)



data_all <- list("DAF" = data_daf,
                 "LC" = data_lc,
                 "QL" = data_ql,
                 "b['q, '] [phi]" = data_rkhs_phase,
                 "FST" = data_fst)
AQLTools::hc_stocks(data_all$FST,digits = 1)
AQLTools::hc_stocks(data_all$`b['q, '] [phi]`,digits = 1)
AQLTools::hc_stocks(window(data_all$FST,start = 2019),digits = 1)
AQLTools::hc_stocks(window(data_all$`b['q, '] [phi]`,start = 2019),digits = 1)



ylim1 <- range(sapply(data_all, function(data) range(window(data,start = 2019),na.rm=TRUE)))
ylim2 <- range(sapply(data_lpp, function(data) range(window(data,start = 2019),na.rm=TRUE)))

Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8") 
for (i in seq_along(data_all)){
  data1 <- data_all[[i]]
  data2 <- data_lpp[,c("ipi", colnames(data_lpp)[i+1])]
  p <- graph_glob(window(data1,start = 2019),n_xlabel = 8,x_lab_month = TRUE,
                  titre = parse(text=sprintf("%s~~filter", names(data_all)[i])),
                  ylimits = ylim1) /
    graph_glob(window(data2,start = 2019),n_xlabel = 8,x_lab_month = TRUE,
               ylimits = ylim2,
               legende = c("Published IPI",
                           parse(text=sprintf("Trend-cycle~estimation~(%s)",names(data_all)[i]))
                           )
               )
  ggsave(filename = sprintf("img/illustrationfr_%i.pdf",i),
         p,
         width = 8, height = 6)
}
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

q <- 0
c("q=0", "q=1", "q=2","q=3","Symmetric filter")
revisions <- lapply(data_all, function(data1){
  print(i)
  data <- window(data1,start = 2020)
  res <- sapply(0:3,function(q){
    res <- colMeans((data-data[, q+2])^2,na.rm = TRUE)
    names(res) <- colnames(data)
    res
  })
  colnames(res) <- c("q=0", "q=1", "q=2","q=3")
  round(res,1)
})
revisions
