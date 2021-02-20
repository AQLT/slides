# ipi_fr <- AQLTools::lectureBDM("010537946",
#                                "010537906",
#                      "010537908",
#                      "010537910",
#                      "010537912",
#                      "010537914",
#                      "010537940",
#                      "010537942")
# colnames(ipi_fr) <- c("CZ", paste0("C",c(1:5,"L1","L2")))
# saveRDS(ipi_fr, "2020 - 03 - NTTS/R/ipi_ntts_slides.RDS")
# fst_coef <- sapply(0:6,function(q){
#     f <- fstfilter(lags = 6, leads = q, pdegree=1,
#                    smoothness.weight=1/1001, timeliness.weight = 1000/1001)$filter
#     coef <- c(f, rep(0,6-q))
#     coef
# })
# colnames(fst_coef) <- colnames(rkhs$frf$weight)
# rownames(fst_coef) <- rownames(rkhs$frf$weight)
# saveRDS(fst_coef, "R/fst.RDS")

asymmetric_lp<-function(y,
                        horizon,
                        degree = 3,
                        kernel = c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"),
                        endpoints = c("LC", "QL", "CQ", "CC", "DAF", "CN"),
                        ic = 4.5,
                        q = 0,
                        tweight = 0, passband = pi/12){
    coef <- lpp_properties(horizon = horizon, degree = degree,
                           kernel = kernel, endpoints = endpoints,
                           ic = ic, tweight = tweight,
                           passband = passband)
    coef <- coef$filters.coef[,sprintf("q=%i",q)]
    jasym_filter(y, coef, horizon)
}
rkhs_apply <- function(y, component = c("frf", "gain", "phase"), q= 0){
    component=match.arg(component)
    coef <- rkhs[[component]]$weight[,sprintf("q=%i",q)]
    date_deb <- tail(time(y),1)
    additional_months <- ((length(coef)-1)/2-q)/frequency(y)
    if(additional_months != 0){
        y <- window(y, end = date_deb + additional_months ,
                    extend = TRUE)
        window(y, start = date_deb+1/frequency(y)) <- 0
    }
    window(stats::filter(y,
                         rev(coef)),
           end = date_deb)
}
fst_apply <- function(y, q= 0){
    coef <- fst_coef[,sprintf("q=%i",q)]
    date_deb <- tail(time(y),1)
    additional_months <- ((length(coef)-1)/2-q)/frequency(y)
    if(additional_months != 0){
        y <- window(y, end = date_deb + additional_months ,
                    extend = TRUE)
        window(y, start = date_deb+1/frequency(y)) <- 0
    }
    window(stats::filter(y,
                         rev(coef)),
           end = date_deb)
}
rkhs_full <- function(y, component = c("frf", "gain", "phase")){
    component=match.arg(component)
    coef <- rkhs[[component]]$weight
    date_deb <- tail(time(y),1)
    res <- lapply(0:6,function(q){
        coef <- coef[,sprintf("q=%i",q)]
        additional_months <- ((length(coef)-1)/2-q)/frequency(y)
        if(additional_months != 0){
            y <- window(y, end = date_deb + additional_months ,
                        extend = TRUE)
            window(y, start = date_deb+1/frequency(y)) <- 0
        }
        window(stats::filter(y,
                             rev(coef)),
               end = date_deb)
    })
    names(res) <- colnames(coef)
    final_serie <- res$`q=6`
    for(q in 5:0){
        current_date <- date_deb-q/frequency(y) 
        window(final_serie, start = current_date, end = current_date) <- 
            window(res[[sprintf("q=%i",q)]], start = current_date, end = current_date)
    }
    final_serie
}
fst_full <- function(y){
    coef <- fst_coef
    date_deb <- tail(time(y),1)
    
    res <- lapply(0:6,function(q){
        coef <- coef[,sprintf("q=%i",q)]
        additional_months <- ((length(coef)-1)/2-q)/frequency(y)
        if(additional_months != 0){
            y <- window(y, end = date_deb + additional_months ,
                        extend = TRUE)
            window(y, start = date_deb+1/frequency(y)) <- 0
        }
        window(stats::filter(y,
                             rev(coef)),
               end = date_deb)
    })
    names(res) <- colnames(coef)
    final_serie <- res$`q=6`
    for(q in 5:0){
        current_date <- date_deb-q/frequency(y) 
        window(final_serie, start = current_date, end = current_date) <- 
            window(res[[sprintf("q=%i",q)]], start = current_date, end = current_date)
    }
    final_serie
}


graph_glob <- function(data, titre = NULL, sous_titre = NULL, legende = NULL, afficheVolatilite = FALSE,
                       cex = 0.6, x_lab = NULL, x_lab_month = FALSE, y_lab = NULL,
                       outDec = ",",
                       n_xlabel = length(time(data)) %/% 24, n_ylabel = 12,
                       geom_size = 0.7,
                       ylimits = NULL){
    time <- time(data)
    freq <- frequency(data)
    dataGraph <- data.frame(cbind(round(time,3), data))
    if (is.null(legende)){
        if(is.mts(data)){
            legende <- colnames(data)
        }else{
            legende <- ""
        }
    }
    colnames(dataGraph) <- c("date", legende)
    breaks_x <- dataGraph$date[c(TRUE,FALSE)]
    
    dataGraph <- reshape2::melt(dataGraph, id="date")  # convert to long format
    
    if (freq==1){
        periode <- "Y"
        periode <- factor(periode)
    }
    if (freq==2){
        periode <- ifelse(time(data)%%1==0, "S1", "S2")
        periode <- factor(periode,levels = c("S1","S2"), ordered = T)
    }
    if (freq==4){
        periode <- capitalize(quarters(zoo::as.yearqtr(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(quarters(zoo::as.yearqtr((0:3)/4))),ordered = T)
    }
    if (freq==12){
        periode <- capitalize(months(zoo::as.yearmon(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(months(zoo::as.yearmon((0:11)/12))),ordered = T)
    }
    
    dataGraph <- data.frame(dataGraph,periode=periode)
    p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable,
                                      color = variable,
                                      linetype = variable,
                                      fill = variable
    )) + 
        scale_linetype_manual(values=c("solid",rep("dashed",ncol(data)-1)),
                              labels = legende)+
        geom_line(size=geom_size) +
        labs(title = titre, subtitle = sous_titre,
             x = x_lab, y = y_lab) +
        scale_x_continuous(breaks = breaks_x,
                           labels = function(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month, outDec = outDec)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
                           labels = function(x) format(x, decimal.mark = outDec),
                           limits = ylimits)+
        AQLTools:::theme_aqltools()
    p+
        scale_colour_manual(breaks = colnames(data),
                            values = c("#000000","#ADCDE6","#0072B2", "#D55E00", "#009E73")
        )+
        guides(color=guide_legend(ncol=ncol(dataGraph)-2)) +
        theme(legend.text.align = 0)
}

est_dynamique <- function(data_f,dates_est = 2009+(3:6-1)/12){
    res <- do.call(ts.union, lapply(dates_est, function(m_fin){
        data <- window(data_f, end = m_fin)
        y <- data[,"Symmetric filter"]
        n <- length(y)
        for(i in 0:5){
            y[n-i] <- data[n-i,paste0("q=",i)]  
        }
        y
    }))
    res <- ts.union(data_f[,"Symmetric filter"], res)
    colnames(res) <- c("Symmetric filter", as.character(as.yearmon(dates_est)))
    res
}


graph_glob <- function(data, titre = NULL, sous_titre = NULL, legende = NULL, afficheVolatilite = FALSE,
                       cex = 0.6, x_lab = NULL, x_lab_month = FALSE, y_lab = NULL,
                       outDec = ",",
                       n_xlabel = length(time(data)) %/% 24, n_ylabel = 12,
                       geom_size = 0.7,
                       ylimits = NULL){
    
    time <- time(data)
    freq <- frequency(data)
    dataGraph <- data.frame(cbind(time, data))
    if (is.null(legende)){
        if(is.mts(data)){
            legende <- colnames(data)
        }else{
            legende <- ""
        }
    }
    colnames(dataGraph) <- c("date", legende)
    
    dataGraph <- reshape2::melt(dataGraph, id="date")  # convert to long format
    
    if (freq==1){
        periode <- "Y"
        periode <- factor(periode)
    }
    if (freq==2){
        periode <- ifelse(time(data)%%1==0, "S1", "S2")
        periode <- factor(periode,levels = c("S1","S2"), ordered = T)
    }
    if (freq==4){
        periode <- capitalize(quarters(zoo::as.yearqtr(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(quarters(zoo::as.yearqtr((0:3)/4))),ordered = T)
    }
    if (freq==12){
        periode <- capitalize(months(zoo::as.yearmon(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(months(zoo::as.yearmon((0:11)/12))),ordered = T)
    }
    
    dataGraph <- data.frame(dataGraph,periode=periode)
    p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable,
                                      color = variable,
                                      linetype = variable,
                                      fill = variable
    )) + 
        scale_linetype_manual(values=c("solid",rep("dashed",ncol(data)-1)))+
        geom_line(size=geom_size) +
        labs(title = titre, subtitle = sous_titre,
             x = x_lab, y = y_lab) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
                           labels = function(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month, outDec = outDec)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
                           labels = function(x) format(x, decimal.mark = outDec),
                           limits = ylimits)+
        AQLTools:::theme_aqltools()
    p+
        scale_colour_manual(breaks = colnames(data),
                            values = c("#000000","#ADCDE6","#0072B2", "#D55E00", "#009E73")
        )+
        guides(color=guide_legend(ncol=5))
}
