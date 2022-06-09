library(RJDemetra)
res = x13(ipi_c_eu[,"FR"])
data = window(round(res$final$series,1), start = 2010)[,c("y","t","s","i")]
data = data.frame(x = time(data), data)
data$s <- data$t + data$s
data$i <- data$t + data$i
cat(c(sprintf("\\addplot+[mark=none] coordinates {%s};",
              paste(sprintf("(%s,%s)",round(data$x,3), data$y), collapse = " ")
),
sprintf("\\addplot+[mark=none] coordinates {%s};",
        paste(sprintf("(%s,%s)",round(data$x,3), data$t), collapse = " ")
),
sprintf("\\addplot+[mark=none] coordinates {%s};",
        paste(sprintf("(%s,%s)",round(data$x,3), data$s), collapse = " ")
),
sprintf("\\addplot+[mark=none] coordinates {%s};",
        paste(sprintf("(%s,%s)",round(data$x,3), data$y), collapse = " ")
)),sep = "\n")
