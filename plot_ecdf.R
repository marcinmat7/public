setwd("C:/Users/garri/Desktop/funkcje_R")
data <- read.csv(file = "framingham.csv")


create_hist <- function(x, title) {
    
    library(ggplot2)
    ceil <- quantile(x = x, probs = 0.999, na.rm = T)
    x[x > ceil] <- ceil
    
    # tmp <- data
    n <- 50
    wid <- (max(x, na.rm = T) - min(x, na.rm = T)) / n
    breaks <- c(-Inf, min(x, na.rm = T) + 1:n * wid, Inf)
    nas <- sum(is.na(tmp$x))
    tmp <- data.frame(x)
    tmp$aggd <- cut(x, breaks = breaks, labels = breaks[1:(length(breaks) - 1)])
    tmp$aggd <- as.numeric(as.character(tmp$aggd))
    tmp$na <- F
    tmp <- rbind(tmp, data.frame(x = rep(0, times = nas), 
                                 aggd = rep(min(tmp$aggd[is.finite(tmp$aggd)], na.rm=T) - 3 * wid, times = nas),
                                 na = rep(T, times = nas)))
    
    maxy <- max(table(tmp$aggd))
    
    tmp1 <- tmp
    maxy1 <- maxy
    table(tmp1$aggd)
    # df_nulls <- data.frame(nulls = rep(breaks[2] - 2 * wid, times = sum(is.na(x))))
    # # ?geom_bar
    p1_chol <- ggplot(tmp1, aes(x = aggd)) + geom_bar(aes(y = (..count..) / maxy1, fill = na), position = 'dodge', width = 5) + 
        scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) + 
        # geom_bar(data = df_nulls, aes(x = nulls, y = (..count..) / maxy1), position = 'dodge', fill = "red") +
        xlab(title) + 
        ylab("CDF") + 
        theme_linedraw(base_size = 11, base_family = "",
                       base_line_size = 22/22, base_rect_size = 22/22) +
        
        # scale_x_continuous() + 
        stat_ecdf(aes(x = x), color = "red", size = 1.5)  +
        scale_y_continuous(sec.axis=sec_axis(trans = ~.*maxy1, name = "Quantity")) + 
        labs(title = title) + theme_bw() + theme(legend.position="none")
    
    return(p1_chol)
    
}   

create_hist(x = data$totChol, title = "Wiek")
    
