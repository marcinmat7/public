plot(ecdf(iris$Sepal.Length))
setwd("C:/Users/oem/Desktop/HTML/img")
i <- 1
for (i in 1:4){
    png(paste0(colnames(iris)[i], ".png"))
    plot(ecdf(iris[, colnames(iris)[i]]))
    dev.off()
}

fileName <- '../template.txt'

for(i in 1:4){
    assign(x = paste0("plot_", i), 
           value = paste0("Zmienna: ", 
                          colnames(iris)[i],
                          "</br> \n <img src = \"img/",
                          colnames(iris)[i], 
                          ".png \"> </br>"))
    
}
plot_1
results_txt <- NULL
for(i in 1:4){
results_txt <- paste0(results_txt, get(paste0("plot_", i)))
}

template <- '../template.txt'
template_txt <- readChar(template, file.info(fileName)$size)

template_txt_2 <- gsub(pattern = "@@", replacement = results_txt, x = template_txt)

cat(template_txt_2, file = "../template_txt_3.html")


Przykladowy tekst </br>
    <img src="img/saturn.png">


