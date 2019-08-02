library(devtools)
devtools::install_github("taotliu/abcde")
library(abcde)


library(dplyr)
glimpse(iris)
summary_tab(c("Sepal.Length", "Sepal.Width", "Petal.Width"), by = "Species", data = iris, test = T)
