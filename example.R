library(devtools)
devtools::install_github("taotliu/abcde")
library(abcde)

library(dplyr)
glimpse(iris)
summary_tab(iris)
n_perc(iris$Species == "setosa")
