# ABCDE - A personal R package 

An R package for my own R functions

## Installation

The latest version of the `abcde` package is available at GitHub [taotliu/abcde](https://github.com/taotliu/abcde). It requires the `devtools` package to be installed in R. If you do not have devtools in your R program, use the code `install.packages("devtools")` to install the devtools package first. Then run the following codes to install the `abcde` package.

```R

install.packages("devtools")
library(devtools)
devtools::install_github("taotliu/abcde")
library(abcde)
```

## Example

The following `R` code example demonstrates the use of the `abcde` package.

```R
library(dplyr)
glimpse(iris)
summary_tab(iris)

N = 150 
     Variable       Level        Median (IQR) or count (%) (N = 150)
[1,] "Sepal.Length" ""           "5.80 (5.10, 6.40)"                
[2,] "Sepal.Width"  ""           "3.00 (2.80, 3.30)"                
[3,] "Petal.Length" ""           "4.35 (1.60, 5.10)"                
[4,] "Petal.Width"  ""           "1.30 (0.30, 1.80)"                
[5,] "Species"      "setosa"     "50 (33.33\\%)"                    
[6,] ""             "versicolor" "50 (33.33\\%)"                    
[7,] ""             "virginica"  "50 (33.33\\%)"          

```
