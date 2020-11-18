Iteration and listcols
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## √ ggplot2 3.3.2     √ purrr   0.3.4
    ## √ tibble  3.0.4     √ dplyr   1.0.2
    ## √ tidyr   1.1.2     √ stringr 1.4.0
    ## √ readr   1.4.0     √ forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)


scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

You can put anything in the list.

``` r
l = list(
vec_numeric = 5:8,
vec_logic = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
mat = matrix(1:8, nrow = 2, ncol = 4),
summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logic
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.35463 -0.70906  0.03727  0.03813  0.76241  2.55926

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 1.677671 4.000436 2.899221 3.215497 2.468868 3.996422 4.433060 2.870975
    ##  [9] 4.835942 1.791532 2.510825 4.678328 4.995969 3.097861 3.433392 3.853860
    ## [17] 5.322037 2.625227 2.473162 5.687328
    ## 
    ## $b
    ##  [1]  -7.84430249  -3.98012341  -0.54435979  -5.41628648  -4.63882655
    ##  [6] -15.48157516  -5.57304133  -3.98462377   1.62674018   9.95432222
    ## [11]  -0.38951885  -4.03613742  -0.41562545   0.03649932  -2.29586381
    ## [16]   5.64355451   0.27704155   0.67780022 -10.42890830  -6.80326268
    ## [21]  -3.18025091   2.32611237  -0.31371622  -9.05873187   4.43765217
    ## [26]  -0.46173088   2.37271744   5.02242310  -3.01356136  -1.27866938
    ## 
    ## $c
    ##  [1] 10.232937  9.857082  9.956340  9.826485 10.179567  9.721117 10.133404
    ##  [8]  9.829698  9.967871  9.743200 10.159630  9.807153  9.895860 10.019089
    ## [15] 10.014806 10.195313 10.028511  9.951934 10.108314 10.170381  9.692530
    ## [22] 10.044656 10.111415  9.832667  9.842172 10.232301  9.925563 10.175405
    ## [29]  9.750638 10.137055  9.910057 10.120882  9.728641  9.514712  9.925760
    ## [36] 10.052011 10.166091 10.133684  9.729598  9.753604
    ## 
    ## $d
    ##  [1] -2.13272105 -2.96716455 -3.86164574  0.02060324 -2.94981154 -3.32673275
    ##  [7] -4.73466632 -3.76736393 -4.73333266 -3.22252809 -2.02804766 -1.62362035
    ## [13] -2.73237351 -4.73885218 -2.47630966 -4.32405497 -4.19800229 -5.45467098
    ## [19] -2.46024165 -3.15602950

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.54  1.17

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.89  5.18

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.182

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.24  1.30

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

Let’s try map\!

``` r
output = map(list_norm, mean_and_sd)
```

What if you want a different function

``` r
output = map(list_norm, median)
```
