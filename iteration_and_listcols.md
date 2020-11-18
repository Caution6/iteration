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
    ## -2.40762 -0.63687  0.05517  0.06619  0.74962  2.49210

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
    ##  [1] 2.687914 2.282216 5.514663 3.088318 2.552463 1.972792 4.600958 3.206927
    ##  [9] 5.476334 3.446875 2.214758 2.816318 1.635765 3.099179 2.420124 2.733085
    ## [17] 3.021662 5.662223 3.043662 2.312235
    ## 
    ## $b
    ##  [1] -1.9709803  7.6199724 -7.8672919 -4.3715169  5.1353328  0.4275205
    ##  [7] -0.8000015  0.1484623 -2.3612242 -7.3723463 -5.0098565 -6.6636593
    ## [13]  1.5317749 -3.3932349 -1.4014796 -2.5989908 -0.3455948  0.8396004
    ## [19]  2.1331725 -2.2705133 -6.0634090 -8.7268290 -5.9657789  0.3381276
    ## [25] -5.2528056  0.1144882 -3.8572606  2.9438489  2.0131478  0.6054329
    ## 
    ## $c
    ##  [1] 10.055637 10.021137 10.216642  9.591024 10.180890 10.006323 10.079913
    ##  [8] 10.019462  9.759769  9.840003 10.146013  9.606461 10.099881  9.652900
    ## [15]  9.874129  9.985306 10.243540  9.562123  9.989596  9.741007  9.841763
    ## [22]  9.894771 10.243179 10.060408  9.763790 10.554112  9.944894 10.101712
    ## [29]  9.856246  9.463997  9.956478 10.246396  9.758393  9.857430 10.008659
    ## [36]  9.963523  9.766951 10.190820  9.849374  9.856334
    ## 
    ## $d
    ##  [1] -3.518466 -3.475875 -2.868376 -2.718558 -3.101646 -3.867742 -2.189328
    ##  [8] -3.580220 -3.401475 -4.128751 -3.647621 -1.882656 -3.883436 -2.823244
    ## [15] -2.099861 -2.189724 -3.029949 -3.041813 -3.664127  0.237232

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
    ## 1  3.19  1.19

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.75  3.92

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.95 0.222

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.94 0.989

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```
