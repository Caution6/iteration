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
    ## -2.27518 -0.65217  0.01380  0.03197  0.70902  2.88728

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
    ##  [1] 3.0592137 4.2943086 2.5743301 2.4777029 0.8150513 1.9980005 2.5831992
    ##  [8] 3.0716836 3.3559796 3.5535169 1.5816002 3.3850558 1.6061639 2.5880516
    ## [15] 2.6428574 2.5841177 2.8250633 2.4318007 4.6032081 3.7706227
    ## 
    ## $b
    ##  [1]  -3.6152716   7.0684180  -1.5500891  -0.4737763   1.5469671   5.2987634
    ##  [7]   6.4998476  -2.5634489   3.1557542  -3.4855448  -0.2800379   2.5205696
    ## [13]   5.0450379   0.4769339   4.3848379 -12.0654627   7.4694744 -10.6854107
    ## [19]  -5.0314118   3.0231850  12.3986199   2.3997862   6.3831897  -2.1101833
    ## [25]  -2.4900950  -7.7767657 -10.9608006  -0.4947282  -5.0800883  -5.5022133
    ## 
    ## $c
    ##  [1]  9.971381  9.766636  9.856896  9.953298 10.095749 10.361247 10.150290
    ##  [8] 10.136545  9.734752 10.205521  9.964118 10.068472 10.344550 10.077384
    ## [15]  9.647977  9.947260 10.073395 10.118461 10.062278 10.052310  9.820006
    ## [22]  9.965510  9.395520 10.040812 10.256841  9.922718  9.842249 10.286815
    ## [29]  9.894205  9.868558  9.924611  9.826903 10.076228 10.012673 10.263519
    ## [36]  9.870256  9.737721  9.791641 10.012046  9.974506
    ## 
    ## $d
    ##  [1] -2.229844 -3.661125 -4.398508 -3.166601 -3.398941 -1.604720 -4.087465
    ##  [8] -4.625220 -3.074421 -4.245445 -5.715009 -2.398373 -2.252752 -2.939538
    ## [15] -3.869754 -3.294651 -2.361381 -3.664676 -1.948691 -3.635391

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
    ## 1  2.79 0.910

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.216  5.92

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.196

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.33  1.02

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
output = map_dbl(list_norm, median, .id = "input")
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns\!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% 
  pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% 
  pull(samp)
```

    ## $a
    ##  [1] 3.0592137 4.2943086 2.5743301 2.4777029 0.8150513 1.9980005 2.5831992
    ##  [8] 3.0716836 3.3559796 3.5535169 1.5816002 3.3850558 1.6061639 2.5880516
    ## [15] 2.6428574 2.5841177 2.8250633 2.4318007 4.6032081 3.7706227
    ## 
    ## $b
    ##  [1]  -3.6152716   7.0684180  -1.5500891  -0.4737763   1.5469671   5.2987634
    ##  [7]   6.4998476  -2.5634489   3.1557542  -3.4855448  -0.2800379   2.5205696
    ## [13]   5.0450379   0.4769339   4.3848379 -12.0654627   7.4694744 -10.6854107
    ## [19]  -5.0314118   3.0231850  12.3986199   2.3997862   6.3831897  -2.1101833
    ## [25]  -2.4900950  -7.7767657 -10.9608006  -0.4947282  -5.0800883  -5.5022133
    ## 
    ## $c
    ##  [1]  9.971381  9.766636  9.856896  9.953298 10.095749 10.361247 10.150290
    ##  [8] 10.136545  9.734752 10.205521  9.964118 10.068472 10.344550 10.077384
    ## [15]  9.647977  9.947260 10.073395 10.118461 10.062278 10.052310  9.820006
    ## [22]  9.965510  9.395520 10.040812 10.256841  9.922718  9.842249 10.286815
    ## [29]  9.894205  9.868558  9.924611  9.826903 10.076228 10.012673 10.263519
    ## [36]  9.870256  9.737721  9.791641 10.012046  9.974506
    ## 
    ## $d
    ##  [1] -2.229844 -3.661125 -4.398508 -3.166601 -3.398941 -1.604720 -4.087465
    ##  [8] -4.625220 -3.074421 -4.245445 -5.715009 -2.398373 -2.252752 -2.939538
    ## [15] -3.869754 -3.294651 -2.361381 -3.664676 -1.948691 -3.635391

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.79 0.910

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.216  5.92

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.79 0.910
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.216  5.92
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.196
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.33  1.02

So … can I add a list column?

``` r
listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median)
  )
```

    ## # A tibble: 4 x 4
    ##   name  samp         summary          medians
    ##   <chr> <named list> <named list>       <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 x 2]>   2.62 
    ## 2 b     <dbl [30]>   <tibble [1 x 2]>  -0.377
    ## 3 c     <dbl [40]>   <tibble [1 x 2]>   9.97 
    ## 4 d     <dbl [20]>   <tibble [1 x 2]>  -3.35
