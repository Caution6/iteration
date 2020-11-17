Writing functions
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

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.45322429 -1.43504998 -0.72777681  0.77249844 -0.26997525 -0.58238440
    ##  [7] -0.98796509  0.12462405  2.88653201 -0.23795367  0.11326037  0.29898945
    ## [13]  0.25498612  0.71820542  0.94927087 -0.42786260 -0.05866971 -1.71930532
    ## [19]  1.02127401  1.27012128 -0.72741745 -1.60145426  1.04108686  0.15517979
    ## [25]  1.20019419 -0.69155104 -0.49238554 -0.96344291 -0.41485929  0.98505473

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -0.45322429 -1.43504998 -0.72777681  0.77249844 -0.26997525 -0.58238440
    ##  [7] -0.98796509  0.12462405  2.88653201 -0.23795367  0.11326037  0.29898945
    ## [13]  0.25498612  0.71820542  0.94927087 -0.42786260 -0.05866971 -1.71930532
    ## [19]  1.02127401  1.27012128 -0.72741745 -1.60145426  1.04108686  0.15517979
    ## [25]  1.20019419 -0.69155104 -0.49238554 -0.96344291 -0.41485929  0.98505473

Try my function on some other things. This should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric
