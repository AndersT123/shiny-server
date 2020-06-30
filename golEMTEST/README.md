
<!-- README.md is generated from README.Rmd. Please edit that file -->

# golEMTEST

<!-- badges: start -->

<!-- badges: end -->

The goal of golEMTEST is to let you comptete with your friends in
predicting the results of sports events.

Use the package by completing the following steps:

1.  Install the package **ADD REFEENCES**
2.  Create a league
3.  Add a data source
4.  Have fun competing with your friends in predicting results for
    sports events

### Create a league

Use `create_league_r()` or `create_league_cmd()` to set up a directory
structure to store user provided predictions. `create_league()_r` is run
from `R` and `create_league_cmd()` is run from the terminal using
`R_CMD...`

### Add a data source

You should specify a data source for your app. Whatever you specify
should just return a tibble with the following fields …

Install golEMTEST as a package. Call a function that sets up the
webscraper into the global.R file. Use golem::set\_golem

Either a csv file R program that scrapes some results

## Installation

You can install the released version of golEMTEST from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("golEMTEST")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
#devtools::install_github("AndersT123/shiny-server")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(golEMTEST)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
