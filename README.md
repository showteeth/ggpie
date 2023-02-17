
<!-- README.md is generated from README.Rmd. Please edit that file -->
# ggpie - A ggplot2 extension to create pie, donut and rose pie plot

<img src = "man/figures/ggpie_hex.png" align = "right" width = "200"/>

[![CRAN](https://www.r-pkg.org/badges/version/ggpie?color=orange)](https://cran.r-project.org/package=ggpie) [![CRAN\_download](http://cranlogs.r-pkg.org/badges/grand-total/ggpie?color=blue)](https://cran.r-project.org/package=ggpie) ![License](https://img.shields.io/badge/license-MIT-green) [![CODE\_SIZE](https://img.shields.io/github/languages/code-size/showteeth/ggpie.svg)](https://github.com/showteeth/ggpie) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6580728.svg)](https://doi.org/10.5281/zenodo.6580728)

## Introduction

`ggpie` aims to create **pie (2D and 3D)**, **donut** and **rose pie** plot with the `ggplot2` plotting system which implemented the grammar of graphics. It contains five main functions:

-   `ggpie`: Create 2D pie plot with single group variable.
-   `ggdonut`: Create 2D donut plot with single group variable.
-   `ggnestedpie`: Create 2D nested pie plot with two group variables.
-   `ggpie3D`: Create 3D pie plot with single group variable.
-   `ggrosepie`: Create rose pie plot with single or two group variables.

<hr />

## Installation

You can install the released version of `ggpie` from [CRAN](https://cran.r-project.org/package=ggpie) with:

``` r
install.packages("ggpie")
```

Or install the package via the [Github repository](https://github.com/showteeth/ggpie):

``` r
# install.package("remotes")   #In case you have not installed it.
remotes::install_github("showteeth/ggpie")
```

In general, it is **recommended** to install from [Github repository](https://github.com/showteeth/ggpie) (update more timely).

<hr />

## Citation

``` r
citation("ggpie")
#> 
#> To cite ggpie in publications use:
#> 
#>   Yabing Song (2022). ggpie: Create Pie, Donut and Rose Pie Plot with
#>   'ggplot2'. R package version 0.2.5.
#>   https://CRAN.R-project.org/package=ggpie
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ggpie: Create Pie, Donut and Rose Pie Plot with 'ggplot2'.},
#>     author = {Yabing Song},
#>     note = {R package version 0.2.5},
#>     year = {2022},
#>     url = {https://CRAN.R-project.org/package=ggpie},
#>   }
```

<hr />

## Contributing

Please note that the `ggpie` project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/). By contributing to this project, you agree to abide by its terms.

<br />
