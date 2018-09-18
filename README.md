
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- [![Travis-CI Build Status](https://travis-ci.org/sflippl/vdem.tectr.svg?branch=master)](https://travis-ci.org/sflippl/vdem.tectr)[![Coverage Status](https://img.shields.io/codecov/c/github/sflippl/vdem.tectr/master.svg)](https://codecov.io/github/sflippl/vdem.tectr?branch=master) -->
vdem.tectr
==========

vdem.tectr provides access to Version 8 of the [V-Dem dataset](v-dem.net) (Oppedge et al. 2018, Pemstein et al. (2018)). It contains:

-   `df_vdem`, the imported V-Dem dataset
-   `vdem_spatial` and `vdem_geocode`, which geocode the countries in the V-Dem dataset
-   `vdem`, `df_vdem` equipped with a metaframe which uses effective explicitness from the package [`tectr`](github.com/sflippl/tectr) to asssist in visualizations and descriptions of variables.

Installation
------------

You can install vdem.tectr from github with:

``` r
# install.packages("devtools")
devtools::install_github("sflippl/vdem.tectr")
```

Oppedge, Michael, John Gerring, Staffan I. Knutsen, Carl Henrik Lindberg, Svend-Erik Skaaning, Jan Teorell, David Altman, Michael Bernhard, et al. 2018. “V-Dem Country-Year Dataset v8.” Varieties of Democracy (V-Dem) Project.

Pemstein, Daniel, Kyle L. Marquardt, Eitan Tzelgov, Yi-ting Wang, Joshua Krusell, and Farhad Miri. 2018. “The V-Dem Measurement Model: Latent Variable Analysis for Cross-National and Cross-Temporal Expert-Coded Data.” University of Gothenburg: Varieties of Democracy Institute.
