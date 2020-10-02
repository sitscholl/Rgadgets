
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rgadgets

The goal of Rgadgets is to store useful functions and datasets that are
needed frequently for data analysis. It also provided helper functions
to download meteorological data from weather stations within South
Tyrol. All the functions included in the package start with the prefix
**rg** to make then distinct from functions within other packages.

## Installation

You can install the released version of Rgadgets from
[github](https://github.com/) with:

``` r
devtools::install_github('sitscholl/Rgadgets')
```

## Download meteorological data

Three helper functions are included that allow to download
meteorological measurements from various weather stations within South
Tyrol:

  - `rg_province_get()`
  - `rg_rebecka_get()`
  - `rg_br_get()`

All three function require the station id (**station\_code**), as well
as the starting (**datestart**) and ending date (**dateend**) as input
parameter. The functions `rg_province_get()` and `rg_br_get()` also
allow to specify the desired sensor (air temperature, precipitation…)
using the parameter **sensor\_code**. Data from the functions
`rg_rebecka_get()` and `rg_br_get()` is not available for all uses and
therefore require access credentials to access the data via the
parameters **private\_key** as well as **user**, **password** and
**host**.
