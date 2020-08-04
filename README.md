# ClimActor

`ClimActor` is an `R` package created by the Data-Driven Envirolab team for the cleaning and preparation of subnational climate actors' names for further analysis. 
As more non-state (i.e., cities, regions, and companies) actors commit to climate action, new initiatives and databases recording such commitments have also become more 
commonplace. Many actors commit to multiple initiatives and appear in more than one database, yet appear across databases with slightly different names. This discrepancy 
makes data cleaning and wrangling more difficult than it should be and can result in over-counting of actor’s climate commitments if not dealt with appropriately.


## Installation
The `ClimActor` package can be installed from github using the `install_github` function from devtools. 

```{r} 
# Install devtools as necessary
# install.packages("devtools")

devtools::install_github("datadrivenyale/ClimActor", build_vignettes = T)
library(ClimActor)
```

It is recommended to build the package vignette during package installation. 

## Use 

The vignette presents a recommended workflow for using the `ClimActor` package, and covers the usage and explanation of the different key functions. 

```{r} 
browseVignettes("ClimActor")
```
## Frequently Asked Questions

- I got an error while trying to install the package 
It is likely that the error is due to a missing package which is required for `ClimActor` but which you have not installed (especially if you are trying to install the vignette as well). Try checking the `DESCRIPTION` file for a list of required packages. 
- Do I have to follow the order of functions described in the vignette/flow diagram?
It is recommended that you follow of the order 

## Bug Reports / Requests
Please file any bugs or requests for the package [here](https://github.com/datadrivenenvirolab/ClimActor/issues).
