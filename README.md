# ClimActor

Release version: 2.0.0

`ClimActor` is an `R` package created by the Data-Driven Envirolab team for the cleaning and preparation of subnational climate actors' names for further analysis. 
As more non-state (i.e., cities, regions, and companies) actors commit to climate action, new initiatives and databases recording such commitments have also become more 
commonplace. Many actors commit to multiple initiatives and appear in more than one database, yet appear across databases with slightly different names. This discrepancy 
makes data cleaning and wrangling more difficult than it should be and can result in over-counting of actor’s climate commitments if not dealt with appropriately.

The more recent version of ClimActor spatializes the subnational government entities data based on a comprehensive geodatabase of administrative entities, allowing climate data 
to be linked to other spatial datasets. Major revisions to package were implemented including new functions for matching under the spatialized logic and an extensive new key dictionary
built from a complete administrative entities geodatase. Previous functions and data is kept for compatibility purposes but user are encouraged to update their subnational government matching
using the new key_dict and function. Corporate matching can remain unchanged in this version but future updates will see an updated process for this as well.

## Citation and Data Access
If you use the **Climactor 2.0** database in your work, please cite the official archived version.
[![DOI](https://img.shields.io/badge/DOI-10.15139%2FS3%2FGGWVVQ-blue.svg)](https://doi.org/10.15139/S3/GGWVVQ)

Manya, Diego; Burley Farr, Katherine; Brown, Elizabeth; Martin, Andrew; Hsu, Angel, 2025, "Climactor 2.0: A spatialized database of subnational climate pledges and emissions data", https://doi.org/10.15139/S3/GGWVVQ, UNC Dataverse, V2.

## Installation
The `ClimActor` package can be installed from github using the `install_github` function from devtools. 

```{r} 
# Install devtools as necessary
# install.packages("devtools")

devtools::install_github("datadrivenenvirolab/ClimActor")
library(ClimActor)
```

## Use 

The vignette in this repository presents a recommended workflow for using the `ClimActor` package, and covers the usage and explanation of the different key functions. 

## Frequently Asked Questions

- I got an error while trying to install the package 

It is likely that the error is due to a missing package which is required for `ClimActor` but which you have not installed (especially if you are trying to install the vignette as well). Try checking the `DESCRIPTION` file for a list of required packages. 

- Do I have to follow the order of functions described in the vignette/flow diagram?

It is recommended that you follow of the order of the vignette/flow diagram as certain functions require outputs that would be created from previous functions.  

- Do I have to run all the functions within the package to clean my dataset? 

No - the functions are meant to be comprehensive and cover all key aspects of data cleaning while working with non-state climate actors. However, you may find that your dataset  have existing data which requires no cleaning, and thus will not require certain functions.   

## Bug Reports / Requests
Please file any bugs or requests for the package [here](https://github.com/datadrivenenvirolab/ClimActor/issues).
