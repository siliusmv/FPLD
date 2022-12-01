
This repository contains the code used for creating all the results in the paper [Quantile based modeling of diurnal temperature range with the five-parameter lambda distribution](https://doi.org/10.1002/env.2719),
which performs modelling of diurnal temperature range in Norway using the FPL distribution.

## Installation

You can install the package directly from github:

``` r
devtools::install_github("siliusmv/FPLD")
```

## Scripts

There are a number of scripts available in the `exec/` folder. These are described below.

### Master-script

The script `master.R` runs all the code in its intended order.

### Download the distance to sea

Distance to the open sea is used as an explanatory variable in our regression model. The script
`download_data.R` downloads a DEM and computes the distance to sea using that.

### Simulation studies

The script `univariate_simulation_study.R` performs a simulation study where we test the method of
quantiles against the starship method and the maximum likelihood method.

### Data preparation

Data preparation is performed in the `prepare_data.R` script.

### Data exploration

In the script `explore_data.R`, we examine the diurnal temperature range data and how it is linked
to the available explanatory variables.

### Univariate modelling with the FPL distribution
The script `local_estimation.R` performs univariate modelling of diurnal temperature range using the
FPL distribution. The script `display_local_estimation_results.R` evaluates the model fit.

### Regression-modelling with the FPL distribution
The script `regional_estimation.R` performs regression-modelling of diurnal temperature range using
a combination of quantile regression and FPL marginal distributions.
The script `display_regional_estimation_results.R` evaluates the model fit.

## Data

Data from all our chosen weather stations is located in the directory `inst/extdata`
