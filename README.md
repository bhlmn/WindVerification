# WindVerification
R Package to process and verify wind forecasts from weather models

## Installation

Install from GitHub with

```R
# install.packages("devtools")
devtools::install_github('bhlmn/WindVerification')
```

## Current Functions

**convertunits** - convert wind speeds from one unit to another

**getbae** - calculate the bivariate absolute error for a forecast/observation wind vector pair

**getbmae** - calculate bivariate mean absolute error over many forecast/observation wind vector pairs

**getmvrank** - calculate a multivariate rank given EMOS parameters (to populate a multivariate rank histogram)

**getenorm** - calculate the two dimensional Eucledian norm

**getesapprox** - perform an energy score approximation given bivariate PDF parameters

**getes** - calculate the energy score for a discrete ensemble

**getmvrank** - calculate a multivariate rank given a discrete ensemble (to populate a multivariate rank histogram)

**getuv** - convert wind speed and wind direction into u and v wind components

**getwspdwdir** - convert u and v wind components into wind speed and wind direction

**getwdirerror** - calculate the wind direction error of a forecast/observation pair

**ndbc10m** - calculate 10-m winds for a vector of wind speed measurements according to Hsu et al. (1994)

**winds10m** - calculate 10-m winds for a vector of wind speed measurements using direction dependent surface roughness information

## Future Functions
