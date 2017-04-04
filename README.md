# WindVerification
R Package to process and verify wind forecasts from weather models

## Installation

Install from GitHub with

```R
# install.packages("devtools")
devtools::install_github('bhlmn/WindVerification')
```

## Current Functions

**convertUnits** - convert wind speeds from one unit to another

**getUV** - convert wind speed and wind direction into u and v wind components

**getwspdwdir** - convert u and v wind components into wind speed and wind direction

**ndbc10m** - calculate 10-m winds for a vector of wind speed measurements according to Hsu et al. (1994)

**winds10m** - calculate 10-m winds for a vector of wind speed measurements using direction dependent surface roughness information

## Future Functions

**approxES** - perform an energy score approximation given bivariate PDF parameters

**getbAE** - calculate the bivariate absolute error for a forecast/observation wind vector pair

**getbMAE** - calculate bivariate mean absolute error over many forecast/observation wind vector pairs

**getENorm** - calculate the two dimensional Eucledian norm

**getES** - calculate the energy score for a discrete ensemble

**getMVRank** - calculate a multivariate rank (to populate a multivariate rank histogram)
