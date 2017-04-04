# WindVerification
R Package to process and verify wind forecasts from weather models

## Installation

Install from GitHub with

```R
# install.packages("devtools")
devtools::install_github('bhlmn/WindVerification')
```

## Functions

**convert_units** -- convert wind speeds from one unit to another

**ndbc_10m** -- calculate 10-m winds for a vector of wind speed measurements according to Hsu et al. (1994)

**winds_10m** -- calculate 10-m winds for a vector of wind speed measurements using direction dependent surface roughness information
