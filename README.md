# Non-stationary Spatio-Temporal Modeling of COVID-19 Progression in The U.S.
This repository contains code to reproduce the results in ``Non-stationary Spatio-Temporal Modeling of COVID-19 Progression in The U.S.''.

## Simulation

Code to reproduce simulation results can be found in the [simulation.R](simulation.R). 

## Real data

#### Example Code
We provide an [Example](Example.pdf) of COVID-19 data analysis for US states.

#### State-Level 

Code to reproduce state-level results can be found in the [Covid-19-state.R](Covid-19-state.R)  and [Covid-19-state-prediction.R](Covid-19-state-prediction.R). 

Latest version of [State-level COVID-19 data](states-05-07.csv) can be found in the [New York times repository](https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv)


[State-level population data](co-est2019-alldata.csv) is extracted from [National Bureau of Economic Research](https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv)


[State-level distance data](sf12010statedistancemiles.csv) can be found in the [US Census Bureau](http://data.nber.org/distance/2010/sf1/state/sf12010statedistancemiles.csv)


#### County-Level 

Code to reproduce county-level results can be found in the [Covid-19-county.R](Covid-19-county.R)  and [Covid-19-county-prediction.R](Covid-19-county-prediction.R). 

Latest version of [County-level  COVID-19](counties-05-12.csv) data can be found in the [New York times repository](https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv)


[County-level population data](co-est2019-alldata.csv) is extracted from [National Bureau of Economic Research](https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv)


[County-level distance data](sf12010countydistance100miles.csv) can be found in the [US Census Bureau](http://data.nber.org/distance/2010/sf1/county/sf12010countydistance100miles.csv)
