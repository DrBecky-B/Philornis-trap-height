Data and code for 'Using modified trapping regimes to understand the spatial and nutritional ecology of the avian vampire fly, Philornis downsi.'
This read me provides details about the headers in each of the files in this repository:

File name: Code-P.downsi-v1-24.5.22.R: this is the R code used to run the analyses and make the figures in the manuscript

File name: Q1.Q3-T3T4-weather-amalg.March.2021.txt: this is the .txt file containing all data including weather variables. The dataset is used to make table 1 and figures 1 and 2.
  Date: Date when traps laid out (collected after 48 hours), format DD/MM/YY
  Temp: Temperature (°C) over the 48 hour period for which the trap was laid out
  Precip: Rainfall (mm) over the 48 hour period for which the trap was laid out
  Humidity: % relative humidity over the 48 hour period for which the trap was laid out
  Trap: Categorical factor for trap height (A-E; lowest to highest)
  Trap.Height: Factor for trap height (exact in m)
  Height: continuous variable of trap height (how high each trap was from the ground in metres)
  FPD: number of female P. downsi collected in that trap over the 48 hour trapping period
  MPD: number of male P. downsi collected in that trap over the 48 hour trapping period
  P.downsi: number of total Philornis downsi (male and female) collected in that trap over the 48 hour trapping period
  Moths: total number of moths collected in that trap over the 48 hour trapping period
  Diptera: total number of other Diptera collected in that trap over the 48 hour trapping period
  P..versocolor: total number of Polistes versicolor wasps collected in that trap over the 48 hour trapping period
  TrapID: identity of each individual trap on each pole included as a random effect in models
  Bait: bait type (either PAPAYA or YEAST)
  WoY: Week of year (numeric), used for gamms
  Study.week: Week of study (numeric) for use in gamms to model season
  OLRE: Observation level random effect (for accounting for overdispersion in models)
  
File name: SR.txt
  Trap: Trap (as factor; A-E from lowest to highest)
  Bait: Bait type (PAPAYA OR YEAST)
  sum: total number of male and female P.downsi counted (by trap height and bait type)
  binomial_CI.method: method used to calculate binomial confidence intervals (logit)	
  binomial_CI.x: number of female P. downsi counted (by trap height and bait type)
  binomial_CI.n: number of male P. downsi counted (by trap height and bait type)
  binomial_CI.mean: mean sex ratio
  binomial_CI.lower: lower binomial CI for sex ratio
  binomial_CI.upper: upper binomial CI for sex ratio
  
File name: SR.temp.txt
  Temp: Temperature (°C) over the 48 hour period for which the trap was laid out
  sum: total number of male and female P.downsi counted (by trap height and bait type)
  binomial_CI.method: method used to calculate binomial confidence intervals (logit)	
  binomial_CI.x: number of female P. downsi counted (by trap height and bait type)
  binomial_CI.n: number of male P. downsi counted (by trap height and bait type)
  binomial_CI.mean: mean sex ratio
  binomial_CI.lower: lower binomial CI for sex ratio
  binomial_CI.upper: upper binomial CI for sex ratio
  
File name: Sex.gamm.3.4: this is the .txt file containing all data to test for seasonal differences in trap rates of male and female Philornis downsi
  Date: Date when traps laid out (collected after 48 hours), format DD/MM/YY
  PD: Number of P. downsi (either male or female) counted in each trap on each day
  Sex: Sex of fly (MALE OR FEMALE)
  TrapID: identity of each individual trap on each pole included as a random effect in models
  WoY: Week of year (numeric), used for gamms to model season
  OLRE: Observation level random effect (for accounting for overdispersion in models)
