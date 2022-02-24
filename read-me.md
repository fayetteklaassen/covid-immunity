# This repository contains code and scripts used for the analysis in the paper:

*Population immunity to pre-Omicron and Omicron SARS-CoV-2 variants in US states and counties through December 1, 2021*

Link to medRxiv

## Data
The raw data is available from published repositories.
All data sources are cited in the manuscript; and in the relevant scripts, links to the download pages are provided.

### data/data-sources

Contains the additional data-sources used: 
- fipspop.csv [population size by fips]
- fipsstate.csv [mapping of fips to states]
- statepop.csv [population size by state]
- fipspop-age.csv [fraction of population under 12y by fips]
- statepop-age.csv [fraction of population under 12y by state]

## Code 
**The code is arranged in the following folders**

### code/booster

Contains the code to impute county booster data previous to December 16, proportional to corresponding state booster data, as described in the eMethods.

### code/census12

Contains code to generate the fraction of under 12 years old by fips/state.

### code/HHPS

Contains the code to pre-process the HHPS survey data and to compute the state-specific odds ratios, used in the main analysis.

### code/IPSOS

Contains the code to pre-process the IPSOS survey data; used to validate the results.

### code/immunity

Contains the code for the final analysis; calculating the immunity and effective protection under waning and immune escape scenarios.
