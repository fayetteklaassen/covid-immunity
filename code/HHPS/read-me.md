# code/HHPS contents

In order of relevance

## *gen_HHPS.R*

file to pre-process the HHPS survey data: 
- recodes variables
- filters excluded categories
- computed effective sample size in each category
Data downloaded from: 

https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html

--> download the csv zip link; 
use the file with name pulse2021_puf_xx.csv

Data files of 2021: Week 24 up to and including Week 36 (end of August)


## *gen_logor.R*

Script to calculate the state-specific odds-ratios 

### *ORstan.stan*

The stan script to run the logistic regression