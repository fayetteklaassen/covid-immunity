# code/immunity

## Main scripts

`runImmunity.R`

script to render the immunity estimates for states and counties.

`immunity.R`

The function that calculates the immunity estimates and 
applies waning and immune evasion functions to compute the effective protection

## Helper files

`prepData.R`
selects the necessary variables in the raw and analyzed data frames;
prepares the data files for submission to immunity

`functions.R`
helper functions for `prepData.R` and for `immunity.R`

`waning-functions.R`
function that generates the waning functions, as described in the manuscript

`omicron-evasion.R`
function that generates the omicron evasion scenarios, as described in the manuscript.