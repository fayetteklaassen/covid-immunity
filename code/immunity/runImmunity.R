## Run 
library(tidyverse)
source("code/immunity/omicron-evasion.R")
source("code/immunity/waning-functions.R")
source("code/immunity/functions.R")
source("code/immunity/immunity.R")

maxDate <- as.Date("2021-12-01")

## Counties 
scn.omi.df <- gen_scn_omi()
waneScn    <- list("basecase" = gen_waning()[["basecase"]])

vaxD <- read_csv("data/analyzed/immunity/vax-counties.csv")
infD <- read_csv("data/analyzed/immunity/infections-counties.csv")
boostD <- read_csv("data/analyzed/immunity/boost-counties.csv") 
popD <- read_csv("data/data-sources/fipspop.csv")
pop12D <- read_csv("data/data-sources/fipspop-age.csv")
oddsD <- read_csv("data/analyzed/immunity/logor-counties.csv")

cnt <- calcImmunity(vaxD, infD, boostD, oddsD, popD, pop12D, 
                 state = FALSE, maxDate = maxDate, 
                 waneScn = waneScn, 
                 scn.omi.df = scn.omi.df)
write_csv(cnt, "data/results/results-counties.csv")

## States
scn.omi.df <- gen_scn_omi()
waneScn    <- gen_waning()

vaxD <- read_csv("data/analyzed/immunity/vax-states.csv")
infD <- read_csv("data/analyzed/immunity/inf-states.csv")
boostD <- read_csv("data/analyzed/immunity/boost-states.csv")
popD <- read_csv("data/data-sources/statepop.csv")
pop12D <- read_csv("data/data-sources/statepop-age.csv")
oddsD <- read_csv("data/analyzed/immunity/logor-states.csv")

stt <- calcImmunity(vaxD, infD, boostD, oddsD, popD, pop12D, 
                 state = TRUE, maxDate = maxDate, 
                 waneScn = waneScn, 
                 scn.omi.df = scn.omi.df)
write_csv(stt, "data/results/results-state.csv")
