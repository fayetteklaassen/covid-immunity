library(tidyverse)
maxDate <- as.Date("2021-12-01")

source("code/immunity/functions.R")

### case-death-rr-vax-counties/states.csv is rendered on December 17, 2021
## using github.com/covidestim/covidestim-sources/immunity-waning
## make -B case-death-rr-vax.csv
## which uses JHU and NYT data for cases and deaths; 
## and Georgetown data for vaccinations.

### infections-counties.csv and infections-states.csv are rendered
## using the case-death-rr-vax.csv files; 
## using github.com/covidestim/dailyFlow and github.com/covidestim/covidestim

read_csv("data/raw/immunity/case-death-rr-vax-counties.csv") %>%
  select(date, fips, vaccinated) %>% 
  write_csv("data/analyzed/immunity/vax-counties.csv")

pivotDF(read_csv("data/raw/immunity/infections-counties.csv")%>%
                  select(date, fips, cum.incidence),
                state = FALSE) %>%
  write_csv("data/analyzed/immunity/inf-counties.csv")

read_csv("data/analyzed/immunity/cdc-boost-counties-imputed.csv") %>%
  transmute(date= date, fips = fips, boost = boost.imp) %>%
  write_csv("data/analyzed/immunity/boost-counties.csv")

pivotOdds(read_csv("data/analyzed/log-OR/logor-vac-fips.csv"), state = FALSE) %>%
  write_csv("data/analyzed/immunity/logor-counties.csv")


read_csv("data/raw/immunity/case-death-rr-vax-state.csv") %>%
  select(date, state, vaccinated) %>%
  filter(state != "Puerto Rico") %>%
  filter(date <= maxDate) %>%
  group_by(state) %>% 
  arrange(date) %>%
  mutate(vaccinated = if_else(state == "Utah",
                              noPeaks(vaccinated),
                              vaccinated)) %>%
  ungroup() %>%
  write_csv("data/analyzed/immunity/vax-states.csv")

pivotDF(read_csv("data/raw/immunity/infections-states.csv") %>%
                  select(date,state,cum.incidence, cum.incidence.lo, cum.incidence.hi),
                state = TRUE) %>%
  write_csv("data/analyzed/immunity/inf-states.csv")

read_csv("data/raw/immunity/cdc-boost-states.csv",
                   col_types = cols(
                     Date = col_date(format = "%m/%d/%Y"),
                     Location = col_character(),
                     .default = col_number()
                   ))   %>%
  transmute(state = Location,
            date = Date,
            boost = Additional_Doses_Vax_Pct) %>%
  mutate(boost = if_else(is.na(boost),
                         0,
                         boost/100),
         state = usdata::abbr2state(state)) %>%
  write_csv("data/analyzed/immunity/boost-states.csv")

pivotOdds(read_csv("data/analyzed/log-OR/logor-vac-state.csv"), state = TRUE) %>%
  write_csv("data/analyzed/immunity/logor-states.csv")
