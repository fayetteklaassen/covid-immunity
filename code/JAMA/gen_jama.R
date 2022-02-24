#### Jama immune data
library(readxl)
library(tidyverse)

### JAMA data is downloded from supplementary materials of 
# 
# Jones JM, Stone M, Sulaeman H, et al. 
# Estimated US Infection- and Vaccine-Induced SARS-CoV-2 Seroprevalence 
# Based on Blood Donations, July 2020-May 2021. JAMA. 2021.  
# https://doi.org/10.1001/jama.2021.15161.

jama <- read_excel("data/raw/JAMA/jama-immune.xlsx", skip = 2)

jdat <- jama[,2]
names(jdat) <- "state"
jdat$date <- as.numeric(as.matrix(jama[,4]))
jdat$sero <- as.numeric(as.matrix(jama[,6]))
jdat$sero.lo <- as.numeric(as.matrix(jama[,7]))
jdat$sero.hi <- as.numeric(as.matrix(jama[,8]))
jdat$Date <- as.character(jdat$date)
jdat$year <- as.numeric(substr(jdat$Date,1,4))
jdat$month <- as.numeric(substr(jdat$Date,5,6))
jdat$dat <- as.numeric(substr(jdat$Date, 7,8))
jdat$datum <- as.Date(with(jdat, paste(year, month, dat, sep ="-")), "%Y-%m-%d")
jdat$cat <- as.numeric(substr(jdat$state, 4,4))
jdat$cat[is.na(jdat$cat)] <- 1
jdat$stateabb <- substr(jdat$state,1,2)
jdat$statename <- usdata::abbr2state(jdat$stateabb)
jdat$statename[jdat$state == "All"] <- "All"

jdat %>% select(-c(state, date)) %>% transmute(state = statename,
                                            date = datum,
                                            cat = cat,
                                            sero.hi = sero.hi,
                                            sero = sero,
                                            sero.lo = sero.lo) -> jdat

saveRDS(jdat, "data/analyzed/JAMA/jama-seroprevalence.RDS")

jama_inf <- read_excel("data/raw/JAMA/jama-infections.xlsx", skip = 2)

idat <- jama_inf[,2]
names(idat) <- "state"
idat$date <- as.numeric(as.matrix(jama_inf[,4]))
idat$sero <- as.numeric(as.matrix(jama_inf[,6]))
idat$sero.lo <- as.numeric(as.matrix(jama_inf[,7]))
idat$sero.hi <- as.numeric(as.matrix(jama_inf[,8]))
idat$Date <- as.character(idat$date)
idat$year <- as.numeric(substr(idat$Date,1,4))
idat$month <- as.numeric(substr(idat$Date,5,6))
idat$dat <- as.numeric(substr(idat$Date, 7,8))
idat$datum <- as.Date(with(idat, paste(year, month, dat, sep ="-")), "%Y-%m-%d")
idat$cat <- as.numeric(substr(idat$state, 4,4))
idat$cat[is.na(idat$cat)] <- 1
idat$stateabb <- substr(idat$state,1,2)
idat$statename <- usdata::abbr2state(idat$stateabb)
idat$statename[idat$state == "All"] <- "All"

idat %>% select(-c(state, date)) %>% transmute(state = statename,
                                            date = datum,
                                            cat = cat,
                                            sero.hi = sero.hi,
                                            sero = sero,
                                            sero.lo = sero.lo) -> idat

saveRDS(idat, "data/analyzed/JAMA/jama-infections.RDS")

ggplot(jdat %>% filter(!is.na(state))) + 
  geom_line(aes(x = date, y = sero, group = cat)) +
  facet_wrap(~state, ncol = 5) +
  geom_line(data = idat, aes(x= date, y = sero, group = cat), col = "red")

